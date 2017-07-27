(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2017 OCamlPro SAS                                    *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result

let pass_name = "continuation-specialisation"
let () = Pass_wrapper.register ~pass_name

type specialising_result =
  | Didn't_specialise
  | Specialised of Continuation.t * Flambda.let_cont_handlers

let environment_for_simplification ~env ~old_handlers =
  let freshening =
    Continuation.Map.fold (fun cont _handler freshening ->
        let _new_cont, freshening =
          Freshening.add_static_exception freshening cont
        in
        freshening)
      old_handlers
      (Freshening.activate (E.freshening env))
  in
  let env =
    E.disallow_continuation_specialisation (
      E.disallow_continuation_inlining (
        E.set_never_inline (E.set_freshening env freshening)))
  in
  let env =
    Continuation.Map.fold (fun cont (handler : Flambda.continuation_handler)
              env ->
        let cont = Freshening.apply_static_exception (E.freshening env) cont in
        let approx =
          Continuation_approx.create_unknown ~name:cont
            ~num_params:(List.length handler.params)
        in
        E.add_continuation env cont approx)
      old_handlers
      env
  in
  freshening, env

let handlers_for_simplification ~old_handlers ~newly_specialised_args
      ~entry_point_cont ~freshening ~invariant_params_flow =
  Continuation.Map.fold (fun cont (old_handler : Flambda.continuation_handler)
          new_handlers ->
      let wrong_spec_args =
        Variable.Set.inter (Variable.Map.keys old_handler.specialised_args)
          (Variable.Map.keys newly_specialised_args)
      in
      if not (Variable.Set.is_empty wrong_spec_args) then begin
          Misc.fatal_errorf "These parameters of continuation %a have already \
            been specialised: %a"
          Continuation.print cont
          Variable.Set.print wrong_spec_args
      end;
      (* For a recursive group of continuations, [newly_specialised_args]
          may contain parameters across all of them, so we need to restrict it
          to just those for [cont]. *)
      let newly_specialised_args =
        let params = Parameter.Set.of_list old_handler.params in
        Variable.Map.filter (fun param _spec_to ->
            Parameter.Set.mem (Parameter.wrap param) params)
          newly_specialised_args
      in
      (* Compute the newly-specialised args.  These are either:
          (a) those arising from the "entry point" (i.e. [Apply_cont] of
              [entry_point_cont]); or
          (b) those arising from invariant parameters flow from the entry
              point's continuation handler. *)
      let newly_specialised_args =
        if Continuation.equal cont entry_point_cont then
          newly_specialised_args
        else
          Variable.Map.fold (fun param (spec_to : Flambda.specialised_to)
                  newly_specialised_args ->
              match Variable.Map.find param invariant_params_flow with
              | exception Not_found -> newly_specialised_args
              | flows_to ->
                let module CV =
                  Invariant_params.Continuations.Continuation_and_variable
                in
                CV.Set.fold (fun (cont', param')
                        newly_specialised_args ->
                    if not (Continuation.equal cont cont') then
                      newly_specialised_args
                    else
                      Variable.Map.add param' spec_to newly_specialised_args)
                  flows_to
                  newly_specialised_args)
            newly_specialised_args
            Variable.Map.empty
      in
      let specialised_args =
        Variable.Map.disjoint_union old_handler.specialised_args
          newly_specialised_args
      in
      let new_handler =
        { old_handler with
          specialised_args;
        }
      in
      let cont = Freshening.apply_static_exception freshening cont in
      Continuation.Map.add cont new_handler new_handlers)
    old_handlers
    Continuation.Map.empty

let usage_information_for_simplification ~env ~old_handlers ~new_handlers
      ~definitions_with_uses ~freshening =
  (* Add usage information for the parameters of the continuations.  The
     parameters are partitioned into two:
     1. the specialised arguments (either pre-existing or
        newly-specialised) with corresponding "specialised-to" variables
        (as opposed to specialised arguments just holding projection
        information), whose approximations are given by those in the
        environment of the variables being specialised to.  When
        newly-specialised arguments are added this should produce an
        increase in precision of the approximations over and above that
        used when the handler was previously simplified;
     2. other arguments, whose approximations will be the same as they were
        when the handler was previously simplified.  (Such approximations
        are the join of the approximations at all use points.)
     Apart from this information, the [r] used for simplification is
     empty. *)
  Continuation.Map.fold (fun cont
          (handler : Flambda.continuation_handler) r ->
      let join_approxs =
        match Continuation.Map.find cont definitions_with_uses with
        | exception Not_found ->
          Misc.fatal_errorf "Continuation %a does not occur in the \
              definitions-with-uses set (only maps %a)"
            Continuation.print cont
            Continuation.Set.print
            (Continuation.Map.keys definitions_with_uses)
        | (uses, _approx, _env, _recursive) ->
          Inline_and_simplify_aux.Continuation_uses.meet_of_args_approxs
            uses ~num_params:(List.length handler.params)
      in
      let freshened_cont =
        Freshening.apply_static_exception freshening cont
      in
      let specialised_args =
        match Continuation.Map.find freshened_cont new_handlers with
        | exception Not_found -> assert false  (* see above *)
        | (handler : Flambda.continuation_handler) -> handler.specialised_args
      in
      let args_approxs =
        List.map2 (fun param join_approx ->
            let param = Parameter.var param in
            match Variable.Map.find param specialised_args with
            | exception Not_found -> join_approx
            | spec_to ->
              match spec_to.var with
              | None -> join_approx
              | Some spec_to ->
                begin match E.find_opt env spec_to with
                | None ->
                  Misc.fatal_errorf "Variable %a of %a specialised to %a \
                      but %a is not in the environment:@ \n%a"
                    Variable.print param
                    Continuation.print cont
                    Variable.print spec_to
                    Variable.print spec_to
                    E.print env
                | Some approx -> approx
                end)
          handler.params
          join_approxs
      in
      R.use_continuation r env freshened_cont
        (Not_inlinable_or_specialisable args_approxs))
    old_handlers
    (R.create ())

(* This function constructs a suitable environment for simplification of a
   continuation's body that is eligible for specialisation.  The freshening
   of the body is performed during the simplification.

   If the continuation is defined simultaneously with others, all of the
   bodies will be simplified; specialised argument information may also be
   introduced for continuations in the same group using the invariant
   parameters flow information.  This saves multiple rounds of simplification
   being required to propagate around mutually-recursive continuations.
*)
let try_specialising ~cont ~(old_handlers : Flambda.continuation_handlers)
      ~(newly_specialised_args : Flambda.specialised_args)
      ~invariant_params_flow ~env ~(recursive : Asttypes.rec_flag)
      ~simplify_let_cont_handlers ~definitions_with_uses
      : specialising_result =
  let freshening, env = environment_for_simplification ~env ~old_handlers in
  let entry_point_cont = cont in
  let entry_point_new_cont =
    Freshening.apply_static_exception freshening cont
  in
  let new_handlers =
    handlers_for_simplification ~old_handlers ~newly_specialised_args
      ~entry_point_cont ~freshening ~invariant_params_flow
  in
  let r =
    usage_information_for_simplification ~env ~old_handlers ~new_handlers
      ~definitions_with_uses ~freshening
  in
  if !Clflags.flambda_invariant_checks then begin
    (* Unbound continuations will be caught by [simplify_let_cont_handlers]
       but it's nicer for debugging to check now. *)
    let handlers : Flambda.let_cont_handlers =
      match recursive with
      | Nonrecursive -> Nonrecursive new_handlers
      | Recursive -> Recursive new_handlers
    in
    let free_conts =
      Flambda.free_continuations_of_let_cont_handlers ~handlers
    in
    let unbound_conts =
      Continuation.Set.filter (fun cont -> not (E.mem_continuation env cont))
        free_conts
    in
    if not (Continuation.Set.is_empty unbound_conts) then begin
      Misc.fatal_errorf "Candidate for specialisation has free \
          continuations %a that are not bound in the environment:@ \n%a@ \n\
          The candidate for specialisation (originally named %a) was:@ \n%a"
        Continuation.Set.print unbound_conts
        E.print env
        Continuation.Set.print (Continuation.Map.keys old_handlers)
        Flambda.print_let_cont_handlers handlers
    end
  end;
  let new_handlers, r =
    simplify_let_cont_handlers ~env ~r ~handlers:new_handlers
      ~args_approxs:None ~recursive ~freshening:(E.freshening env)
  in
  match new_handlers with
  | None -> Didn't_specialise
  | Some new_handlers ->
    let module W = Inlining_cost.Whether_sufficient_benefit in
    let wsb =
      let _originals =
        List.map (fun (handler : Flambda.continuation_handler) ->
            handler.handler)
          (Continuation.Map.data old_handlers)
      in
      let new_handlers =
        match (new_handlers : Flambda.let_cont_handlers) with
        | Nonrecursive handlers | Recursive handlers ->
          List.map (fun (handler : Flambda.continuation_handler) ->
              handler.handler)
            (Continuation.Map.data handlers)
        | Alias _ ->
          Misc.fatal_error "simplify_let_cont_handlers should not return Alias"
      in
      (* CR-someday mshinwell: Probably some stuff about jump benefits should
         be added... *)
      W.create_list ~originals:[]
        ~toplevel:(E.at_toplevel env)
        ~branch_depth:(E.branch_depth env)
        new_handlers
        ~benefit:(R.benefit r)
        ~lifting:false
        ~round:(E.round env)
    in
    if W.evaluate wsb then Specialised (entry_point_new_cont, new_handlers)
    else Didn't_specialise

let handlers_and_invariant_params ~cont ~approx ~backend =
  match Continuation_approx.handlers approx with
  | None -> None
  | Some handlers ->
    match handlers with
    | Nonrecursive handlers ->
      let handler = match Continuation.Map.data handlers with
        | [handler] -> handler
        | _ -> (* CR vlaviron: handle multiple handlers *)
          Misc.fatal_errorf "Multiple handlers in nonrecursive approx"
      in
      if handler.is_exn_handler then None else begin
      (* Non-recursive continuation: all parameters are invariant. *)
      let invariant_params =
        List.fold_left (fun invariant_params param ->
            let param = Parameter.var param in
            let param_set = Variable.Set.singleton param in
            Variable.Map.add param param_set invariant_params)
          Variable.Map.empty
          handler.params
      in
      let invariant_params_flow = Variable.Map.empty in
      Some (handlers, invariant_params, invariant_params_flow)
      end
    | Recursive handlers ->
      let invariant_params =
        Invariant_params.Continuations.invariant_params_in_recursion
          handlers ~backend
      in
      let invariant_params_flow =
        Invariant_params.Continuations.invariant_param_sources
          handlers ~backend
      in
      Some (handlers, invariant_params, invariant_params_flow)

let can_specialise_param ~(handler : Flambda.continuation_handler) ~param
      ~arg_approx ~invariant_params =
  (not handler.stub)
    && Variable.Map.mem param invariant_params
    && (not (Variable.Map.mem param handler.specialised_args))
    && A.useful arg_approx

let examine_use ~specialisations ~cont
      ~(handler : Flambda.continuation_handler) ~invariant_params
      ~invariant_params_flow ~handlers ~recursive
      ~(use : Inline_and_simplify_aux.Continuation_uses.Use.t) =
  let module CA = Continuation.With_args in
  let module CSA = Continuation_with_specialised_args in
  let module U = Inline_and_simplify_aux.Continuation_uses in
  match U.Use.Kind.is_specialisable use.kind with
  | None -> specialisations
  | Some args_and_approxs ->
    assert (List.length handler.params = List.length args_and_approxs);
    let params_with_args =
      Variable.Map.of_list (
        let params = Parameter.List.vars handler.params in
        List.combine params args_and_approxs)
    in
    let params_with_specialised_args =
      Variable.Map.filter_map params_with_args
        ~f:(fun param (arg, arg_approx) ->
          if can_specialise_param ~handler ~param ~arg_approx ~invariant_params
          then
            let spec_to : Flambda.specialised_to =
              { var = Some arg;
                projection = None;
              }
            in
            Some spec_to
          else None)
    in
    if Variable.Map.cardinal params_with_specialised_args < 1 then
      specialisations
    else
      let key : CSA.t = cont, params_with_specialised_args in
      let uses, env =
        match CSA.Map.find key specialisations with
        | exception Not_found -> CA.Set.empty, use.env
        | uses, env, _, _, _ -> uses, env
      in
      (* We arbitrarily pick the first of the use environments as the
         environment in which we will try specialising the continuation.  This
         is correct because the variables to which we are specialising
         parameters must all occur (and thus have equal approximations) in the
         environment at every one of the use points we are specialising.
         The following [iter] checks this. *)
      if !Clflags.flambda_invariant_checks then begin
        Variable.Map.iter (fun param
                (spec_to : Flambda.specialised_to) ->
            match spec_to.var with
            | None -> assert false  (* see above *)
            | Some spec_to ->
              if not (E.mem use.env spec_to) then begin
                Misc.fatal_errorf "Newly-specialised parameter %a \
                    of %a (being specialised to %a) is not in this \
                    use environment:@ \n%a"
                  Variable.print param
                  Continuation.print cont
                  Variable.print spec_to
                  E.print use.env
              end)
          params_with_specialised_args
      end;
      let use_args = List.map (fun (arg, _approx) -> arg) args_and_approxs in
      let uses = CA.Set.add (cont, use_args) uses in
      CSA.Map.add key (uses, env, invariant_params_flow, handlers, recursive)
        specialisations

let find_candidate_specialisations r ~backend =
  let module N = Num_continuation_uses in
  let module U = Inline_and_simplify_aux.Continuation_uses in
  let definitions_with_uses = R.continuation_definitions_with_uses r in
  (* The result of this fold is a map that groups together uses of a
     continuation where some subset of its invariant parameters have the same
     arguments across those uses.  The range of the map, amongst other things,
     enables identification of the corresponding [Apply_cont] nodes which will
     need to be repointed if the continuation is specialised for those uses. *)
  Continuation.Map.fold (fun cont (uses, approx, _env, recursive)
        specialisations ->
      match handlers_and_invariant_params ~cont ~approx ~backend with
      | None -> specialisations
      | Some (handlers, invariant_params, invariant_params_flow) ->
        (* We don't currently share specialised continuations in the same
           set of recursive handlers between entry points to that set.
           So for example if there is a [Let_cont] binding k1 and k2
           which are recursive and we see [Apply_cont]s that can be
           specialised---one for k1 and one for k2---then they will end up
           each producing a specialised copy of the entire {k1, k2} set. *)
        let handler =
          match Continuation.Map.find cont handlers with
          | handler -> handler
          | exception Not_found ->
            Misc.fatal_errorf "Continuation %a not found in handler map \
                inside the approximation"
              Continuation.print cont
        in
        let application_points = U.application_points uses in
        let num_application_points = List.length application_points in
        match (recursive : Asttypes.rec_flag), num_application_points with
        | Nonrecursive, n when n <= 1 ->
          (* Non-recursive continuations that only have a single (inlinable)
             use point will be inlined out by [Continuation_inlining].
             (There would be no point in specialising such a continuation
             here in any case, since it's already been simplified under the
             most precise approximations available for its parameters.) *)
          specialisations
        | (Nonrecursive | Recursive), _ ->
          List.fold_left (fun specialisations use ->
              examine_use ~specialisations ~cont ~handler ~invariant_params
              ~invariant_params_flow ~handlers ~recursive ~use)
            specialisations
            application_points)
    definitions_with_uses
    Continuation_with_specialised_args.Map.empty

let beneficial_specialisations r ~specialisations ~simplify_let_cont_handlers =
  let module CA = Continuation.With_args in
  let module CSA = Continuation_with_specialised_args in
  let definitions_with_uses = R.continuation_definitions_with_uses r in
  CSA.Map.fold (fun
          (cont, newly_specialised_args)
          (cont_application_points, env, invariant_params_flow, old_handlers,
            (recursive : Asttypes.rec_flag))
          ((new_conts, apply_cont_rewrites) as acc) ->
      (* CR mshinwell: We should stop this trying to specialise
         already-specialised arguments.  (It isn't clear whether there is
         such a check for functions.)  This might be easy to do in
         [try_specialising] given a suitable ordering on approximations. *)
      match
        try_specialising ~cont ~old_handlers
          ~newly_specialised_args ~invariant_params_flow ~env ~recursive
          ~simplify_let_cont_handlers ~definitions_with_uses
      with
      | Didn't_specialise -> acc
      | Specialised (entry_point_new_cont, new_handlers) ->
        let existing_conts =
          match Continuation.Map.find cont new_conts with
          | exception Not_found -> []
          | existing_conts -> existing_conts
        in
        let new_conts =
          Continuation.Map.add cont (new_handlers :: existing_conts) new_conts
        in
        let apply_cont_rewrites =
          CA.Set.fold (fun ((cont', _args) as key)
                  apply_cont_rewrites ->
              assert (Continuation.equal cont cont');
              assert (not (CA.Map.mem key apply_cont_rewrites));
              CA.Map.add key entry_point_new_cont apply_cont_rewrites)
            cont_application_points
            apply_cont_rewrites
        in
        new_conts, apply_cont_rewrites)
    specialisations
    (Continuation.Map.empty, CA.Map.empty)

let insert_specialisations (expr : Flambda.expr) ~vars_in_scope ~new_conts
        ~apply_cont_rewrites =
  let module Placement = Place_continuations.Placement in
  let placed =
    Place_continuations.find_insertion_points expr ~vars_in_scope ~new_conts
  in
  let place ~placement ~around =
    match Placement.Map.find placement placed with
    | exception Not_found -> None
    | handlers_list ->
      let expr =
        List.fold_left (fun body handlers : Flambda.expr ->
            Let_cont { body; handlers; })
          around
          handlers_list
      in
      Some expr
  in
  Flambda_iterators.map_toplevel_expr (fun (expr : Flambda.t) : Flambda.t ->
      match expr with
      | Let { var; defining_expr; body } ->
        let placement : Placement.t = After_let var in
        begin match place ~placement ~around:body with
        | None -> expr
        | Some body -> Flambda.create_let var defining_expr body
        end
      | Let_cont { body; handlers = Nonrecursive { name; handler; }; } ->
        let done_something = ref false in
        let placement : Placement.t = Just_inside_continuation name in
        let handler_body =
          match place ~placement ~around:handler.handler with
          | None -> handler.handler
          | Some handler_body ->
            done_something := true;
            handler_body
        in
        let body =
          let placement : Placement.t =
            After_let_cont (Continuation.Set.singleton name)
          in
          match place ~placement ~around:body with
          | None -> body
          | Some body ->
            done_something := true;
            body
        in
        if not !done_something then expr
        else
          Let_cont {
            body;
            handlers = Nonrecursive { name; handler = {
              handler with handler = handler_body; };
            };
          }
      | Let_cont { body; handlers = Recursive handlers; } ->
        let done_something = ref false in
        let handlers =
          Continuation.Map.mapi (fun name
                  (handler : Flambda.continuation_handler) ->
              let placement : Placement.t = Just_inside_continuation name in
              begin match place ~placement ~around:handler.handler with
              | None -> handler
              | Some handler_body ->
                done_something := true;
                { handler with handler = handler_body; }
              end)
            handlers
        in
        let body =
          let placement : Placement.t =
            After_let_cont (Continuation.Map.keys handlers)
          in
          match place ~placement ~around:body with
          | None -> body
          | Some body ->
            done_something := true;
            body
        in
        if not !done_something then expr
        else Let_cont { body; handlers = Recursive handlers; }
      | Apply_cont (cont, trap_action, args) ->
        let key = cont, args in
        begin match
          Continuation.With_args.Map.find key apply_cont_rewrites
        with
        | exception Not_found -> expr
        | new_cont -> Apply_cont (new_cont, trap_action, args)
        end
      | Let_cont { handlers = Alias _; _ }
      | Apply _ | Let_mutable _ | Switch _ | Proved_unreachable -> expr)
    expr

let for_toplevel_expression expr ~vars_in_scope r ~simplify_let_cont_handlers
      ~backend =
  Pass_wrapper.with_dump ~pass_name
    ~input:expr
    ~print_input:Flambda.print
    ~print_output:Flambda.print
    ~f:(fun () ->
      let new_conts, apply_cont_rewrites =
        let specialisations = find_candidate_specialisations r ~backend in
        beneficial_specialisations r ~specialisations
          ~simplify_let_cont_handlers
      in
      if Continuation.Map.is_empty new_conts then None
      else
        Some (insert_specialisations expr ~vars_in_scope ~new_conts
          ~apply_cont_rewrites))

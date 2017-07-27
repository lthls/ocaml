(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* CR-someday mshinwell: If we can get the types in [Flambda] to be uniform
   enough between functions and continuations then we can probably share
   code for doing remove-unused-parameters. *)

let remove_parameters ~(handler : Flambda.continuation_handler)
        ~to_remove : Flambda_utils.with_wrapper =
  let freshened_params =
    List.map (fun param -> param, Parameter.rename param) handler.params
  in
  let wrapper_params =
    List.map (fun (_param, freshened_param) -> freshened_param)
      freshened_params
  in
  let wrapper_params_not_unused =
    Misc.Stdlib.List.filter_map (fun (param, freshened_param) ->
        if Parameter.Set.mem param to_remove then None
        else Some (Parameter.var freshened_param))
      freshened_params
  in
  let new_params =
    List.filter (fun param -> not (Parameter.Set.mem param to_remove))
      handler.params
  in
  let freshening = Parameter.Map.of_list freshened_params in
  let freshen param =
    match Parameter.Map.find param freshening with
    | param -> param
    | exception Not_found -> assert false
  in
  let new_specialised_args =
    Flambda_utils.clean_specialised_args_projections (
      Variable.Map.filter (fun param _spec_to ->
          not (Parameter.Set.mem (Parameter.wrap param) to_remove))
        handler.specialised_args)
  in
  let wrapper_specialised_args =
    Variable.Map.fold (fun var (spec_to : Flambda.specialised_to)
            wrapper_specialised_args ->
        let var = Parameter.var (freshen (Parameter.wrap var)) in
        let projection =
          Misc.Stdlib.Option.map (fun projection ->
              Projection.map_projecting_from projection ~f:(fun from ->
                Parameter.var (freshen (Parameter.wrap from))))
            spec_to.projection
        in
        let spec_to : Flambda.specialised_to =
          { var = spec_to.var;
            projection;
          }
        in
        Variable.Map.add var spec_to wrapper_specialised_args)
      handler.specialised_args
      Variable.Map.empty
  in
  let new_cont = Continuation.create () in
  let wrapper_handler : Flambda.continuation_handler =
    { params = wrapper_params;
      stub = true;
      is_exn_handler = false;
      handler = Apply_cont (new_cont, None, wrapper_params_not_unused);
      specialised_args = wrapper_specialised_args;
    }
  in
  assert (not handler.is_exn_handler);
  let new_handler =
    Parameter.Set.fold (fun param body ->
        Flambda.create_let (Parameter.var param) (Const (Const_pointer 0)) body)
      to_remove
      handler.handler
  in
  let new_handler : Flambda.continuation_handler =
    { params = new_params;
      stub = handler.stub;
      is_exn_handler = false;
      handler = new_handler;
      specialised_args = new_specialised_args;
    }
  in
  With_wrapper {
    new_cont;
    new_handler;
    wrapper_handler;
  }

let for_continuation ~body ~unused ~(handlers : Flambda.continuation_handlers)
      ~original ~recursive : Flambda.expr =
  if Parameter.Set.is_empty unused then
    original
  else
    let handlers =
      Continuation.Map.fold (fun cont
              (handler : Flambda.continuation_handler) handlers ->
          let to_remove =
            Parameter.Set.inter unused (Parameter.Set.of_list handler.params)
          in
          let with_wrapper : Flambda_utils.with_wrapper =
            if handler.stub || Parameter.Set.is_empty to_remove then
              Unchanged { handler; }
            else
              remove_parameters ~handler ~to_remove
          in
          Continuation.Map.add cont with_wrapper handlers)
        handlers
        Continuation.Map.empty
    in
    Flambda_utils.build_let_cont_with_wrappers ~body ~recursive
      ~with_wrappers:handlers

let run program ~backend =
  (* CR mshinwell: This is very inefficient, given the deeply-nested
     continuation structures that are typical.  The continuation declarations
     should probably be added one by one as they are encountered inside
     [Invariant_params] itself. *)
  Flambda_iterators.map_exprs_at_toplevel_of_program program ~f:(fun expr ->
    Flambda_iterators.map_expr (fun (expr : Flambda.expr) ->
        match expr with
        | Let_cont { body; handlers = Nonrecursive handlers; } ->
          begin
            match Continuation.Map.bindings handlers with
            | [ name, handler ] ->
              if handler.is_exn_handler then expr
              else
                let unused =
                  let fvs = Flambda.free_variables handler.handler in
                  let params = Parameter.Set.of_list handler.params in
                  Parameter.Set.filter (fun param ->
                      not (Variable.Set.mem (Parameter.var param) fvs))
                    params
                in
                let handlers =
                  Continuation.Map.add name handler Continuation.Map.empty
                in
                for_continuation ~body ~handlers ~unused ~original:expr
                  ~recursive:Asttypes.Nonrecursive
            | _ -> (* CR vlaviron: handle general case *)
              Misc.fatal_errorf "Let_cont nonrecursive handlers are expected \
                  to have exactly one handler"
          end
        | Let_cont { body; handlers = Recursive handlers; } ->
          let unused =
            Invariant_params.Continuations.unused_arguments handlers ~backend
          in
          let unused = Parameter.Set.wrap unused in
          for_continuation ~body ~handlers ~unused ~original:expr
            ~recursive:Asttypes.Recursive
        | Let_cont { handlers = Alias _; _ }
        | Let _ | Let_mutable _ | Apply _ | Apply_cont _ | Switch _
        | Proved_unreachable -> expr)
      expr)


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

module A = Simple_value_approx

module How_to_unbox = struct
  type t = {
    being_unboxed_to_wrapper_params_being_unboxed : Variable.t Variable.Map.t;
    add_bindings_in_wrapper : Flambda.expr -> Flambda.expr;
    new_arguments_for_call_in_wrapper : Variable.t list;
    new_params : (Variable.t * Projection.t) list;
    build_boxed_value_from_new_params :
      (Variable.t * (Flambda.expr -> Flambda.expr)) list;
  }

  let create () =
    { being_unboxed_to_wrapper_params_being_unboxed = Variable.Map.empty;
      add_bindings_in_wrapper = (fun expr -> expr);
      new_arguments_for_call_in_wrapper = [];
      new_params = [];
      build_boxed_value_from_new_params = [];
    }

  let new_specialised_args t =
    List.fold_left (fun new_specialised_args (param, projection) ->
        let spec_to : Flambda.specialised_to =
          { var = None;
            projection = Some projection;
          }
        in
        Variable.Map.add param spec_to new_specialised_args)
      Variable.Map.empty
      t.new_params

  let merge t1 t2 =
    { build_boxed_value_from_new_params =
        t1.build_boxed_value_from_new_params
          @ t2.build_boxed_value_from_new_params;
      being_unboxed_to_wrapper_params_being_unboxed =
        Variable.Map.union (fun _ param1 param2 ->
            assert (Variable.equal param1 param2);
            Some param1)
          t1.being_unboxed_to_wrapper_params_being_unboxed
          t2.being_unboxed_to_wrapper_params_being_unboxed;
      add_bindings_in_wrapper = (fun expr ->
        t2.add_bindings_in_wrapper (
          t1.add_bindings_in_wrapper expr));
      new_arguments_for_call_in_wrapper =
        t1.new_arguments_for_call_in_wrapper
          @ t2.new_arguments_for_call_in_wrapper;
      new_params = t1.new_params @ t2.new_params;
    }

  let merge_variable_map t_map =
    Variable.Map.fold (fun _param t1 t2 -> merge t1 t2) t_map (create ())
end

let how_to_unbox_core ~constant_ctors ~blocks ~being_unboxed
      ~unbox_returns : How_to_unbox.t =
  let dbg = Debuginfo.none in
  let num_constant_ctors = Numbers.Int.Set.cardinal constant_ctors in
  assert (num_constant_ctors >= 0);
  (* CR mshinwell: We need to think about this more.
     Suppose we have code that deconstructs an "int option".  That code uses
     Pisint.  However we know that the thing is only ever going to be
     "Some x" and try to elide the "_is_int" parameter.  However that means
     we don't know that Pisint foo_option = false.  For the moment we don't
     elide the "_is_int".  Note that for Unbox_continuation_params the
     extra argument isn't really a problem---it will be removed---but for
     Unbox_returns we really don't want to generate an extra return value
     if it isn't needed.
     Follow-up: think this might be ok for Unbox_returns only, since we don't
     need the Pisint = false judgements etc.
  *)
  let no_constant_ctors = 
    if unbox_returns then num_constant_ctors = 0
    else false
  in
  let num_tags = Tag.Map.cardinal blocks in
  assert (num_tags >= 1);  (* see below *)
  let wrapper_param_being_unboxed = Variable.rename being_unboxed in
  let being_unboxed_to_wrapper_params_being_unboxed =
    Variable.Map.add being_unboxed wrapper_param_being_unboxed
      Variable.Map.empty
  in
  let is_int = Variable.rename ~append:"_is_int" being_unboxed in
  let is_int_in_wrapper = Variable.rename is_int in
  let is_int_known_value =
    if no_constant_ctors then Some ((Const (Int 0)) : Flambda.named)
    else None
  in
  (* CR-soon mshinwell: On [discriminant] add information that tells us
     about the individual unboxed field parameters _given that_ we are
     in some particular case of a match on [discriminant] (GADT-style). *)
  let discriminant = Variable.rename ~append:"_discr" being_unboxed in
  let discriminant_in_wrapper = Variable.rename discriminant in
  let discriminant_known_value =
    let discriminant_possible_values =
      let all_tags =
        Tag.Map.fold (fun tag _ all_tags ->
            Numbers.Int.Set.add (Tag.to_int tag) all_tags)
          blocks
          Numbers.Int.Set.empty
      in
      Numbers.Int.Set.union constant_ctors all_tags
    in
    match Numbers.Int.Set.elements discriminant_possible_values with
    | [] -> assert false  (* see the bottom of [how_to_unbox], below *)
    | [tag] -> Some ((Const (Int tag)) : Flambda.named)
    | _tags -> None
  in
  let needs_discriminant =
    match discriminant_known_value with
    | None -> true
    | Some _ -> false
  in
  let max_size =
    Tag.Map.fold (fun _tag fields max_size ->
        max (Array.length fields) max_size)
      blocks 0
  in
  let field_arguments_for_call_in_wrapper =
    Array.to_list (Array.init max_size (fun index ->
        Variable.create (Printf.sprintf "field%d" index)))
  in
  let is_int_in_wrapper' = Variable.rename is_int_in_wrapper in
  let discriminant_in_wrapper' = Variable.rename discriminant_in_wrapper in
  let new_arguments_for_call_in_wrapper =
    let is_int =
      if no_constant_ctors then [] else [is_int_in_wrapper']
    in
    let discriminant =
      if not needs_discriminant then [] else [discriminant_in_wrapper']
    in
    is_int @ discriminant @ field_arguments_for_call_in_wrapper
  in
  let tags_to_sizes = Tag.Map.map (fun fields -> Array.length fields) blocks in
  let all_tags = Tag.Map.keys blocks in
  let sizes_to_filler_conts =
    List.fold_left (fun sizes_to_filler_conts size ->
        Numbers.Int.Map.add size (Continuation.create ()) sizes_to_filler_conts)
      Numbers.Int.Map.empty
      (Tag.Map.data tags_to_sizes)
  in
  let tags_to_sizes_and_boxing_conts =
    Tag.Map.map (fun size -> size, Continuation.create ()) tags_to_sizes
  in
  let unit_value = Variable.create "unit" in
  let all_units = Array.to_list (Array.init max_size (fun _ -> unit_value)) in
  let add_bindings_in_wrapper expr =
    let is_int_cont = Continuation.create () in
    let is_block_cont = Continuation.create () in
    let join_cont = Continuation.create () in
    let tag = Variable.create "tag" in
    let is_int_switch =
      Flambda.create_switch ~scrutinee:is_int_in_wrapper
        ~all_possible_values:(Numbers.Int.Set.of_list [0; 1])
        ~arms:[0, is_block_cont]
        ~default:(Some is_int_cont)
    in
    let add_fill_fields_conts expr =
      Numbers.Int.Map.fold (fun size filler_cont expr : Flambda.expr ->
          let fields =
            Array.init max_size (fun index ->
              if index < size then
                let name = Printf.sprintf "_field%d" index in
                index, Some (Variable.rename ~append:name being_unboxed)
              else
                index, None)
          in
          let fields_for_apply =
            List.map (fun (_index, var_opt) ->
                match var_opt with
                | None -> unit_value
                | Some var -> var)
              (Array.to_list fields)
          in
          let filler : Flambda.expr =
            let filler : Flambda.expr =
              let is_int_in_wrapper =
                if no_constant_ctors then [] else [is_int_in_wrapper]
              in
              let tag =
                if not needs_discriminant then [] else [tag]
              in
              Apply_cont (join_cont, None,
                is_int_in_wrapper @ tag @ fields_for_apply)
            in
            Array.fold_right (fun (index, var_opt) filler ->
                match var_opt with
                | None -> filler
                | Some var ->
                    Flambda.create_let var
                      (Prim (Pfield index, [wrapper_param_being_unboxed], dbg))
                      filler)
              fields
              filler
          in
          let handler : Flambda.continuation_handler = {
            params = [];
            (* CR mshinwell: All of the "stub" settings in this file are
               "true" so we don't try to unbox their arguments over and
               over.  Maybe instead we should have a "kind" field which
               could include the stub, is_exn_handler, etc data plus
               something saying not to unbox *)
            stub = true;
            is_exn_handler = false;
            handler = filler;
            specialised_args = Variable.Map.empty;
          }
          in
          Let_cont {
            body = expr;
            handlers = Nonrecursive (
                Continuation.Map.singleton filler_cont handler);
          })
        sizes_to_filler_conts
        expr      
    in
    let fill_fields_switch =
      let all_possible_values =
        Tag.Set.fold (fun tag all_possible_values ->
            Numbers.Int.Set.add (Tag.to_int tag) all_possible_values)
          all_tags
          Numbers.Int.Set.empty
      in
      let arms =
        Numbers.Int.Map.fold (fun size filler_cont consts ->
            Tag.Map.fold (fun tag fields consts ->
                if Array.length fields = size then
                  (Tag.to_int tag, filler_cont) :: consts
                else
                  consts)
              blocks
              consts)
          sizes_to_filler_conts
          []
      in
      Flambda.create_switch ~scrutinee:tag
        ~all_possible_values
        ~arms
        ~default:None
    in
    Flambda.create_let unit_value (Const (Int 0))
      (Flambda.create_let is_int_in_wrapper
        (if no_constant_ctors then
           Const (Int 0)
         else
           Prim (Pisint, [wrapper_param_being_unboxed], dbg))
        (Let_cont {
          body = Let_cont {
            body = Let_cont {
              body = is_int_switch;
              handlers = Nonrecursive (Continuation.Map.singleton
                is_int_cont
                {
                  Flambda.params = [];
                  handler =
                    (let is_int_in_wrapper =
                      if no_constant_ctors then [] else [is_int_in_wrapper]
                    in
                    let wrapper_param_being_unboxed =
                      if not needs_discriminant then []
                      else [wrapper_param_being_unboxed]
                    in
                    Apply_cont (join_cont, None,
                      is_int_in_wrapper @ wrapper_param_being_unboxed
                        @ all_units));
                  stub = true;
                  is_exn_handler = false;
                  specialised_args = Variable.Map.empty;
                }
              );
            };
            handlers = Nonrecursive (Continuation.Map.singleton
              is_block_cont
              {
                Flambda.params = [];
                handler =
                  Flambda.create_let tag
                    (match discriminant_known_value with
                     | Some known -> known
                     | None ->
                       Prim (Pgettag, [wrapper_param_being_unboxed], dbg))
                    (add_fill_fields_conts fill_fields_switch);
                stub = true;
                is_exn_handler = false;
                specialised_args = Variable.Map.empty;
              }
            );
          };
          handlers = Nonrecursive (Continuation.Map.singleton
            join_cont
            {
              Flambda.params = Parameter.List.wrap new_arguments_for_call_in_wrapper;
              handler = expr;
              stub = true;
              is_exn_handler = false;
              specialised_args = Variable.Map.empty;
            }
          );
        }))
  in
  let fields_with_projections =
    Array.to_list (Array.init max_size (fun index ->
      let append = string_of_int index in
      let var = Variable.rename ~append being_unboxed in
      let projection : Projection.t = Field (index, being_unboxed) in
      var, projection))
  in
  (* CR mshinwell: This next section is only needed for [Unbox_returns] at
     present; we shouldn't run it unless required. *)
  let boxing_is_int_cont = Continuation.create () in
  let boxing_is_block_cont = Continuation.create () in
  let boxing_is_int_switch =
    Flambda.create_switch ~scrutinee:is_int
      ~all_possible_values:(Numbers.Int.Set.of_list [0; 1])
      ~arms:[0, boxing_is_block_cont]
      ~default:(Some boxing_is_int_cont)
  in
  let boxing_switch =
    let all_possible_values =
      Tag.Set.fold (fun tag numconsts ->
          Numbers.Int.Set.add (Tag.to_int tag) numconsts)
        all_tags
        Numbers.Int.Set.empty
    in
    let arms =
      Tag.Map.fold (fun tag (_size, boxing_cont) consts ->
          (Tag.to_int tag, boxing_cont) :: consts)
        tags_to_sizes_and_boxing_conts
        []
    in
    Flambda.create_switch ~scrutinee:discriminant
      ~all_possible_values
      ~arms
      ~default:None
  in
  let build_boxed_value_from_new_params =
    let boxed = Variable.rename ~append:"_boxed" being_unboxed in
    let join_cont = Continuation.create () in
    let build (expr : Flambda.expr) : Flambda.expr =
      let consts =
        Numbers.Int.Set.fold (fun ctor_index consts ->
            let cont = Continuation.create () in
            (ctor_index, cont) :: consts)
          constant_ctors
          []
      in
      let constant_ctor_switch =
        Flambda.create_switch ~scrutinee:discriminant
          ~all_possible_values:constant_ctors
          ~arms:consts
          ~default:None
      in
      let add_constant_ctor_conts expr =
        List.fold_left (fun expr (ctor_index, cont) ->
            let ctor_index_var = Variable.create "ctor_index" in
            Flambda.create_let ctor_index_var (Const (Int ctor_index))
              (Let_cont {
                body = expr;
                handlers = Nonrecursive (Continuation.Map.singleton
                  cont
                  {
                    Flambda.handler = Apply_cont (join_cont, None, [ctor_index_var]);
                    params = [];
                    stub = true;
                    is_exn_handler = false;
                    specialised_args = Variable.Map.empty;
                  }
                );
              }))
          expr
          consts
      in
      let add_boxing_conts expr =
        Tag.Map.fold (fun tag (size, boxing_cont) expr : Flambda.expr ->
            let boxed = Variable.rename boxed in
            let fields =
              let fields, _index =
                List.fold_left (fun (fields, index) (field, _proj) ->
                    if index >= size then fields, index + 1
                    else (field :: fields), index + 1)
                  ([], 0)
                  fields_with_projections
              in
              List.rev fields
            in
            let handler : Flambda.expr =
              Flambda.create_let boxed
                (Prim (Pmakeblock (Tag.to_int tag, Immutable, None),
                  fields, dbg))
                (Flambda.Apply_cont (join_cont, None, [boxed]))
            in
            Let_cont {
              body = expr;
              handlers = Nonrecursive (Continuation.Map.singleton
                boxing_cont
                {
                  Flambda.params = [];
                  handler;
                  stub = true;
                  is_exn_handler = false;
                  specialised_args = Variable.Map.empty;
                }
              );
            })
          tags_to_sizes_and_boxing_conts
          expr
      in
      let body : Flambda.expr =
        Let_cont {
          body = Let_cont {
            body = Let_cont {
              body = boxing_is_int_switch;
              handlers = Nonrecursive (Continuation.Map.singleton
                boxing_is_block_cont
                {
                  Flambda.params = [];
                  handler = add_boxing_conts boxing_switch;
                  stub = true;
                  is_exn_handler = false;
                  specialised_args = Variable.Map.empty;
                }
              );
            };
            handlers = Nonrecursive (Continuation.Map.singleton
              boxing_is_int_cont
              {
                Flambda.params = [];
                (* We could just call [join_cont] with [discriminant] as the
                   argument, but that wouldn't pass on the knowledge to the
                   place in which this stub gets inlined that [discriminant]
                   is an integer. *)
                (* CR-someday mshinwell: Maybe adding some kind of support for
                   coercions would help here.  Perhaps another approach would be
                   to do CSE on "Pisint discriminant" (which would rewrite to
                   the "is_int" variable returned from the callee).  This would
                   require propagation of the projection information from the
                   stub function generated by Unbox_returns to the place it's
                   being inlined. *)
                handler = add_constant_ctor_conts constant_ctor_switch;
                stub = true;
                is_exn_handler = false;
                specialised_args = Variable.Map.empty;
              }
            );
          };
          handlers = Nonrecursive (Continuation.Map.singleton
            join_cont
            {
              Flambda.params = [Parameter.wrap boxed];
              handler = expr;
              stub = true;
              is_exn_handler = false;
              specialised_args = Variable.Map.empty;
            }
          );
        }
      in
      let body =
        match is_int_known_value with
        | None -> body
        | Some named -> Flambda.create_let is_int named body
      in
      match discriminant_known_value with
      | None -> body
      | Some named -> Flambda.create_let discriminant named body
    in
    [boxed, build]
  in
  let is_int =
    if no_constant_ctors then []
    else [is_int, Projection.Prim (Pisint, [being_unboxed])]
  in
  let discriminant =
    if not needs_discriminant then []
    else [discriminant, Projection.Prim (Pgettag, [being_unboxed])]
  in
  { being_unboxed_to_wrapper_params_being_unboxed;
    add_bindings_in_wrapper;
    new_arguments_for_call_in_wrapper;
    new_params = is_int @ discriminant @ fields_with_projections;
    build_boxed_value_from_new_params;
  }

let how_to_unbox ~being_unboxed ~being_unboxed_approx ~unbox_returns =
  match A.check_approx_for_variant being_unboxed_approx with
  | Wrong -> None
  | Ok approx ->
(*
Format.eprintf "how_to_unbox %a: %a\n%!"
  Variable.print being_unboxed
  A.print being_unboxed_approx;
*)
    let constant_ctors =
      match approx with
      | Blocks _ -> Numbers.Int.Set.empty
      | Blocks_and_immediates (_, imms) | Immediates imms ->
        let module I = A.Unionable.Immediate in
        I.Set.fold (fun (approx : I.t) ctor_indexes ->
            let ctor_index =
              match approx with
              | Int i -> i
              | Char c -> Char.code c
              | Constptr p -> p
            in
            Numbers.Int.Set.add ctor_index ctor_indexes)
          imms
          Numbers.Int.Set.empty
    in
    let blocks =
      match approx with
      | Blocks blocks | Blocks_and_immediates (blocks, _) -> blocks
      | Immediates _ -> Tag.Map.empty
    in
    (* CR mshinwell: This is sometimes returning "new_params" being empty;
       this should be an error presumably *)
    if Tag.Map.is_empty blocks then None
    else
      Some (how_to_unbox_core ~constant_ctors ~blocks ~being_unboxed
        ~unbox_returns)

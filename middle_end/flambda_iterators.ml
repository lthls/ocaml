(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let apply_on_subexpressions f f_named (flam : Flambda.t) =
  match flam with
  | Apply _ | Apply_cont _ | Switch _ | Proved_unreachable -> ()
  | Let { defining_expr; body; _ } ->
    f_named defining_expr;
    f body
  | Let_mutable { body; _ } ->
    f body
  | Let_cont { body; handlers = (Recursive handlers | Nonrecursive handlers); } ->
    f body;
    Continuation.Map.iter
      (fun _cont ({ handler; _ } : Flambda.continuation_handler) -> f handler)
      handlers
  | Let_cont { handlers = Alias _; _ } -> ()

let map_subexpressions f f_named (tree:Flambda.t) : Flambda.t =
  match tree with
  | Apply _ | Apply_cont _ | Switch _ | Proved_unreachable -> tree
  | Let { var; defining_expr; body; _ } ->
    let new_named = f_named var defining_expr in
    let new_body = f body in
    if new_named == defining_expr && new_body == body then
      tree
    else
      Flambda.create_let var new_named new_body
  | Let_mutable mutable_let ->
    let new_body = f mutable_let.body in
    if new_body == mutable_let.body then
      tree
    else
      Let_mutable { mutable_let with body = new_body }
  | Let_cont ({ body; handlers; } as let_cont) ->
    let new_body = f body in
    let map_handlers make_handlers handlers : Flambda.t =
      let something_changed = ref false in
      let candidate_handlers =
        Continuation.Map.map
          (fun (handler : Flambda.continuation_handler) ->
            let new_handler = f handler.handler in
            if not (new_handler == handler.handler) then begin
              something_changed := true
            end;
            { handler with handler = new_handler; })
          handlers
      in
      if !something_changed || not (new_body == body) then
        Let_cont {
          body = new_body;
          handlers = make_handlers candidate_handlers;
        }
      else
        tree
    in
    match handlers with
    | Alias _ ->
      if new_body == body then
        tree
      else
        Let_cont { let_cont with body = new_body; }
    | Nonrecursive handlers ->
      map_handlers (fun handlers -> Nonrecursive handlers) handlers
    | Recursive handlers ->
      map_handlers (fun handlers -> Recursive handlers) handlers

let iter_general = Flambda.iter_general

let iter f f_named t = iter_general ~toplevel:false f f_named (Is_expr t)
let iter_expr f t = iter f (fun _ -> ()) t
let iter_on_named f f_named t =
  iter_general ~toplevel:false f f_named (Is_named t)
let iter_named f_named t = iter (fun (_ : Flambda.t) -> ()) f_named t
let iter_named_on_named f_named named =
  iter_general ~toplevel:false (fun (_ : Flambda.t) -> ()) f_named
    (Is_named named)

let iter_toplevel f f_named t =
  iter_general ~toplevel:true f f_named (Is_expr t)
let iter_named_toplevel f f_named named =
  iter_general ~toplevel:true f f_named (Is_named named)

(* CR-soon mshinwell: Remove "let_rec" from these names *)
let iter_all_immutable_let_and_let_rec_bindings t ~f =
  iter_expr (function
      | Let { var; defining_expr; _ } -> f var defining_expr
      | _ -> ())
    t

let iter_all_toplevel_immutable_let_and_let_rec_bindings t ~f =
  iter_general ~toplevel:true
    (function
      | Let { var; defining_expr; _ } -> f var defining_expr
      | _ -> ())
    (fun _ -> ())
    (Is_expr t)

let iter_on_sets_of_closures f t =
  iter_named (function
      | Set_of_closures clos -> f clos
      | Var _ | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Assign _ | Read_symbol_field _ | Project_closure _
      | Move_within_set_of_closures _ | Project_var _ | Prim _ -> ())
    t

let iter_exprs_at_toplevel_of_program (program : Flambda.program) ~f =
  let rec loop (program : Flambda.program_body) =
    match program with
    | Let_symbol (_, Set_of_closures set_of_closures, program) ->
      Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
          f ~continuation_arity:function_decl.return_arity
            function_decl.continuation_param function_decl.body)
        set_of_closures.function_decls.funs;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (function
          | (_, Flambda.Set_of_closures set_of_closures) ->
            Variable.Map.iter
              (fun _ (function_decl : Flambda.function_declaration) ->
                f ~continuation_arity:function_decl.return_arity
                  function_decl.continuation_param function_decl.body)
              set_of_closures.function_decls.funs
          | _ -> ()) defs;
      loop program
    | Let_symbol (_, _, program) ->
      loop program
    | Initialize_symbol (_, _, fields, program) ->
      List.iter (fun (field, cont) -> f ~continuation_arity:1 cont field)
        fields;
      loop program
    | Effect (expr, cont, program) ->
      f ~continuation_arity:1 cont expr;
      loop program
    | End _ -> ()
  in
  loop program.program_body

(* CR mshinwell: think of a better name *)
let iter_exprs_at_toplevels_in_program (program : Flambda.program) ~f =
  iter_exprs_at_toplevel_of_program program
    ~f:(fun ~continuation_arity cont expr ->
      let rec iter_expr ~continuation_arity cont expr =
        iter_named (fun (named : Flambda.named) ->
            match named with
            | Set_of_closures set_of_closures ->
              Variable.Map.iter
                (fun _ (function_decl : Flambda.function_declaration) ->
                  iter_expr ~continuation_arity:function_decl.return_arity
                    function_decl.continuation_param
                    function_decl.body)
                set_of_closures.function_decls.funs
            | _ -> ())
          expr;
        f ~continuation_arity cont expr
      in
      iter_expr ~continuation_arity cont expr)

let iter_named_of_program program ~f =
  iter_exprs_at_toplevel_of_program program
    ~f:(fun ~continuation_arity:_ _ e -> iter_named f e)

let iter_on_set_of_closures_of_program (program : Flambda.program) ~f =
  let rec loop (program : Flambda.program_body) =
    match program with
    | Let_symbol (_, Set_of_closures set_of_closures, program) ->
      f ~constant:true set_of_closures;
      Variable.Map.iter (fun _ (function_decl : Flambda.function_declaration) ->
          iter_on_sets_of_closures (f ~constant:false) function_decl.body)
        set_of_closures.function_decls.funs;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (function
          | (_, Flambda.Set_of_closures set_of_closures) ->
            f ~constant:true set_of_closures;
            Variable.Map.iter
              (fun _ (function_decl : Flambda.function_declaration) ->
                iter_on_sets_of_closures (f ~constant:false) function_decl.body)
              set_of_closures.function_decls.funs
          | _ -> ()) defs;
      loop program
    | Let_symbol (_, _, program) ->
      loop program
    | Initialize_symbol (_, _, fields, program) ->
      List.iter (fun (field, _cont) ->
          iter_on_sets_of_closures (f ~constant:false) field)
        fields;
      loop program
    | Effect (expr, _cont, program) ->
      iter_on_sets_of_closures (f ~constant:false) expr;
      loop program
    | End _ -> ()
  in
  loop program.program_body

let iter_constant_defining_values_on_program (program : Flambda.program) ~f =
  let rec loop (program : Flambda.program_body) =
    match program with
    | Let_symbol (_, const, program) ->
      f const;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (fun (_, const) -> f const) defs;
      loop program
    | Initialize_symbol (_, _, _, program) ->
      loop program
    | Effect (_, _, program) ->
      loop program
    | End _ -> ()
  in
  loop program.program_body

let map_general ~toplevel f f_named tree =
  let rec aux (tree : Flambda.t) =
    match tree with
    | Let _ ->
      Flambda.map_lets tree ~for_defining_expr:aux_named ~for_last_body:aux
        ~after_rebuild:f
    | _ ->
      let exp : Flambda.t =
        match tree with
        | Apply _ | Apply_cont _ | Switch _ | Proved_unreachable -> tree
        | Let _ -> assert false
        | Let_mutable mutable_let ->
          let new_body = aux mutable_let.body in
          if new_body == mutable_let.body then
            tree
          else
            Let_mutable { mutable_let with body = new_body }
        (* CR-soon mshinwell: There's too much code duplication here with
           [map_subexpressions]. *)
        | Let_cont ({ body; handlers; } as let_cont) ->
          let new_body = aux body in
          let map_handlers make_handlers handlers : Flambda.t =
            let something_changed = ref false in
            let candidate_handlers =
              Continuation.Map.map
                (fun (handler : Flambda.continuation_handler) ->
                  let new_handler = aux handler.handler in
                  if not (new_handler == handler.handler) then begin
                    something_changed := true
                  end;
                  { handler with handler = new_handler; })
                handlers
            in
            if !something_changed || not (new_body == body) then
              Let_cont {
                body = new_body;
                handlers = make_handlers candidate_handlers;
              }
            else
              tree
          in
          match handlers with
          | Alias _ ->
            if new_body == body then
              tree
            else
              Let_cont { let_cont with body = new_body; }
          | Nonrecursive handlers ->
            map_handlers (fun handlers -> Nonrecursive handlers) handlers
          | Recursive handlers ->
            map_handlers (fun handlers -> Recursive handlers) handlers
      in
      f exp
  and aux_named (id : Variable.t) (named : Flambda.named) =
    let named : Flambda.named =
      match named with
      | Var _ | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Assign _ | Project_closure _ | Move_within_set_of_closures _
      | Project_var _ | Prim _ | Read_symbol_field _ -> named
      | Set_of_closures ({ function_decls; free_vars; specialised_args;
          direct_call_surrogates }) ->
        if toplevel then named
        else begin
          let done_something = ref false in
          let funs =
            Variable.Map.map (fun (func_decl : Flambda.function_declaration) ->
                let new_body = aux func_decl.body in
                if new_body == func_decl.body then begin
                  func_decl
                end else begin
                  done_something := true;
                  Flambda.update_body_of_function_declaration func_decl
                    ~body:new_body
                end)
              function_decls.funs
          in
          if not !done_something then
            named
          else
            let function_decls =
              Flambda.update_function_declarations function_decls ~funs
            in
            let set_of_closures =
              Flambda.create_set_of_closures ~function_decls ~free_vars
                ~specialised_args ~direct_call_surrogates
            in
            Set_of_closures set_of_closures
        end
    in
    f_named id named
  in
  aux tree

let iter_apply_on_program program ~f =
  iter_exprs_at_toplevel_of_program program
    ~f:(fun ~continuation_arity:_ _cont expr ->
      iter (function
          | Apply apply -> f apply
          | _ -> ())
        (fun _ -> ())
        expr)

let map f f_named tree =
  map_general ~toplevel:false f (fun _ n -> f_named n) tree
let map_expr f tree = map f (fun named -> named) tree
let map_named f_named tree = map (fun expr -> expr) f_named tree
let map_named_with_id f_named tree =
  map_general ~toplevel:false (fun expr -> expr) f_named tree
let map_toplevel f f_named tree =
  map_general ~toplevel:true f (fun _ n -> f_named n) tree
let map_toplevel_expr f_expr tree =
  map_toplevel f_expr (fun named -> named) tree
let map_toplevel_named f_named tree =
  map_toplevel (fun tree -> tree) f_named tree

let map_symbols tree ~f =
  map_named (function
      | (Symbol sym) as named ->
        let new_sym = f sym in
        if new_sym == sym then
          named
        else
          Symbol new_sym
      | ((Read_symbol_field (sym, field)) as named) ->
        let new_sym = f sym in
        if new_sym == sym then
          named
        else
          Read_symbol_field (new_sym, field)
      | (Var _ | Const _ | Allocated_const _ | Set_of_closures _
         | Read_mutable _ | Project_closure _ | Move_within_set_of_closures _
         | Project_var _ | Prim _ | Assign _) as named -> named)
    tree

let map_symbols_on_set_of_closures
    ({ Flambda.function_decls; free_vars; specialised_args;
        direct_call_surrogates; } as
      set_of_closures)
    ~f =
  let done_something = ref false in
  let funs =
    Variable.Map.map (fun (func_decl : Flambda.function_declaration) ->
        let body = map_symbols func_decl.body ~f in
        if not (body == func_decl.body) then begin
          done_something := true;
        end;
        Flambda.update_body_of_function_declaration func_decl ~body)
      function_decls.funs
  in
  if not !done_something then
    set_of_closures
  else
    let function_decls =
      Flambda.update_function_declarations function_decls ~funs
    in
    Flambda.create_set_of_closures ~function_decls ~free_vars
      ~specialised_args ~direct_call_surrogates

let map_toplevel_sets_of_closures tree ~f =
  map_toplevel_named (function
      | (Set_of_closures set_of_closures) as named ->
        let new_set_of_closures = f set_of_closures in
        if new_set_of_closures == set_of_closures then
          named
        else
          Set_of_closures new_set_of_closures
      | (Var _ | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
      | Read_symbol_field _
      | Project_closure _ | Move_within_set_of_closures _ | Project_var _
      | Prim _ | Assign _) as named -> named)
    tree

let map_apply tree ~f =
  map (function
      | (Apply apply) as expr ->
        let new_apply = f apply in
        if new_apply == apply then
          expr
        else
          Apply new_apply
      | expr -> expr)
    (fun named -> named)
    tree

let map_sets_of_closures tree ~f =
  map_named (function
      | (Set_of_closures set_of_closures) as named ->
        let new_set_of_closures = f set_of_closures in
        if new_set_of_closures == set_of_closures then
          named
        else
          Set_of_closures new_set_of_closures
      | (Var _ | Symbol _ | Const _ | Allocated_const _ | Project_closure _
      | Move_within_set_of_closures _ | Project_var _ | Assign _
      | Prim _ | Read_mutable _ | Read_symbol_field _) as named -> named)
    tree

let map_project_var_to_named_opt tree ~f =
  map_named (function
      | (Project_var project_var) as named ->
        begin match f project_var with
        | None -> named
        | Some named -> named
        end
      | (Var _ | Symbol _ | Const _ | Allocated_const _
      | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _
      | Prim _ | Read_mutable _ | Read_symbol_field _ | Assign _) as named ->
        named)
    tree

let map_function_bodies ?ignore_stubs
      (set_of_closures : Flambda.set_of_closures) ~f =
  let done_something = ref false in
  let funs =
    Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
        let new_body =
          match ignore_stubs, function_decl.stub with
          | Some (), true -> function_decl.body
          | _, _ -> f function_decl.body
        in
        if new_body == function_decl.body then
          function_decl
        else begin
          done_something := true;
          Flambda.update_body_of_function_declaration function_decl
            ~body:new_body
        end)
      set_of_closures.function_decls.funs
  in
  if not !done_something then
    set_of_closures
  else
    let function_decls =
      Flambda.update_function_declarations set_of_closures.function_decls ~funs
    in
    Flambda.create_set_of_closures
      ~function_decls
      ~free_vars:set_of_closures.free_vars
      ~specialised_args:set_of_closures.specialised_args
      ~direct_call_surrogates:set_of_closures.direct_call_surrogates

let map_sets_of_closures_of_program (program : Flambda.program)
    ~(f : Flambda.set_of_closures -> Flambda.set_of_closures) =
  let rec loop (program : Flambda.program_body) : Flambda.program_body =
    let map_constant_set_of_closures (set_of_closures:Flambda.set_of_closures) =
      let done_something = ref false in
      let function_decls =
        let funs =
          Variable.Map.map (fun
                  (function_decl : Flambda.function_declaration) ->
              let body = map_sets_of_closures ~f function_decl.body in
              if body == function_decl.body then
                function_decl
              else begin
                done_something := true;
                Flambda.update_body_of_function_declaration function_decl ~body
              end)
            set_of_closures.function_decls.funs
        in
        if not !done_something then
          set_of_closures.function_decls
        else
          Flambda.update_function_declarations set_of_closures.function_decls
            ~funs
      in
      let new_set_of_closures = f set_of_closures in
      if new_set_of_closures == set_of_closures then
        set_of_closures
      else
        Flambda.create_set_of_closures ~function_decls
          ~free_vars:set_of_closures.free_vars
          ~specialised_args:set_of_closures.specialised_args
          ~direct_call_surrogates:set_of_closures.direct_call_surrogates
    in
    match program with
    | Let_symbol (symbol, Set_of_closures set_of_closures, program') ->
      let new_set_of_closures = map_constant_set_of_closures set_of_closures in
      let new_program' = loop program' in
      if new_set_of_closures == set_of_closures
          && new_program' == program' then
        program
      else
        Let_symbol (symbol, Set_of_closures new_set_of_closures, new_program')
    | Let_symbol (symbol, const, program') ->
      let new_program' = loop program' in
      if new_program' == program' then
        program
      else
        Let_symbol (symbol, const, new_program')
    | Let_rec_symbol (defs, program') ->
      let done_something = ref false in
      let defs =
        List.map (function
            | (var, Flambda.Set_of_closures set_of_closures) ->
              let new_set_of_closures =
                map_constant_set_of_closures set_of_closures
              in
              if not (new_set_of_closures == set_of_closures) then begin
                done_something := true
              end;
              var, Flambda.Set_of_closures new_set_of_closures
            | def -> def)
          defs
      in
      let new_program' = loop program' in
      if new_program' == program' && not !done_something then
        program
      else
        Let_rec_symbol (defs, loop program')
    | Initialize_symbol (symbol, tag, fields, program') ->
      let done_something = ref false in
      let fields =
        List.map (fun (field, cont) ->
            let new_field = map_sets_of_closures field ~f in
            if not (new_field == field) then begin
              done_something := true
            end;
            new_field, cont)
          fields
      in
      let new_program' = loop program' in
      if new_program' == program' && not !done_something then
        program
      else
        Initialize_symbol (symbol, tag, fields, new_program')
    | Effect (expr, cont, program') ->
      let new_expr = map_sets_of_closures expr ~f in
      let new_program' = loop program' in
      if new_expr == expr && new_program' == program' then
        program
      else
        Effect (new_expr, cont, new_program')
    | End _ -> program
  in
  { program with
    program_body = loop program.program_body;
  }

let map_exprs_at_toplevel_of_program (program : Flambda.program)
    ~(f : Flambda.t -> Flambda.t) =
  let rec loop (program : Flambda.program_body) : Flambda.program_body =
    let map_constant_set_of_closures (set_of_closures:Flambda.set_of_closures) =
      let done_something = ref false in
      let funs =
        Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
            let body = f function_decl.body in
            if body == function_decl.body then
              function_decl
            else begin
              done_something := true;
              Flambda.update_body_of_function_declaration function_decl ~body
            end)
          set_of_closures.function_decls.funs
      in
      if not !done_something then
        set_of_closures
      else
        let function_decls =
          Flambda.update_function_declarations set_of_closures.function_decls
            ~funs
        in
        Flambda.create_set_of_closures ~function_decls
          ~free_vars:set_of_closures.free_vars
          ~specialised_args:set_of_closures.specialised_args
          ~direct_call_surrogates:set_of_closures.direct_call_surrogates
    in
    (* CR-soon mshinwell: code very similar to the above function *)
    match program with
    | Let_symbol (symbol, Set_of_closures set_of_closures, program') ->
      let new_set_of_closures = map_constant_set_of_closures set_of_closures in
      let new_program' = loop program' in
      if new_set_of_closures == set_of_closures
          && new_program' == program' then
        program
      else
        Let_symbol (symbol, Set_of_closures new_set_of_closures, new_program')
    | Let_symbol (symbol, const, program') ->
      let new_program' = loop program' in
      if new_program' == program' then
        program
      else
        Let_symbol (symbol, const, new_program')
    | Let_rec_symbol (defs, program') ->
      let done_something = ref false in
      let defs =
        List.map (function
            | (var, Flambda.Set_of_closures set_of_closures) ->
              let new_set_of_closures =
                map_constant_set_of_closures set_of_closures
              in
              if not (new_set_of_closures == set_of_closures) then begin
                done_something := true
              end;
              var, Flambda.Set_of_closures new_set_of_closures
            | def -> def)
          defs
      in
      let new_program' = loop program' in
      if new_program' == program' && not !done_something then
        program
      else
        Let_rec_symbol (defs, new_program')
    | Initialize_symbol (symbol, tag, fields, program') ->
      let done_something = ref false in
      let fields =
        List.map (fun (field, cont) ->
            let new_field = f field in
            if not (new_field == field) then begin
              done_something := true
            end;
            new_field, cont)
          fields
      in
      let new_program' = loop program' in
      if new_program' == program' && not !done_something then
        program
      else
        Initialize_symbol (symbol, tag, fields, new_program')
    | Effect (expr, cont, program') ->
      let new_expr = f expr in
      let new_program' = loop program' in
      if new_expr == expr && new_program' == program' then
        program
      else
        Effect (new_expr, cont, new_program')
    | End _ -> program
  in
  { program with
    program_body = loop program.program_body;
  }

let map_named_of_program (program : Flambda.program)
      ~(f : Variable.t -> Flambda.named -> Flambda.named) : Flambda.program =
  map_exprs_at_toplevel_of_program program
      ~f:(fun expr -> map_named_with_id f expr)

let map_all_immutable_let_and_let_rec_bindings (expr : Flambda.t)
      ~(f : Variable.t -> Flambda.named -> Flambda.named) : Flambda.t =
  map_named_with_id f expr

let fold_function_decls_ignoring_stubs
      (set_of_closures : Flambda.set_of_closures) ~init ~f =
  Variable.Map.fold (fun fun_var function_decl acc ->
      f ~fun_var ~function_decl acc)
    set_of_closures.function_decls.funs
    init

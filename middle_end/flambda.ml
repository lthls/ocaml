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

module Int = Numbers.Int

type call_kind =
  | Indirect
  | Direct of {
      closure_id : Closure_id.t;
      return_arity : int;
    }

module Const = struct
  type t =
    | Int of int
    | Char of char
    | Const_pointer of int

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = Pervasives.compare
    let equal t1 t2 = (compare t1 t2) = 0
    let hash = Hashtbl.hash

    let print ppf (t : t) =
      match t with
      | Int n -> Format.fprintf ppf "%i" n
      | Char c -> Format.fprintf ppf "%C" c
      | Const_pointer n -> Format.fprintf ppf "%ia" n

    let output _ _ = Misc.fatal_error "Not implemented"
  end)
end

type const = Const.t

type apply_kind =
  | Function
  | Method of { kind : Lambda.meth_kind; obj : Variable.t; }

type apply = {
  kind : apply_kind;
  func : Variable.t;
  continuation : Continuation.t;
  args : Variable.t list;
  call_kind : call_kind;
  dbg : Debuginfo.t;
  inline : Lambda.inline_attribute;
  specialise : Lambda.specialise_attribute;
}

type assign = {
  being_assigned : Mutable_variable.t;
  new_value : Variable.t;
}

type project_closure = Projection.project_closure
type move_within_set_of_closures = Projection.move_within_set_of_closures
type project_var = Projection.project_var

type free_var = {
  var : Variable.t;
  projection : Projection.t option;
}

type free_vars = free_var Variable.Map.t

type specialised_to = {
  var : Variable.t option;
  projection : Projection.t option;
}

type specialised_args = specialised_to Variable.Map.t

type trap_action =
  | Push of { id : Trap_id.t; exn_handler : Continuation.t; }
  | Pop of { id : Trap_id.t; exn_handler : Continuation.t; }

type t =
  | Let of let_expr
  | Let_mutable of let_mutable
  | Let_cont of let_cont
  | Apply of apply
  | Apply_cont of Continuation.t * trap_action option * Variable.t list
  | Switch of Variable.t * switch
  | Proved_unreachable

and named =
  | Var of Variable.t
  | Const of const
  | Prim of Lambda.primitive * Variable.t list * Debuginfo.t
  | Assign of assign
  | Read_mutable of Mutable_variable.t
  | Symbol of Symbol.t
  | Read_symbol_field of Symbol.t * int
  | Allocated_const of Allocated_const.t
  | Set_of_closures of set_of_closures
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Project_var of project_var

and let_expr = {
  var : Variable.t;
  defining_expr : named;
  body : t;
  free_vars_of_defining_expr : Variable.Set.t;
  free_vars_of_body : Variable.Set.t;
}

and let_mutable = {
  var : Mutable_variable.t;
  initial_value : Variable.t;
  contents_kind : Lambda.value_kind;
  body : t;
}

and let_cont = {
  body : t;
  handlers : let_cont_handlers;
}

and let_cont_handlers =
  | Nonrecursive of continuation_handlers
  | Recursive of continuation_handlers
  | Alias of { name : Continuation.t; alias_of : Continuation.t; }

and continuation_handlers =
  continuation_handler Continuation.Map.t

and continuation_handler = {
  params : Parameter.t list;
  stub : bool;
  is_exn_handler : bool;
  handler : t;
  specialised_args : specialised_args;
}

and set_of_closures = {
  function_decls : function_declarations;
  free_vars : free_vars;
  specialised_args : specialised_args;
  direct_call_surrogates : Variable.t Variable.Map.t;
}

and function_declarations = {
  set_of_closures_id : Set_of_closures_id.t;
  set_of_closures_origin : Set_of_closures_origin.t;
  funs : function_declaration Variable.Map.t;
}

and function_declaration = {
  closure_origin : Closure_origin.t;
  continuation_param : Continuation.t;
  return_arity : int;
  params : Parameter.t list;
  body : t;
  free_variables : Variable.Set.t;
  free_symbols : Symbol.Set.t;
  stub : bool;
  dbg : Debuginfo.t;
  inline : Lambda.inline_attribute;
  specialise : Lambda.specialise_attribute;
  is_a_functor : bool;
}

and switch = {
  numconsts : Int.Set.t;
  consts : (int * Continuation.t) list;
  failaction : Continuation.t option;
}

and constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * constant_defining_value_block_field list
  | Set_of_closures of set_of_closures  (* [free_vars] must be empty *)
  | Project_closure of Symbol.t * Closure_id.t

and constant_defining_value_block_field =
  | Symbol of Symbol.t
  | Const of const

type expr = t

type program_body =
  | Let_symbol of Symbol.t * constant_defining_value * program_body
  | Let_rec_symbol of (Symbol.t * constant_defining_value) list * program_body
  | Initialize_symbol of Symbol.t * Tag.t * (t * Continuation.t) list
      * program_body
  | Effect of t * Continuation.t * program_body
  | End of Symbol.t

type program = {
  imported_symbols : Symbol.Set.t;
  program_body : program_body;
}

let fprintf = Format.fprintf

let print_const = Const.print

let print_free_var ppf (free_var : free_var) =
  match free_var.projection with
  | None ->
    fprintf ppf "%a" Variable.print free_var.var
  | Some projection ->
    fprintf ppf "%a(= %a)"
      Variable.print free_var.var
      Projection.print projection

let print_free_vars ppf free_vars =
  Variable.Map.iter (fun id v ->
      fprintf ppf "@ %a -rename-> %a"
        Variable.print id print_free_var v)
    free_vars

let print_specialised_to ppf (spec_to : specialised_to) =
  match spec_to.projection with
  | None ->
    begin match spec_to.var with
    | None -> fprintf ppf "<none>"
    | Some var -> fprintf ppf "%a" Variable.print var
    end
  | Some projection ->
    match spec_to.var with
    | None ->
      fprintf ppf "%a"
        Projection.print projection
    | Some var ->
      fprintf ppf "%a = %a"
        Variable.print var
        Projection.print projection

let print_specialised_args ppf spec_args =
  if not (Variable.Map.is_empty spec_args)
  then begin
    fprintf ppf "@ ";
    Variable.Map.iter (fun id (spec_to : specialised_to) ->
        fprintf ppf "@ %a := %a"
          Variable.print id print_specialised_to spec_to)
      spec_args
  end

let print_specialised_args' ppf spec_args =
  if not (Variable.Map.is_empty spec_args)
  then begin
    fprintf ppf "@ @[<v 2>specialising ";
    Variable.Map.iter (fun id (spec_to : specialised_to) ->
        fprintf ppf "@ %a := %a"
          Variable.print id print_specialised_to spec_to)
      spec_args;
    fprintf ppf "@]"
  end

(* CR-soon mshinwell: delete uses of old names *)
let print_project_var = Projection.print_project_var
let print_move_within_set_of_closures =
  Projection.print_move_within_set_of_closures
let print_project_closure = Projection.print_project_closure

let print_trap_action ppf trap_action =
  match trap_action with
  | None -> ()
  | Some (Push { id; exn_handler; }) ->
    fprintf ppf "push %a %a then " Trap_id.print id
      Continuation.print exn_handler
  | Some (Pop { id; exn_handler; }) ->
    fprintf ppf "pop %a %a then " Trap_id.print id
      Continuation.print exn_handler

(** CR-someday lwhite: use better name than this *)
let rec lam ppf (flam : t) =
  match flam with
  | Apply({kind; func; continuation; args; call_kind; inline;
      dbg}) ->
    let print_func_and_kind ppf func =
      match kind with
      | Function -> Variable.print ppf func
      | Method { kind; obj; } ->
        Format.fprintf ppf "send%a %a#%a"
          Printlambda.meth_kind kind
          Variable.print obj
          Variable.print func
    in
    let direct ppf () =
      match call_kind with
      | Indirect -> ()
      | Direct { closure_id; _ } ->
        fprintf ppf "*[%a]" Closure_id.print closure_id
    in
    let inline ppf () =
      match inline with
      | Always_inline -> fprintf ppf "<always>"
      | Never_inline -> fprintf ppf "<never>"
      | Unroll i -> fprintf ppf "<unroll %i>" i
      | Default_inline -> ()
    in
    let return_arity =
      match call_kind with
      | Indirect -> ""
      | Direct { return_arity; _ } ->
        if return_arity < 2 then ""
        else Printf.sprintf " (return arity %d)" return_arity
    in
    fprintf ppf "@[<2>(apply%a%a<%s>%s@ <%a> %a %a)@]" direct () inline ()
      (Debuginfo.to_string dbg)
      return_arity
      Continuation.print continuation
      print_func_and_kind func
      Variable.print_list args
  | Let { var = id; defining_expr = arg; body; _ } ->
      let rec letbody (ul : t) =
        match ul with
        | Let { var = id; defining_expr = arg; body; _ } ->
            fprintf ppf "@ @[<2>%a@ %a@]" Variable.print id print_named arg;
            letbody body
        | _ -> ul
      in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@ %a@]"
        Variable.print id print_named arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" lam expr
  | Let_mutable { var = mut_var; initial_value = var; body; contents_kind } ->
    let print_kind ppf (kind : Lambda.value_kind) =
      match kind with
      | Pgenval -> ()
      | _ -> Format.fprintf ppf " %s" (Printlambda.value_kind kind)
    in
    fprintf ppf "@[<2>(let_mutable%a@ @[<2>%a@ %a@]@ %a)@]"
      print_kind contents_kind
      Mutable_variable.print mut_var
      Variable.print var
      lam body
  | Switch (scrutinee, sw) ->
    fprintf ppf
      "@[<v 1>(switch %a@ @[<v 0>%a@])@]"
      Variable.print scrutinee print_switch sw
  | Apply_cont (i, trap_action, []) ->
    fprintf ppf "@[<2>(%agoto@ %a)@]"
      print_trap_action trap_action
      Continuation.print i
  | Apply_cont (i, trap_action, ls) ->
    fprintf ppf "@[<2>(%aapply_cont@ %a@ %a)@]"
      print_trap_action trap_action
      Continuation.print i
      Variable.print_list ls
  | Let_cont { body; handlers; } ->
    (* Printing the same way as for [Let] is easier when debugging lifting
       passes. *)
    if !Clflags.dump_let_cont then begin
      let rec let_cont_body (ul : t) =
        match ul with
        | Let_cont { body; handlers; } ->
          fprintf ppf "@ @[<2>%a@]" print_let_cont_handlers handlers;
          let_cont_body body
        | _ -> ul
      in
      fprintf ppf "@[<2>(let_cont@ @[<hv 1>(@[<2>%a@]"
        print_let_cont_handlers handlers;
      let expr = let_cont_body body in
      fprintf ppf ")@]@ %a)@]" lam expr
    end else begin
      (* CR mshinwell: Share code with ilambda.ml *)
      let rec gather_let_conts let_conts (t : t) =
        match t with
        | Let_cont let_cont ->
          gather_let_conts (let_cont :: let_conts) let_cont.body
        | body -> let_conts, body
      in
      let let_conts, body = gather_let_conts [] flam in
      let print_let_cont ppf { handlers; body = _; } =
        let print_handlers recursive handlers =
          let first = ref true in
          fprintf ppf "@[<v 2>where %s" recursive;
          Continuation.Map.iter (fun name
                  { params; stub; is_exn_handler; handler;
                    specialised_args; } ->
              if not !first then fprintf ppf "@ ";
              fprintf ppf "@[%s%a%s%s%s@[%a@]%s@]%a =@ %a"
                (if !first then "" else "and ")
                Continuation.print name
                (if stub then " *stub*" else "")
                (if is_exn_handler then "*exn* " else "")
                (match params with [] -> "" | _ -> " (")
                Parameter.List.print params
                (match params with [] -> "" | _ -> ")")
                print_specialised_args' specialised_args
                lam handler;
              first := false)
            handlers;
          fprintf ppf "@]"
        in
        match handlers with
        | Nonrecursive handlers ->
          print_handlers "" handlers
        | Recursive handlers ->
          print_handlers "rec " handlers
        | Alias { name; alias_of; } ->
          fprintf ppf "@[<v 2>where %a = %a@]"
            Continuation.print name
            Continuation.print alias_of
      in
      let pp_sep ppf () = fprintf ppf "@ " in
      fprintf ppf "@[<2>(@[<v 0>%a@;@[<v 0>%a@]@])@]"
        lam body
        (Format.pp_print_list ~pp_sep print_let_cont) let_conts
    end
  | Proved_unreachable -> fprintf ppf "unreachable"
and print_named ppf (named : named) =
  match named with
  | Var var -> Variable.print ppf var
  | Symbol symbol -> Symbol.print ppf symbol
  | Const cst -> fprintf ppf "Const(%a)" print_const cst
  | Allocated_const cst -> fprintf ppf "Aconst(%a)" Allocated_const.print cst
  | Read_mutable mut_var ->
    fprintf ppf "Read_mut(%a)" Mutable_variable.print mut_var
  | Assign { being_assigned; new_value; } ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]"
      Mutable_variable.print being_assigned
      Variable.print new_value
  | Read_symbol_field (symbol, field) ->
    fprintf ppf "%a.(%d)" Symbol.print symbol field
  | Project_closure project_closure ->
    print_project_closure ppf project_closure
  | Project_var project_var -> print_project_var ppf project_var
  | Move_within_set_of_closures move_within_set_of_closures ->
    print_move_within_set_of_closures ppf move_within_set_of_closures
  | Set_of_closures set_of_closures ->
    print_set_of_closures ppf set_of_closures
  | Prim (prim, args, dbg) ->
    fprintf ppf "@[<2>(%a@ <%s>@ %a)@]" Printlambda.primitive prim
      (Debuginfo.to_string dbg)
      Variable.print_list args

and print_switch ppf (sw : switch) =
  let spc = ref false in
  List.iter
    (fun (n, l) ->
        if !spc then fprintf ppf "@ " else spc := true;
        fprintf ppf "@[<hv 1>| %i ->@ goto %a@]"
          n Continuation.print l)
    sw.consts;
  begin match sw.failaction with
  | None  -> ()
  | Some l ->
      if !spc then fprintf ppf "@ " else spc := true;
      let module Int = Int in
      fprintf ppf "@[<hv 1>| _ ->@ goto %a@]"
        Continuation.print l
  end

and print_let_cont_handlers ppf (handler : let_cont_handlers) =
  let print_generic recursive handlers =
    let first = ref true in
    Continuation.Map.iter (fun name
            { params; stub; is_exn_handler; handler; specialised_args; } ->
        if !first then begin
          fprintf ppf "@;%s" recursive
        end else begin
          fprintf ppf "@;and "
        end;
        fprintf ppf "%a@ %s%s%s%a%s%a=@ %a"
          Continuation.print name
          (if stub then "*stub* " else "")
          (if is_exn_handler then "*exn* " else "")
          (match params with [] -> "" | _ -> "(")
          Parameter.List.print params
          (match params with [] -> "" | _ -> ") ")
          print_specialised_args specialised_args
          lam handler;
        first := false)
      handlers
  in
  match handler with
  | Nonrecursive handlers ->
    print_generic "" handlers
  | Recursive handlers ->
    print_generic "rec " handlers
  | Alias { name; alias_of; } ->
    fprintf ppf "%a = %a" Continuation.print name Continuation.print alias_of

and print_function_declaration ppf var (f : function_declaration) =
  let param ppf p =
    Variable.print ppf (Parameter.var p)
  in
  let params ppf =
    List.iter (fprintf ppf "@ %a" param) in
  let stub =
    if f.stub then
      " *stub*"
    else
      ""
  in
  let arity =
    if f.return_arity < 2 then ""
    else Printf.sprintf " (return arity %d)" f.return_arity
  in
  let is_a_functor =
    if f.is_a_functor then
      " *functor*"
    else
      ""
  in
  let inline =
    match f.inline with
    | Always_inline -> " *inline*"
    | Never_inline -> " *never_inline*"
    | Unroll _ -> " *unroll*"
    | Default_inline -> ""
  in
  let specialise =
    match f.specialise with
    | Always_specialise -> " *specialise*"
    | Never_specialise -> " *never_specialise*"
    | Default_specialise -> ""
  in
  fprintf ppf
    "@[<2>(%a%s%s%s%s%s(origin %a)@ =@ fun@[<2> <%a>%a@] ->@ @[<2>%a@])@]@ "
    Variable.print var stub arity is_a_functor inline specialise
    Closure_origin.print f.closure_origin
    Continuation.print f.continuation_param
    params f.params lam f.body

and print_set_of_closures ppf (set_of_closures : set_of_closures) =
  match set_of_closures with
  | { function_decls; free_vars; specialised_args} ->
    let funs ppf =
      Variable.Map.iter (print_function_declaration ppf)
    in
    fprintf ppf "@[<2>(set_of_closures id=%a@ %a@ @[<2>free_vars={%a@ }@]@ \
        @[<2>specialised_args={%a})@]@ \
        @[<2>direct_call_surrogates=%a@]@ \
        @[<2>set_of_closures_origin=%a@]@]"
      Set_of_closures_id.print function_decls.set_of_closures_id
      funs function_decls.funs
      print_free_vars free_vars
      print_specialised_args specialised_args
      (Variable.Map.print Variable.print)
      set_of_closures.direct_call_surrogates
      Set_of_closures_origin.print function_decls.set_of_closures_origin

let print_function_declarations ppf (fd : function_declarations) =
  let funs ppf =
    Variable.Map.iter (print_function_declaration ppf)
  in
  fprintf ppf "@[<2>(%a)(origin = %a)@]" funs fd.funs
    Set_of_closures_origin.print fd.set_of_closures_origin

let print ppf flam =
  fprintf ppf "%a@." lam flam

let print_function_declaration ppf (var, decl) =
  print_function_declaration ppf var decl

let print_constant_defining_value ppf (const : constant_defining_value) =
  match const with
  | Allocated_const const ->
    fprintf ppf "(Allocated_const %a)" Allocated_const.print const
  | Block (tag, []) -> fprintf ppf "(Empty block (tag %d))" (Tag.to_int tag)
  | Block (tag, fields) ->
    let print_field ppf (field : constant_defining_value_block_field) =
      match field with
      | Symbol symbol -> Symbol.print ppf symbol
      | Const const -> print_const ppf const
    in
    let print_fields ppf =
      List.iter (fprintf ppf "@ %a" print_field)
    in
    fprintf ppf "(Block (tag %d, %a))" (Tag.to_int tag)
      print_fields fields
  | Set_of_closures set_of_closures ->
    fprintf ppf "@[<2>(Set_of_closures (@ %a))@]" print_set_of_closures
      set_of_closures
  | Project_closure (set_of_closures, closure_id) ->
    fprintf ppf "(Project_closure (%a, %a))" Symbol.print set_of_closures
      Closure_id.print closure_id

let rec print_program_body ppf (program : program_body) =
  match program with
  | Let_symbol (symbol, constant_defining_value, body) ->
    let rec letbody (ul : program_body) =
      match ul with
      | Let_symbol (symbol, constant_defining_value, body) ->
        fprintf ppf "@ @[<2>(%a@ %a)@]" Symbol.print symbol
          print_constant_defining_value constant_defining_value;
        letbody body
      | _ -> ul
    in
    fprintf ppf "@[<2>let_symbol@ @[<hv 1>(@[<2>%a@ %a@])@]@ "
      Symbol.print symbol
      print_constant_defining_value constant_defining_value;
    let program = letbody body in
    fprintf ppf "@]@.";
    print_program_body ppf program
  | Let_rec_symbol (defs, program) ->
    let bindings ppf id_arg_list =
      let spc = ref false in
      List.iter
        (fun (symbol, constant_defining_value) ->
           if !spc then fprintf ppf "@ " else spc := true;
           fprintf ppf "@[<2>%a@ %a@]"
             Symbol.print symbol
             print_constant_defining_value constant_defining_value)
        id_arg_list in
    fprintf ppf
      "@[<2>let_rec_symbol@ (@[<hv 1>%a@])@]@."
      bindings defs;
    print_program_body ppf program
  | Initialize_symbol (symbol, tag, fields, program) ->
    let lam_and_cont ppf (field, defn, cont) =
      fprintf ppf "Field %d, return continuation %a:@;@[<h>@ @ %a@]"
        field Continuation.print cont lam defn
    in
    let pp_sep ppf () = fprintf ppf "@ " in
    fprintf ppf
      "@[<2>initialize_symbol@ @[<hv 1>(@[<2>%a@ %a@;@[<v>%a@]@])@]@]@."
      Symbol.print symbol
      Tag.print tag
      (Format.pp_print_list ~pp_sep lam_and_cont)
      (List.mapi (fun i (defn, cont) -> i, defn, cont) fields);
    print_program_body ppf program
  | Effect (lam, cont, program) ->
    fprintf ppf "@[effect @[<v 2><%a>:@. %a@]@]"
      Continuation.print cont print lam;
    print_program_body ppf program;
  | End root -> fprintf ppf "End %a" Symbol.print root

let print_program ppf program =
  Symbol.Set.iter (fun symbol ->
      fprintf ppf "@[import_symbol@ %a@]@." Symbol.print symbol)
    program.imported_symbols;
  print_program_body ppf program.program_body

let free_variables_of_specialised_args specialised_args =
  Variable.Map.fold (fun _ (spec_to : specialised_to) fvs ->
      (* We don't need to do anything with [spec_to.projectee.var], if
         it is present, since it would only be another specialised arg
         in the same set of closures or continuation. *)
      match spec_to.var with
      | None -> fvs
      | Some var -> Variable.Set.add var fvs)
    specialised_args
    Variable.Set.empty

let rec variables_usage ?ignore_uses_as_callee
    ?ignore_uses_as_argument ?ignore_uses_as_continuation_argument
    ?ignore_uses_in_project_var ?ignore_uses_in_apply_cont
    ~all_used_variables tree =
  let free = ref Variable.Set.empty in
  let bound = ref Variable.Set.empty in
  let free_variables ids = free := Variable.Set.union ids !free in
  let free_variable fv = free := Variable.Set.add fv !free in
  let bound_variable id = bound := Variable.Set.add id !bound in
  (* N.B. This function assumes that all bound identifiers are distinct. *)
  let rec aux (flam : t) : unit =
    match flam with
    | Apply { func; args; kind; _ } ->
      begin match ignore_uses_as_callee with
      | None -> free_variable func
      | Some () -> ()
      end;
      begin match kind with
      | Function -> ()
      | Method { obj; _ } -> free_variable obj
      end;
      begin match ignore_uses_as_argument with
      | None -> List.iter free_variable args
      | Some () -> ()
      end
    | Let { var; free_vars_of_defining_expr; free_vars_of_body;
            defining_expr; body; _ } ->
      bound_variable var;
      if all_used_variables
          || ignore_uses_as_callee <> None
          || ignore_uses_as_argument <> None
          || ignore_uses_as_continuation_argument <> None
          || ignore_uses_in_project_var <> None
          || ignore_uses_in_apply_cont <> None
      then begin
        (* In these cases we can't benefit from the pre-computed free
            variable sets. *)
        free_variables
          (variables_usage_named ?ignore_uses_in_project_var defining_expr);
        aux body
      end else begin
        free_variables free_vars_of_defining_expr;
        free_variables free_vars_of_body
      end
    | Let_mutable { initial_value = var; body; _ } ->
      free_variable var;
      aux body
    | Apply_cont (_, _, es) ->
      (* CR mshinwell: why two variables? *)
      begin match ignore_uses_in_apply_cont with
      | Some () -> ()
      | None ->
        match ignore_uses_as_continuation_argument with
        | None -> List.iter free_variable es
        | Some () -> ()
      end
    | Let_cont { handlers; body; } ->
      aux body;
      begin match handlers with
      | Alias _ -> ()
      | Nonrecursive handlers | Recursive handlers ->
        Continuation.Map.iter (fun _name { params; handler;
                specialised_args; _ } ->
            List.iter (fun param -> bound_variable (Parameter.var param))
              params;
            free_variables
              (free_variables_of_specialised_args specialised_args);
            aux handler)
          handlers
      end
    | Switch (var, _) -> free_variable var
    | Proved_unreachable -> ()
  in
  aux tree;
  if all_used_variables then
    !free
  else
    Variable.Set.diff !free !bound

and variables_usage_named ?ignore_uses_in_project_var (named : named) =
  match named with
  | Var var -> Variable.Set.singleton var
  | _ ->
    let free = ref Variable.Set.empty in
    let free_variable fv = free := Variable.Set.add fv !free in
    begin match named with
    | Var var -> free_variable var
    | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
    | Read_symbol_field _ -> ()
    | Assign { being_assigned = _; new_value; } ->
      free_variable new_value
    | Set_of_closures { free_vars; specialised_args; _ } ->
      (* Sets of closures are, well, closed---except for the free variable and
         specialised argument lists, which may identify variables currently in
         scope outside of the closure. *)
      Variable.Map.iter (fun _ (renamed_to : free_var) ->
          (* We don't need to do anything with [renamed_to.projectee.var], if
             it is present, since it would only be another free variable
             in the same set of closures. *)
          free_variable renamed_to.var)
        free_vars;
      Variable.Set.iter (fun var -> free_variable var)
        (free_variables_of_specialised_args specialised_args)
    | Project_closure { set_of_closures; closure_id = _ } ->
      free_variable set_of_closures
    | Project_var { closure; var = _ } ->
      begin match ignore_uses_in_project_var with
      | None -> free_variable closure
      | Some () -> ()
      end
    | Move_within_set_of_closures { closure; move = _ } ->
      free_variable closure
    | Prim (_, args, _) -> List.iter free_variable args
    end;
    !free

let free_variables ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var
    ?ignore_uses_in_apply_cont tree =
  variables_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var
    ?ignore_uses_in_apply_cont ~all_used_variables:false tree

let free_variables_named ?ignore_uses_in_project_var named =
  variables_usage_named ?ignore_uses_in_project_var named

let used_variables ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var tree =
  variables_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
    ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var
    ~all_used_variables:true tree

let used_variables_named ?ignore_uses_in_project_var named =
  variables_usage_named ?ignore_uses_in_project_var named

let create_let var defining_expr body : t =
  begin match !Clflags.dump_flambda_let with
  | None -> ()
  | Some stamp ->
    Variable.debug_when_stamp_matches var ~stamp ~f:(fun () ->
      Printf.eprintf "Creation of [Let] with stamp %d:\n%s\n%!"
        stamp
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int)))
  end;
  let free_vars_of_defining_expr = free_variables_named defining_expr in
  Let {
    var;
    defining_expr;
    body;
    free_vars_of_defining_expr;
    free_vars_of_body = free_variables body;
  }

let map_defining_expr_of_let let_expr ~f =
  let defining_expr = f let_expr.defining_expr in
  if defining_expr == let_expr.defining_expr then
    Let let_expr
  else
    let free_vars_of_defining_expr =
      free_variables_named defining_expr
    in
    Let {
      var = let_expr.var;
      defining_expr;
      body = let_expr.body;
      free_vars_of_defining_expr;
      free_vars_of_body = let_expr.free_vars_of_body;
    }

let iter_lets t ~for_defining_expr ~for_last_body ~for_each_let =
  let rec loop (t : t) =
    match t with
    | Let { var; defining_expr; body; _ } ->
      for_each_let t;
      for_defining_expr var defining_expr;
      loop body
    | t ->
      for_last_body t
  in
  loop t

let map_lets t ~for_defining_expr ~for_last_body ~after_rebuild =
  let rec loop (t : t) ~rev_lets =
    match t with
    | Let { var; defining_expr; body; _ } ->
      let new_defining_expr =
        for_defining_expr var defining_expr
      in
      let original =
        if new_defining_expr == defining_expr then
          Some t
        else
          None
      in
      let rev_lets = (var, new_defining_expr, original) :: rev_lets in
      loop body ~rev_lets
    | t ->
      let last_body = for_last_body t in
      (* As soon as we see a change, we have to rebuild that [Let] and every
         outer one. *)
      let seen_change = ref (not (last_body == t)) in
      List.fold_left (fun t (var, defining_expr, original) ->
          let let_expr =
            match original with
            | Some original when not !seen_change -> original
            | Some _ | None ->
              seen_change := true;
              create_let var defining_expr t
          in
          let new_let = after_rebuild let_expr in
          if not (new_let == let_expr) then begin
            seen_change := true
          end;
          new_let)
        last_body
        rev_lets
  in
  loop t ~rev_lets:[]

(** CR-someday lwhite: Why not use two functions? *)
type maybe_named =
  | Is_expr of t
  | Is_named of named

let iter_general ~toplevel f f_named maybe_named =
  let rec aux (t : t) =
    match t with
    | Let _ ->
      iter_lets t
        ~for_defining_expr:(fun _var named -> aux_named named)
        ~for_last_body:aux
        ~for_each_let:f
    (* CR mshinwell: add tail recursive case for Let_cont *)
    | _ ->
      f t;
      match t with
      | Apply _ | Apply_cont _ | Switch _ -> ()
      | Let _ -> assert false
      | Let_mutable { body; _ } -> aux body
      | Let_cont { body; handlers; _ } ->
        aux body;
        begin match handlers with
        | Nonrecursive handlers | Recursive handlers ->
          Continuation.Map.iter (fun _cont { handler; } -> aux handler)
            handlers
        | Alias _ -> ()
        end
      | Proved_unreachable -> ()
  and aux_named (named : named) =
    f_named named;
    match named with
    | Var _ | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
    | Read_symbol_field _ | Project_closure _ | Project_var _
    | Move_within_set_of_closures _ | Prim _ | Assign _ -> ()
    | Set_of_closures ({ function_decls = funcs; free_vars = _;
          specialised_args = _}) ->
      if not toplevel then begin
        Variable.Map.iter (fun _ (decl : function_declaration) ->
            aux decl.body)
          funcs.funs
      end
  in
  match maybe_named with
  | Is_expr expr -> aux expr
  | Is_named named -> aux_named named

module With_free_variables = struct
  type 'a t =
    | Expr : expr * Variable.Set.t -> expr t
    | Named : named * Variable.Set.t -> named t

  let print (type a) ppf (t : a t) =
    match t with
    | Expr (expr, _) -> print ppf expr
    | Named (named, _) -> print_named ppf named

  let of_defining_expr_of_let let_expr =
    Named (let_expr.defining_expr, let_expr.free_vars_of_defining_expr)

  let of_body_of_let let_expr =
    Expr (let_expr.body, let_expr.free_vars_of_body)

  let of_expr expr =
    Expr (expr, free_variables expr)

  let of_named named =
    Named (named, free_variables_named named)

  let to_named (t : named t) =
    match t with
    | Named (named, _) -> named

  let create_let_reusing_defining_expr var (t : named t) body =
    match t with
    | Named (defining_expr, free_vars_of_defining_expr) ->
      Let {
        var;
        defining_expr;
        body;
        free_vars_of_defining_expr;
        free_vars_of_body = free_variables body;
      }

  let create_let_reusing_body var defining_expr (t : expr t) =
    match t with
    | Expr (body, free_vars_of_body) ->
      Let {
        var;
        defining_expr;
        body;
        free_vars_of_defining_expr = free_variables_named defining_expr;
        free_vars_of_body;
      }

  let create_let_reusing_both var (t1 : named t) (t2 : expr t) =
    match t1, t2 with
    | Named (defining_expr, free_vars_of_defining_expr),
        Expr (body, free_vars_of_body) ->
      Let {
        var;
        defining_expr;
        body;
        free_vars_of_defining_expr;
        free_vars_of_body;
      }

  let contents (type a) (t : a t) : a =
    match t with
    | Expr (expr, _) -> expr
    | Named (named, _) -> named

  let free_variables (type a) (t : a t) =
    match t with
    | Expr (_, free_vars) -> free_vars
    | Named (_, free_vars) -> free_vars
end

type named_reachable =
  | Reachable of named
  | Non_terminating of named
  | Unreachable

let fold_lets_option t ~init ~for_defining_expr ~for_last_body
      ~filter_defining_expr =
  let finish ~last_body ~acc ~rev_lets =
    let module W = With_free_variables in
    let acc, t =
      List.fold_left (fun (acc, t) (var, defining_expr) ->
          let free_vars_of_body = W.free_variables t in
          let acc, var, defining_expr =
            filter_defining_expr acc var defining_expr free_vars_of_body
          in
          match defining_expr with
          | None ->
(*
Format.eprintf "Deleting binding of %a.  FVs of body %a.  Body:\n%a\n\
    Backtrace:\n%s%!"
  Variable.print var Variable.Set.print free_vars_of_body W.print t
  (Printexc.raw_backtrace_to_string (Printexc.get_callstack 100));
*)
            acc, t
          | Some defining_expr ->
            let let_expr =
              W.create_let_reusing_body var defining_expr t
            in
            acc, W.of_expr let_expr)
        (acc, W.of_expr last_body)
        rev_lets
    in
    W.contents t, acc
  in
  let rec loop (t : t) ~acc ~rev_lets =
    match t with
    | Let { var; defining_expr; body; _ } ->
      let acc, bindings, var, (defining_expr : named_reachable) =
        for_defining_expr acc var defining_expr
      in
      begin match defining_expr with
      | Reachable defining_expr ->
        let rev_lets =
          (var, defining_expr) :: (List.rev bindings) @ rev_lets
        in
        loop body ~acc ~rev_lets
      | Non_terminating defining_expr ->
(*
Format.eprintf "This defining expression doesn't terminate:\n@ %a\n%!"
  print_named defining_expr;
*)
        let rev_lets =
          (var, defining_expr) :: (List.rev bindings) @ rev_lets
        in
        let last_body, acc = for_last_body acc Proved_unreachable in
        finish ~last_body ~acc ~rev_lets
      | Unreachable ->
        let rev_lets = (List.rev bindings) @ rev_lets in
        let last_body, acc = for_last_body acc Proved_unreachable in
        finish ~last_body ~acc ~rev_lets
      end
    | t ->
      let last_body, acc = for_last_body acc t in
      finish ~last_body ~acc ~rev_lets
  in
  loop t ~acc:init ~rev_lets:[]

let rec free_continuations (expr : expr) =
  match expr with
  | Let { body; _ }
  | Let_mutable { body; _ } ->
    free_continuations body
  | Let_cont { body; handlers; } ->
    let fcs_handlers, bound_conts =
      free_continuations_of_let_cont_handlers' ~handlers
    in
    Continuation.Set.union fcs_handlers
     (Continuation.Set.diff (free_continuations body) bound_conts)
  | Apply_cont (cont, trap_action, _args) ->
    let trap_action =
      match trap_action with
      | Some (Push { exn_handler; _ })
      | Some (Pop { exn_handler; _ }) -> Continuation.Set.singleton exn_handler
      | None -> Continuation.Set.empty
    in
    Continuation.Set.add cont trap_action
  | Apply { continuation; } -> Continuation.Set.singleton continuation
  | Switch (_scrutinee, switch) ->
    let consts = List.map (fun (_int, cont) -> cont) switch.consts in
    let failaction =
      match switch.failaction with
      | None -> Continuation.Set.empty
      | Some cont -> Continuation.Set.singleton cont
    in
    Continuation.Set.union failaction (Continuation.Set.of_list consts)
  | Proved_unreachable -> Continuation.Set.empty

and free_continuations_of_let_cont_handlers' ~(handlers : let_cont_handlers) =
  match handlers with
  | Alias { name; alias_of; } ->
    Continuation.Set.singleton alias_of, Continuation.Set.singleton name
  | Nonrecursive nonrec_handlers ->
    let bound_conts = Continuation.Map.keys nonrec_handlers in
    let fcs =
      Continuation.Map.fold (fun _name { handler; _ } fcs ->
          Continuation.Set.union fcs
            (free_continuations handler))
        nonrec_handlers
        Continuation.Set.empty
    in
    if not (Continuation.Set.is_empty
              (Continuation.Set.inter bound_conts fcs)) then begin
      Misc.fatal_errorf "Nonrecursive [Let_cont] handler appears to be \
          recursive:@ \n%a"
        print_let_cont_handlers handlers
    end;
    fcs, bound_conts
  | Recursive handlers ->
    let bound_conts = Continuation.Map.keys handlers in
    let fcs =
      Continuation.Map.fold (fun _name { handler; _ } fcs ->
          Continuation.Set.union fcs
            (Continuation.Set.diff (free_continuations handler) bound_conts))
        handlers
        Continuation.Set.empty
    in
    fcs, bound_conts

let free_continuations_of_let_cont_handlers ~(handlers : let_cont_handlers) =
  fst (free_continuations_of_let_cont_handlers' ~handlers)

let free_variables_of_let_cont_handlers (handlers : let_cont_handlers) =
  match handlers with
  | Alias _ -> Variable.Set.empty
  | Nonrecursive handlers | Recursive handlers ->
    Continuation.Map.fold (fun _name { params; handler; specialised_args; _ }
            fvs ->
        Variable.Set.union fvs
          (Variable.Set.union
            (free_variables_of_specialised_args specialised_args)
            (Variable.Set.diff (free_variables handler)
              (Parameter.Set.vars params))))
      handlers
      Variable.Set.empty

let free_symbols_helper symbols (named : named) =
  match named with
  | Symbol symbol
  | Read_symbol_field (symbol, _) -> symbols := Symbol.Set.add symbol !symbols
  | Set_of_closures set_of_closures ->
    Variable.Map.iter (fun _ (function_decl : function_declaration) ->
        symbols := Symbol.Set.union function_decl.free_symbols !symbols)
      set_of_closures.function_decls.funs
  | _ -> ()

let free_symbols expr =
  let symbols = ref Symbol.Set.empty in
  iter_general ~toplevel:true
    (fun (_ : t) -> ())
    (fun (named : named) -> free_symbols_helper symbols named)
    (Is_expr expr);
  !symbols

let free_symbols_named named =
  let symbols = ref Symbol.Set.empty in
  iter_general ~toplevel:true
    (fun (_ : t) -> ())
    (fun (named : named) -> free_symbols_helper symbols named)
    (Is_named named);
  !symbols

let free_symbols_allocated_constant_helper symbols
      (const : constant_defining_value) =
  match const with
  | Allocated_const _ -> ()
  | Block (_, fields) ->
    List.iter
      (function
        | (Symbol s : constant_defining_value_block_field) ->
          symbols := Symbol.Set.add s !symbols
        | (Const _ : constant_defining_value_block_field) -> ())
      fields
  | Set_of_closures set_of_closures ->
    symbols := Symbol.Set.union !symbols
      (free_symbols_named (Set_of_closures set_of_closures))
  | Project_closure (s, _) ->
    symbols := Symbol.Set.add s !symbols

let free_symbols_program (program : program) =
  let symbols = ref Symbol.Set.empty in
  let rec loop (program : program_body) =
    match program with
    | Let_symbol (_, const, program) ->
      free_symbols_allocated_constant_helper symbols const;
      loop program
    | Let_rec_symbol (defs, program) ->
      List.iter (fun (_, const) ->
          free_symbols_allocated_constant_helper symbols const)
        defs;
      loop program
    | Initialize_symbol (_, _, fields, program) ->
      List.iter (fun (field, _cont) ->
          symbols := Symbol.Set.union !symbols (free_symbols field))
        fields;
      loop program
    | Effect (expr, _cont, program) ->
      symbols := Symbol.Set.union !symbols (free_symbols expr);
      loop program
    | End symbol -> symbols := Symbol.Set.add symbol !symbols
  in
  (* Note that there is no need to count the [imported_symbols]. *)
  loop program.program_body;
  !symbols

let create_switch' ~scrutinee ~all_possible_values ~arms ~default
      : expr * (int Continuation.Map.t) =
  let result_switch : switch =
    { numconsts = all_possible_values;
      consts = arms;
      failaction = default;
    }
  in
  let result : expr = Switch (scrutinee, result_switch) in
  let arms =
    List.sort (fun (value1, _) (value2, _) -> Pervasives.compare value1 value2)
      arms
  in
  let num_possible_values = Int.Set.cardinal all_possible_values in
  let num_arms = List.length arms in
  let arm_values = List.map (fun (value, _cont) -> value) arms in
  let num_arm_values = List.length arm_values in
  let arm_values_set = Int.Set.of_list arm_values in
  let num_arm_values_set = Int.Set.cardinal arm_values_set in
  if num_arm_values <> num_arm_values_set then begin
    Misc.fatal_errorf "More than one arm of this switch matches on \
        the same value: %a"
      print result
  end;
  if num_arms > num_possible_values then begin
    Misc.fatal_errorf "This switch has too many arms: %a"
      print result
  end;
  if not (Int.Set.subset arm_values_set all_possible_values) then begin
    Misc.fatal_errorf "This switch matches on values that were not specified \
        in the set of all possible values: %a"
      print result
  end;
  if num_possible_values < 1 then begin
    Proved_unreachable, Continuation.Map.empty
  end else if num_arms = 0 && default = None then begin
    (* [num_possible_values] might be strictly greater than zero in this
       case, but that doesn't matter. *)
    Proved_unreachable, Continuation.Map.empty
  end else begin
    let default =
      if num_arm_values = num_possible_values then None
      else default
    in
    let single_case =
      match arms, default with
      | [_, cont], None
      | [], Some cont -> Some cont
      | arms, default ->
        let destinations =
          Continuation.Set.of_list (List.map (fun (_, cont) -> cont) arms)
        in
        assert (not (Continuation.Set.is_empty destinations));
        match Continuation.Set.elements destinations, default with
        | [cont], None -> Some cont
        | [cont], Some cont' when Continuation.equal cont cont' -> Some cont
        | _, _ -> None
    in
    match single_case with
    | Some cont ->
      Apply_cont (cont, None, []),
        Continuation.Map.add cont 1 Continuation.Map.empty
    | None ->
      let num_uses = Continuation.Tbl.create 42 in
      let add_use cont =
        match Continuation.Tbl.find num_uses cont with
        | exception Not_found -> Continuation.Tbl.add num_uses cont 1
        | num -> Continuation.Tbl.replace num_uses cont (num + 1)
      in
      List.iter (fun (_const, cont) -> add_use cont) result_switch.consts;
      begin match default with
      | None -> ()
      | Some default -> add_use default
      end;
      Switch (scrutinee, { result_switch with failaction = default; }),
        Continuation.Tbl.to_map num_uses
  end

let create_switch ~scrutinee ~all_possible_values ~arms ~default =
  let switch, _uses =
    create_switch' ~scrutinee ~all_possible_values ~arms ~default
  in
  switch

let create_function_declaration ~params ~continuation_param ~return_arity
      ~body ~stub ~dbg
      ~(inline : Lambda.inline_attribute)
      ~(specialise : Lambda.specialise_attribute) ~is_a_functor
      ~closure_origin
      : function_declaration =
  begin match stub, inline with
  | true, (Never_inline | Default_inline)
  | false, (Never_inline | Default_inline | Always_inline | Unroll _) -> ()
  | true, (Always_inline | Unroll _) ->
    Misc.fatal_errorf
      "Stubs may not be annotated as [Always_inline] or [Unroll]: %a"
      print body
  end;
  begin match stub, specialise with
  | true, (Never_specialise | Default_specialise)
  | false, (Never_specialise | Default_specialise | Always_specialise) -> ()
  | true, Always_specialise ->
    Misc.fatal_errorf
      "Stubs may not be annotated as [Always_specialise]: %a"
      print body
  end;
  if return_arity < 1 then begin
    Misc.fatal_errorf "Illegal return arity %d" return_arity
  end;
  { closure_origin;
    params;
    continuation_param;
    return_arity;
    body;
    free_variables = free_variables body;
    free_symbols = free_symbols body;
    stub;
    dbg;
    inline;
    specialise;
    is_a_functor;
  }

let update_body_of_function_declaration (func_decl: function_declaration)
      ~body : function_declaration =
  { closure_origin = func_decl.closure_origin;
    params = func_decl.params;
    continuation_param = func_decl.continuation_param;
    return_arity = func_decl.return_arity;
    body;
    free_variables = free_variables body;
    free_symbols = free_symbols body;
    stub = func_decl.stub;
    dbg = func_decl.dbg;
    inline = func_decl.inline;
    specialise = func_decl.specialise;
    is_a_functor = func_decl.is_a_functor;
  }

let update_function_decl's_params_and_body
      (func_decl : function_declaration) ~params ~body =
  { closure_origin = func_decl.closure_origin;
    params;
    continuation_param = func_decl.continuation_param;
    return_arity = func_decl.return_arity;
    body;
    free_variables = free_variables body;
    free_symbols = free_symbols body;
    stub = func_decl.stub;
    dbg = func_decl.dbg;
    inline = func_decl.inline;
    specialise = func_decl.specialise;
    is_a_functor = func_decl.is_a_functor;
  }

let create_function_declarations ~funs =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let set_of_closures_id = Set_of_closures_id.create compilation_unit in
  let set_of_closures_origin =
    Set_of_closures_origin.create set_of_closures_id
  in
  { set_of_closures_id;
    set_of_closures_origin;
    funs;
  }

let update_function_declarations function_decls ~funs =
  let compilation_unit = Compilation_unit.get_current_exn () in
  let set_of_closures_id = Set_of_closures_id.create compilation_unit in
  let set_of_closures_origin = function_decls.set_of_closures_origin in
  { set_of_closures_id;
    set_of_closures_origin;
    funs;
  }

let import_function_declarations_for_pack function_decls
    import_set_of_closures_id import_set_of_closures_origin =
  { set_of_closures_id =
      import_set_of_closures_id function_decls.set_of_closures_id;
    set_of_closures_origin =
      import_set_of_closures_origin function_decls.set_of_closures_origin;
    funs = function_decls.funs;
  }

let create_set_of_closures ~function_decls ~free_vars ~specialised_args
      ~direct_call_surrogates =
  if !Clflags.flambda_invariant_checks then begin
    let all_fun_vars = Variable.Map.keys function_decls.funs in
    let expected_free_vars =
      Variable.Map.fold (fun _fun_var (function_decl : function_declaration)
                expected_free_vars ->
          let free_vars =
            Variable.Set.diff function_decl.free_variables
              (Variable.Set.union (Parameter.Set.vars function_decl.params)
                all_fun_vars)
          in
          Variable.Set.union free_vars expected_free_vars)
        function_decls.funs
        Variable.Set.empty
    in
    (* CR-soon pchambart: We do not seem to be able to maintain the
       invariant that if a variable is not used inside the closure, it
       is not used outside either. This would be a nice property for
       better dead code elimination during inline_and_simplify, but it
       is not obvious how to ensure that.

       This would be true when the function is known never to have
       been inlined.

       Note that something like that may maybe enforcable in
       inline_and_simplify, but there is no way to do that on other
       passes.

       mshinwell: see CR in Flambda_invariants about this too
    *)
    let free_vars_domain = Variable.Map.keys free_vars in
    if not (Variable.Set.subset expected_free_vars free_vars_domain) then begin
      Misc.fatal_errorf "create_set_of_closures: [free_vars] mapping of \
          variables bound by the closure(s) is wrong.  (Must map at least \
          %a but only maps %a.)@ \nfunction_decls:@ %a"
        Variable.Set.print expected_free_vars
        Variable.Set.print free_vars_domain
        print_function_declarations function_decls
    end;
    let all_params =
      Variable.Map.fold (fun _fun_var (function_decl : function_declaration)
                all_params ->
          Variable.Set.union (Parameter.Set.vars function_decl.params)
            all_params)
        function_decls.funs
        Variable.Set.empty
    in
    let spec_args_domain = Variable.Map.keys specialised_args in
    if not (Variable.Set.subset spec_args_domain all_params) then begin
      Misc.fatal_errorf "create_set_of_closures: [specialised_args] \
          maps variable(s) that are not parameters of the given function \
          declarations.  specialised_args domain=%a all_params=%a \n\
          function_decls:@ %a"
        Variable.Set.print spec_args_domain
        Variable.Set.print all_params
        print_function_declarations function_decls
    end
  end;
  { function_decls;
    free_vars;
    specialised_args;
    direct_call_surrogates;
  }

let used_params (function_decl : function_declaration) =
  Variable.Set.filter
    (fun param -> Variable.Set.mem param function_decl.free_variables)
    (Parameter.Set.vars function_decl.params)

let compare_constant_defining_value_block_field
    (c1:constant_defining_value_block_field)
    (c2:constant_defining_value_block_field) =
  match c1, c2 with
  | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
  | Const c1, Const c2 -> Const.compare c1 c2
  | Symbol _, Const _ -> -1
  | Const _, Symbol _ -> 1

module Constant_defining_value = struct
  type t = constant_defining_value

  include Identifiable.Make (struct
    type nonrec t = t

    let compare (t1 : t) (t2 : t) =
      match t1, t2 with
      | Allocated_const c1, Allocated_const c2 ->
        Allocated_const.compare c1 c2
      | Block (tag1, fields1), Block (tag2, fields2) ->
        let c = Tag.compare tag1 tag2 in
        if c <> 0 then c
        else
          Misc.Stdlib.List.compare compare_constant_defining_value_block_field
            fields1 fields2
      | Set_of_closures set1, Set_of_closures set2 ->
        Set_of_closures_id.compare set1.function_decls.set_of_closures_id
          set2.function_decls.set_of_closures_id
      | Project_closure (set1, closure_id1),
          Project_closure (set2, closure_id2) ->
        let c = Symbol.compare set1 set2 in
        if c <> 0 then c
        else Closure_id.compare closure_id1 closure_id2
      | Allocated_const _, Block _ -> -1
      | Allocated_const _, Set_of_closures _ -> -1
      | Allocated_const _, Project_closure _ -> -1
      | Block _, Allocated_const _ -> 1
      | Block _, Set_of_closures _ -> -1
      | Block _, Project_closure _ -> -1
      | Set_of_closures _, Allocated_const _ -> 1
      | Set_of_closures _, Block _ -> 1
      | Set_of_closures _, Project_closure _ -> -1
      | Project_closure _, Allocated_const _ -> 1
      | Project_closure _, Block _ -> 1
      | Project_closure _, Set_of_closures _ -> 1

    let equal t1 t2 =
      t1 == t2 || compare t1 t2 = 0

    let hash = Hashtbl.hash

    let print = print_constant_defining_value

    let output o v =
      output_string o (Format.asprintf "%a" print v)
  end)
end

let compare_free_var (free_var1 : free_var)
      (free_var2 : free_var) =
  let c = Variable.compare free_var1.var free_var2.var in
  if c <> 0 then c
  else
    match free_var1.projection, free_var2.projection with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some proj1, Some proj2 -> Projection.compare proj1 proj2

let equal_free_var (free_var1 : free_var)
      (free_var2 : free_var) =
  compare_free_var free_var1 free_var2 = 0

let compare_specialised_to (spec_to1 : specialised_to)
      (spec_to2 : specialised_to) =
  let c =
    match spec_to1.var, spec_to2.var with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some var1, Some var2 -> Variable.compare var1 var2
  in
  if c <> 0 then c
  else
    match spec_to1.projection, spec_to2.projection with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some proj1, Some proj2 -> Projection.compare proj1 proj2

let equal_specialised_to (spec_to1 : specialised_to)
      (spec_to2 : specialised_to) =
  compare_specialised_to spec_to1 spec_to2 = 0

let compare_project_var = Projection.compare_project_var
let compare_project_closure = Projection.compare_project_closure
let compare_move_within_set_of_closures =
  Projection.compare_move_within_set_of_closures
let compare_const = Const.compare

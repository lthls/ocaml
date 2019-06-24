module Env = struct
  type t =
    { conts : int Continuation.Map.t;
      known_functions : (* ?? *);
    }

  let add_cont cont n t =
    { t with conts = Continuation.Map.add cont n t.conts; }

  let find_cont_exn cont t =
    Continuation.Map.find cont t.conts
end

type result =
  { expr : Cmm.expression;
    data : Cmm.data_item list;
    functions : Flambda.Function_declaration list;
    sets_of_closures : Flambda.Set_of_closures.t Symbol.Map.t;
    closures : (Symbol.t * Closure_id.t) Symbol.Map.t;
  }

let empty_result =
  { expr = Ctuple [];
    data = [];
    functions = [];
    sets_of_closures = Symbol.Map.empty;
    closures = Symbol.Map.empty;
  }

let combine_result r1 r2 =
  { expr = (match r1.expr with
        | Ctuple [] -> r2.expr
        | _ -> Csequence (r1.expr, r2.expr));
    data = r1.data @ r2.data;
    functions = r1.functions @ r2.functions;
    sets_of_closures = Symbol.Map.

let transl_cont env cont =
  let n = Continuation.next_raise_count () in
  n, Env.add_cont cont n env

let wrap_exn nexn exp =
  let exn_ident = Ident.create "exn" in
  Ccatch (Nonrecursive,
          [nexn, [exn_ident],
           raise_regular Debuginfo.none (Cvar exn_ident)],
          exp)

let wrap_return ncont exp ids updater =
  Ccatch (Nonrecursive,
          [ncont, ids, updater],
          exp)

let static_value v =
  match v with
  | Symbol s -> Csymbol_address (Linkage_name.to_string (Symbol.label s))
  | Tagged_immediate i ->
      Cint (Targetint.Ocaml.to_tagged_nativeint (Immediate.to_targetint i))
  | Dynamically_computed _ ->
      (* Needs to be a valid Ocaml value, but will be overwritten later *)
      Cint 1

let static_with_default f default v cont =
  match v with
  | Const c -> f c cont
  | Var _ -> f default cont

let transl_static_structure_item varmap r (symb, _kind, st_part) =
  match st_part with
  | Block (tag, _mut, vals) ->
      let fields = List.map static_value vals in
      let block =
        emit_block (block_header tag (List.length vals)) symb fields
      in
      let update_field i var =
        let id =
          try Variable.Map.find var varmap
          with Not_found ->
            Misc.fatal_errorf "Unbound variable %a in static structure"
              Variable.print var
        in
        let address = field_address (Cconst_symbol symb) i Debuginfo.none in
        Cop(Cstore (Word_val, Root_initialization),
            [address; Cvar id], Debuginfo.none)
      in
      let make_update i v =
        match v with
        | Const _ -> None
        | Var var -> update_field i var
      in
      let updates = List.mapi make_update vals in
      let expr =
        List.fold_left (fun acc up_opt ->
            match up_opt with
            | None -> acc
            | Some update -> Csequence (update, acc))
          r.expr
          updates
      in
      { r with expr; data = block @ r.data; }
  | Fabricated_block _ -> (* TODO *) assert false
  | Set_of_closures s ->
      { r with sets_of_closures = Symbol.Map.add symb s r.sets_of_closures }
      (* let fundecls = s.function_decls.funs in
       * let to_clambda_fundecl cid fundecl acc =
       *   let fundecl =
       *     { label = Closure_id.unique_name cid;
       *       arity = List.length fundecl.params;
       *     } (\* TODO: add dummy fields or refactor emit_constant_closure *\)
       *   in
       *   fundecl :: acc
       * in
       * let symb = (Linkage_name.to_string (Symbol.label symb), Global) in
       * let clambda_fundecls =
       *   Closure_id.Map.fold to_clambda_fundecl fundecls []
       * in
       * emit_constant_closure symb clambda_fundecls [] cont *)
  | Closure (set_symb, cid) ->
      { r with closures = Symbol.Map.add symb (set_symb, cid) r.closures }
  | Boxed_float v ->
      let f fl cont =
        emit_float_constant (symb, Global)
          (Numbers.Float_by_bit_pattern.to_float fl) cont
      in
      let data =
        static_with_default f Numbers.Float_by_bit_pattern.zero v r.data
      in
      { r with data }
  | Boxed_int32 ->
      let f i32 cont =
        emit_boxed_int32_constant (symb, Global) i32 cont
      in
      let data =
        static_with_default f 0l v r.data
      in
      { r with data }
  | Boxed_int64 ->
      let f i64 cont =
        emit_boxed_int64_constant (symb, Global) i64 cont
      in
      let data =
        static_with_default f 0L v r.data
      in
      { r with data }
  | Boxed_nativeint ->
      let f natint cont =
        emit_boxed_nativeint_constant (symb, Global)
          (Targetint.to_nativeint natint) cont
      in
      let data =
        static_with_default f Targetint.zero v r.data
      in
      { r with data }
  | Mutable_float_array { initial_value }
  | Immutable_float_array initial_value ->
      let fields =
        List.map
          (function Const f -> Numbers.Float_by_bit_pattern.to_float f
                  | Var _ -> 0.)
          initial_value
      in
      let data =
        emit_float_array_constant (symb, Global) fields r.data
      in
      { r with data }
  | Mutable_string { initial_value }
  | Immutable_string initial_value ->
      begin match initial_value with
      | Var v ->
          (* CR vlaviron: this doesn't make sense, strings
             can't be initialized without knowing their length *)
          assert false
      | Const str ->
          let data =
            emit_string_constant (symb, Global) str r.data
          in
          { r with data }

let transl_static_structure ids comp_values st_structure =
  let varmap =
    List.fold_left2
      (fun acc (v, _) id -> Variable.Map.add v id acc)
      Variable.Map.empty ids comp_values
  in
  List.fold_left (transl_static_structure_item varmap)
    empty_result st_structure

let rec transl_program_body env r body k =
  match body with
  | Define_symbol (def, rest)
  | Define_symbol_rec (def, rest) ->
    begin match def.computation with
    | Some comp ->
      let n, env1 = transl_cont env comp.return_cont in
      let nexn, env2 = transl_cont env1 comp.exception_cont in
      let exp = transl_expr env2 comp.expr in
      let rec mk_ids num =
        if num <= 0 then []
        else
          let id = Ident.create "retval" in
          id :: mk_ids (num - 1)
      in
      let ids = mk_ids (List.length comp.computed_values) in
      let r =
        transl_static_structure ids comp.computed_values def.static_structure
      in
      let expr = wrap_exn nexn (wrap_return n exp ids updater) in
      transl_program_body env r rest (fun res ->
          k { expr = Csequence (exp, res.expr);
              data = static @ res.data;
              functions = functions @ res.functions;
            })
    | None ->
      let r =
        transl_static_structure ids comp.computed_values def.static_structure
      in
      transl_program_body env r rest (fun res ->
          k { expr = res.expr;
              data = static @ res.data;
              functions = functions @ res.functions;
            })
    end
  | Root sym ->
    k { expr = Ctuple [];
        data = [];
        functions = [];
      }

let transl_program env prog =
  let r =
    { expr = Ctuple [];
      data = [];
      functions = [];
      sets_of_closures = Symbol.Map.empty;
      closures = Symbol.Map.empty;
    }
  in
  let res = transl_program_body env r prog.body (Ctuple []) in
  (* TODO *)res

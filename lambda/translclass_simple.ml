open Typedtree
open Lambda

let oo_prim = Translobj_simple.oo_prim

let mkappl (func, args) =
  Lapply {
    ap_loc=Loc_unknown;
    ap_func=func;
    ap_args=args;
    ap_tailcall=Default_tailcall;
    ap_inlined=Default_inline;
    ap_specialised=Default_specialise;
  }

type error = Tags of string * string

exception Error of Location.t * error

let lfield v i = Lprim(Pfield (i, Pointer, Mutable),
                       [Lvar v], Loc_unknown)

let transl_label l = Lconst (Const_immstring l)

let transl_meth_list lst =
  Lconst (Const_block (0, List.map (fun lab -> Const_immstring lab) lst))

let set_inst_var ~scopes self_id inst_var_id expr =
  Lprim(Psetfield_computed (Typeopt.maybe_pointer expr, Assignment),
    [Lvar self_id; Lvar inst_var_id; Translcore.transl_exp ~scopes expr], Loc_unknown)

(* let rec place_toplevel_lets ~scopes cl lam = *)
(*   match cl.cl_desc with *)
(*     Tcl_let (rec_flag, defs, _vals, cl') -> *)
(*       let lam = place_toplevel_lets ~scopes cl' lam in *)
(*       Translcore.transl_let ~scopes rec_flag defs lam *)
(*   | _ -> lam *)

let transl_val tbl create name =
  mkappl (oo_prim (if create then "new_variable" else "get_variable"),
          [Lvar tbl; transl_label name])

let transl_vals tbl create strict vals rem =
  List.fold_right
    (fun (name, id) rem ->
      Llet(strict, Pgenval, id, transl_val tbl create name, rem))
    vals rem

let meths_super tbl meths inh_meths =
  List.fold_right
    (fun (nm, id) rem ->
       try
         (nm, id,
          mkappl(oo_prim "get_method", [Lvar tbl; Lvar (Types.Meths.find nm meths)]))
         :: rem
       with Not_found -> rem)
    inh_meths []

let bind_super tbl (vals, meths) cl_init =
  transl_vals tbl false StrictOpt vals
    (List.fold_right (fun (_nm, id, def) rem ->
         Llet(StrictOpt, Pgenval, id, def, rem))
       meths cl_init)

let bind_methods tbl meths vals body =
  let methl = Types.Meths.fold (fun lab id tl -> (lab,id) :: tl) meths [] in
  let len = List.length methl and nvals = List.length vals in
  let ids = Ident.create_local "ids" in
  let i = ref (len + nvals) in
  Llet(Strict, Pgenval, ids,
       mkappl (oo_prim "new_methods_variables",
               [Lvar tbl;
                transl_meth_list (List.map fst methl);
                transl_meth_list (List.map fst vals)]),
       List.fold_right
         (fun (_lab,id) lam -> decr i; Llet(StrictOpt, Pgenval, id,
                                           lfield ids !i, lam))
         (methl @ vals) body)

let output_methods tbl methods lam =
  match methods with
    [] -> lam
  | [lab; code] ->
      Lsequence (mkappl(oo_prim "set_method", [Lvar tbl; lab; code]), lam)
  | _ ->
      Lsequence (mkappl(oo_prim "set_methods",
                        [Lvar tbl; Lprim(Pmakeblock(0,Immutable,None),
                                         methods, Loc_unknown)]),
                 lam)

let bind_id_as_val (id, _) = ("", id)

(* [build_class_init] returns two values:
   - [obj_init] is an expression that creates and initialises new objects.
     If the class takes parameters, it is a function that, given values for the
     parameters, performs the initialisations and (if needed) object creation.
     The [self_id] variable will be bound to either the integer 0, in which case
     [obj_init] must allocate the object and return it, or to an already allocated
     object, in which case [obj_init] will initialize the relevant parts of it
     throguh side-effects.
   - [class_init] is a wrapper that binds everything needed in the evaluation
     of [obj_init]

   Concretely, [class_init] is the code that runs at class creation time and
   [obj_init] is the code that runs at object creation time.
*)
type build_class_init_result =
  { obj_init : lambda;
    class_init : lambda -> lambda
  }

let create_object cl_table_id self_id init =
  let allocated_self_id = Ident.create_local "self" in
  let { obj_init; class_init } = init allocated_self_id in
  { class_init;
    obj_init =
      Llet(Strict, Pgenval, allocated_self_id,
           mkappl (oo_prim "create_object_opt", [Lvar self_id; Lvar cl_table_id]),
           (* [obj_init] has type unit since [allocated_self_id] will always
              be an allocated object *)
           Lsequence
             (obj_init,
              (* [run_initializers_opt] only runs initializers if the first
                 parameter (the original [self_id]) was not already allocated.
                 This ensures that initializers are run exactly once for each
                 object.
                 If it does run the initializers, it returns the object.
                 Otherwise, it should return unit (the current implementation
                 returns the object too, but the invariants around [obj_init]
                 ensure that it is never used). *)
              mkappl (oo_prim "run_initializers_opt",
                      [Lvar self_id; Lvar allocated_self_id; Lvar cl_table_id])))
  }

let name_pattern default p =
  match p.pat_desc with
  | Tpat_var (id, _, _) -> id
  | Tpat_alias(_, id, _, _) -> id
  | _ -> Ident.create_local default

let rec build_class_init ~scopes cl_table must_constrain super free_ids_with_defs self_id cl =
  match cl.cl_desc with
  | Tcl_ident (path, _, _) ->
      (* The object initialiser for the class in [path], specialised
         to the class being defined *)
      let obj_init_id = Ident.create_local "class_init" in
      let loc = Debuginfo.Scoped_location.of_location ~scopes cl.cl_loc in
      let path_lam = transl_class_path loc cl.cl_env path in
      let class_init body =
        let class_init_expr =
          Lprim(Pfield (1, Pointer, Mutable), [path_lam], Loc_unknown)
        in
        let env_expr =
          Lprim(Pfield (3, Pointer, Mutable), [path_lam], Loc_unknown)
        in
        Llet (Strict, Pgenval, obj_init_id,
              (* Load the [class_init] field of the class,
                 and apply it to our current table and the class' environment.
                 This gets us the object initialiser. *)
              mkappl(class_init_expr, [Lvar cl_table; env_expr]),
              (* Bind the method and instance variable indices *)
              bind_super cl_table super body)
      in
      (* The object initialiser is passed the current object and will
         either allocate it or update it in place, as appropriate. *)
      let obj_init = mkappl(Lvar obj_init_id, [Lvar self_id]) in
      { obj_init; class_init }
  | Tcl_structure str ->
      create_object cl_table self_id (fun allocated_self_id ->
          let init_acc =
            { obj_init = Lvar allocated_self_id;
              class_init = Fun.id
            },
            [],
            []
          in
          let { obj_init; class_init }, unset_methods, instance_variables =
            List.fold_right
              (fun field ({ obj_init; class_init }, unset_methods, instance_variables) ->
                 match field.cf_desc with
                 | Tcf_inherit (_, cl, _, vals, meths) ->
                     let { obj_init = obj_init_inh; class_init = class_init_inh } =
                       let super =
                         (vals, meths_super cl_table str.cstr_meths meths)
                       in
                       build_class_init ~scopes cl_table true super [] allocated_self_id cl
                     in
                     let obj_init =
                       Lsequence (obj_init_inh, obj_init)
                     in
                     let class_init body =
                       class_init_inh
                         (output_methods cl_table unset_methods
                            (class_init body))
                     in
                     { obj_init; class_init }, [], instance_variables
                 | Tcf_val (name, _, id, def, over) ->
                     let obj_init =
                       match def with
                       | Tcfk_concrete (_, exp) ->
                           Lsequence (set_inst_var ~scopes allocated_self_id id exp,
                                      obj_init)
                       | _ -> obj_init
                     in
                     let instance_variables =
                       if over
                       then instance_variables
                       else (name.txt, id) :: instance_variables
                     in
                     { obj_init; class_init }, unset_methods, instance_variables
                 | Tcf_method (_, _, Tcfk_virtual _) ->
                     { obj_init; class_init }, unset_methods, instance_variables
                 | Tcf_method (name, _, Tcfk_concrete (_, exp)) ->
                     let scopes = Debuginfo.Scoped_location.enter_method_definition ~scopes name.txt in
                     let met_code = Translcore.transl_scoped_exp ~scopes exp in
                     let met_code =
                       (* Force correct naming of method for profiles *)
                       let met = Ident.create_local ("method_" ^ name.txt) in
                       Llet(Strict, Pgenval, met, met_code, Lvar met)
                     in
                     let unset_methods =
                       Lvar(Types.Meths.find name.txt str.cstr_meths)
                       :: met_code
                       :: unset_methods
                     in
                     { obj_init; class_init }, unset_methods, instance_variables
                 | Tcf_constraint _ | Tcf_attribute _ ->
                     { obj_init; class_init }, unset_methods, instance_variables
                 | Tcf_initializer exp ->
                     let class_init body =
                       Lsequence
                         (mkappl (oo_prim "add_initializer",
                                  [Lvar cl_table; Translcore.transl_exp ~scopes exp]),
                          body)
                     in
                     { obj_init; class_init }, unset_methods, instance_variables
              ) str.cstr_fields init_acc
          in
          (* Lifused optimisation *)
          let obj_init =
            List.fold_right
              (fun (id, expr) obj_init ->
                 begin match expr.exp_desc with
                 | Texp_ident _ -> ()
                 | _ -> Misc.fatal_error "effectful expression in binding for free variable"
                 end;
                 Lsequence (Lifused (id, set_inst_var ~scopes allocated_self_id id expr),
                            obj_init))
              free_ids_with_defs obj_init
          in
          let class_init body =
            bind_methods cl_table str.cstr_meths instance_variables
              (output_methods cl_table unset_methods
                 (class_init body))
          in
          { obj_init; class_init }
        )
  | Tcl_fun (_, pat, vals, cl, partial) ->
      let { obj_init; class_init } =
        build_class_init ~scopes cl_table must_constrain super (vals @ free_ids_with_defs) self_id cl
      in
      let obj_init =
        let build params body =
          let param = name_pattern "param" pat in
          Lambda.lfunction
            ~kind:Curried ~params:((param, Pgenval)::params)
            ~return:Pgenval
            ~attr:default_function_attribute
            ~loc:(Debuginfo.Scoped_location.of_location ~scopes pat.pat_loc)
            ~body:(Matching.for_function ~scopes pat.pat_loc
                     None (Lvar param) [pat, body] partial)
        in
        match obj_init with
        | Lfunction { kind = Curried; params; body } -> build params body
        | body                                           -> build [] body
      in
      let vals = List.map bind_id_as_val vals in
      let class_init body =
        transl_vals cl_table true StrictOpt vals
          (class_init body)
      in
      { obj_init; class_init }
  | Tcl_apply (cl, oexprs) ->
      let { obj_init; class_init } =
        build_class_init ~scopes cl_table must_constrain super free_ids_with_defs self_id cl
      in
      let obj_init =
        Translcore.transl_apply ~scopes obj_init oexprs Loc_unknown
      in
      { obj_init; class_init }
  | Tcl_let (rec_flag, defs, vals, cl) ->
      let { obj_init; class_init } =
        build_class_init ~scopes cl_table must_constrain super (vals @ free_ids_with_defs) self_id cl
      in
      let obj_init =
        Translcore.transl_let ~scopes rec_flag defs obj_init
      in
      let vals = List.map bind_id_as_val vals in
      let class_init body =
        transl_vals cl_table true StrictOpt vals
          (class_init body)
      in
      { obj_init; class_init }
  | Tcl_open (_, cl) ->
      build_class_init ~scopes cl_table must_constrain super free_ids_with_defs self_id cl
  | Tcl_constraint (cl, _, vals, meths, concr_meths) ->
      let { obj_init; class_init } =
        build_class_init ~scopes cl_table false super free_ids_with_defs self_id cl
      in
      let virt_meths =
        List.filter (fun lab -> not (Types.MethSet.mem lab concr_meths)) meths in
      let concr_meths = Types.MethSet.elements concr_meths in
      let narrow_args =
        [ Lvar cl_table;
          transl_meth_list vals;
          transl_meth_list virt_meths;
          transl_meth_list concr_meths ]
      in
      let class_init =
        if must_constrain then
          (fun body ->
             Lsequence
               (mkappl (oo_prim "narrow", narrow_args),
                class_init
                  (Lsequence
                     (mkappl (oo_prim "widen", [Lvar cl_table]),
                      body))))
        else
          class_init
      in
      { obj_init; class_init }

let transl_class ~scopes rec_ids cl_id pub_meths cl vflag =
  let scopes = Debuginfo.Scoped_location.enter_class_definition ~scopes cl_id in
  (* Sort methods by hash *)
  let pub_meths =
    List.sort
      (fun s s' -> compare (Btype.hash_variant s) (Btype.hash_variant s'))
      pub_meths in
  (* Check for hash conflicts *)
  let tags = List.map Btype.hash_variant pub_meths in
  let rev_map = List.combine tags pub_meths in
  List.iter2
    (fun tag name ->
      let name' = List.assoc tag rev_map in
      if name' <> name then raise(Error(cl.cl_loc, Tags(name, name'))))
    tags pub_meths;
  (* Build the class *)
  let class_init_id = Ident.create_local (Ident.name cl_id ^ "_init") in
  let table_id = Ident.create_local "table" in
  let self_id = Ident.create_local "self" in
  let { obj_init; class_init } =
    build_class_init ~scopes table_id false ([],[]) [] self_id cl
  in
  let obj_init_func =
    (Lambda.lfunction
       ~kind:Curried
       ~attr:default_function_attribute
       ~loc:Loc_unknown
       ~return:Pgenval
       ~params:[self_id, Pgenval]
       ~body:obj_init)
  in
  let class_init_func =
    let unused_env_id = Ident.create_local "unused" in
    Lambda.lfunction
      ~kind:Curried
      ~attr:default_function_attribute
      ~loc:Loc_unknown
      ~return:Pgenval
      ~params:[(table_id, Pgenval); (unused_env_id, Pgenval)]
      ~body:(class_init obj_init_func)
  in
  let fv = free_variables class_init_func in
  let recursive = List.exists (fun id -> Ident.Set.mem id fv) rec_ids in
  let class_allocation, rec_kind =
    match (vflag : Asttypes.virtual_flag) with
    | Virtual ->
      (* Virtual classes only need to provide the [class_init] and [env]
         fields. [obj_init] and [env_init] are filled with dummy
         [lambda_unit] values. *)
        Lprim(Pmakeblock(0, Immutable, None),
              [lambda_unit (* dummy *);
               class_init_func;
               lambda_unit (* dummy *);
               lambda_unit],
              Loc_unknown),
        Value_rec_types.Static
    | Concrete ->
      if not recursive then
        (* Not recursive: can use make_class directly *)
        mkappl (oo_prim "make_class",[transl_meth_list pub_meths;
                                      Lvar class_init_id]),
        Value_rec_types.Dynamic
      else
        (* Recursive: need to have an actual allocation for let rec compilation
           to work, so hardcode make_class *)
        let table_id = Ident.create_local "table" in
        let bind_table_id body =
          Llet(Strict, Pgenval, table_id,
               mkappl (oo_prim "create_table", [transl_meth_list pub_meths]),
               body)
        in
        let env_init_id = Ident.create_local "env_init" in
        let bind_env_init_id body =
          Llet(Strict, Pgenval, env_init_id,
               mkappl (Lvar class_init_id, [Lvar table_id]),
               body)
        in
        let call_init_class body =
          Lsequence(
            mkappl (oo_prim "init_class", [Lvar table_id]),
            body)
        in
        let class_block =
          Lprim(Pmakeblock(0, Immutable, None),
                [mkappl (Lvar env_init_id, [lambda_unit]);
                 Lvar class_init_id;
                 Lvar env_init_id;
                 lambda_unit],
                Loc_unknown)
        in
        bind_table_id (bind_env_init_id (call_init_class class_block)),
        Value_rec_types.Static
  in
  Llet(Strict, Pgenval, class_init_id, class_init_func, class_allocation),
  rec_kind

let () =
  if Translobj.simple_version then
    Translcore.transl_object :=
      (fun ~scopes id meths cl ->
         let class_expr, _rkind = transl_class ~scopes [] id meths cl Asttypes.Concrete in
         let obj_init_expr =
           Lprim(Pfield (0, Pointer, Mutable), [class_expr], Loc_unknown)
         in
         mkappl (obj_init_expr, [lambda_unit]))

open Format
module Style = Misc.Style

let report_error ppf = function
  | Tags (lab1, lab2) ->
      fprintf ppf "Method labels %a and %a are incompatible.@ %s"
        Style.inline_code lab1
        Style.inline_code lab2
        "Change one of them."

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )

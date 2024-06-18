(*
   # Translation of class and object expressions

   ## Objects

   ### Memory layout

   Objects are represented in memory using two layers:
   - The outer layer is a block with tag [Obj.object_tag].
     It has a first field pointing to the inner layer (the methods),
     a second field acting as a unique identifier to allow
     polymorphic comparison, and the rest of the block contains
     the values of the instance variables, class parameters, and
     other values that can vary between two objects of the same class.
   - The inner layer is a regular block (with tag zero). It contains
     all values that are shared between all objects of the same classes,
     which means mostly methods. The first field corresponds to the number of
     public methods, the second field is a mask used for optimising method
     access (not relevant for this file), the following fields are
     alternating between the methods themselves and the hash of their name
     (sorted in increasing hash order). Additional fields are used for
     private methods (and other values, but irrelevant for this file).

   ### Primitives

   Method access is compiled in one of three possible ways:
   - Generic method access (outside a class, or to an object that is not
     self or an ancestor) uses dynamic lookup. A dichotomic search in
     the part of the method array that stores public methods finds
     the expected method and calls it on the current object.
   - Method access through the self object inside a class:
     the (runtime) position of the method inside the inner layer block
     has been computed at class creation time, so the method is fetched
     from the block through a dynamic block load (like an array load)
   - Accessing the method of an ancestor inside a class (ancestors are
     variables bound by [inherit ... as ancestor] constructions):
     at class creation time, the ancestor method is bound to a variable,
     and the method call just calls this function without any (further)
     dynamic lookup.

   Instance variable access (getting and setting) also computes offsets
   at class initialisation time, with those offsets used to index directly
   in the outer layer of the object.

   There are no other object primitives (objects cannot be allocated
   in the IR directly, they are allocated in [CamlinternalOO])

   ## Classes

   Classes are stored as module fields. The runtime value that represents
   classes is used in two contexts:

   - When using the [new] construction, to generate an object from a class.
   - When referencing a class inside another class (either through
     inheritance or other class expressions).

   This is done by storing classes as blocks where the first field
   is used to generate objects, and the second field is used to derive
   classes (in a general sense, not only for inheritance).
   In practice classes also contain one other field, which is used to
   implement some optimisations in the main compiler (to ensure that each
   class only runs its initialisation code once in the whole program, even
   if its definition is in a context that is expected to be run several
   times like a functor). This file does not implement or document this
   optimisation, but uses a compatible layout to allow mixing code compiled
   by both backends.
   So the block layout is the following:
   - A field named [obj_init] that is used for creating objects
   - A field named [class_init] that is used for deriving classes
   - A field named [env] containing values for all the variables
     captured by [Translobj.oo_wrap] calls (unused in this implementation).

   The module [CamlinternalOO] also defines a type [table] that represents
   class layouts. Such values are not stored in the class block directly,
   but the [obj_init] field captures the table for the class and [class_init]
   manipulates such tables.

   ### The [obj_init] field

   As described earlier, each object contains an inner layer that is computed
   only once at class initialisation time; it seems natural to store this
   block in the runtime value of the class (this block is one of the fields of
   the [CamlinternalOO.table] type). However, given that creating an
   object also involves setting up the instance variables and running the
   initialisers, in practice the class only exports a function that creates
   objects, and the table is captured in this function's closure along with
   any value necessary to properly initialise the object.
   Classes can have parameters, so in practice this object creation function
   takes a first unit parameter (to ensure that it is always a function)
   and returns a regular OCaml value that is either an object (if the class
   doesn't have parameters) or a function which, given values
   for the class parameters, will return an object.

   Here is the type of the [obj_init] function for a class which type is
   [p1 -> ... -> pn -> object method m1 : t1 ... method mn : tn end]:
   [unit -> p1 -> ... -> pn -> < m1 : t1; ... mn : tn >]
   (If the class has instance variables or initialisers, they are not
   reflected in the type of [obj_init]).

   ### The [class_init] field

   This field is used in two cases:
   - When a class is defined in terms of another class, for instance as an
     alias, a partial application, or some other kind of wrapper.
   - When a class structure (i.e. the [object ... end] syntactic construction)
     contains inheritance fields (e.g. [inherit cl as super]).

   In both cases, we only have access to the other class' public type at
   compile time, but we must still make sure all of the private fields
   are setup correctly, in a way that is compatible with the current
   class.

   This is where tables come into play: the [class_init] field is a function
   taking a table as parameter, updates it in-place, and returns a function
   that is very similar to the [obj_init] function, except that instead of
   taking [unit] as its first parameter and returning an object, it takes
   a partially initialised object, and updates the parts of it that are
   relevant for the corresponding class.

   So the type of [class_init] is:
   [table -> env -> Obj.t -> p1 -> ... -> pn -> unit]
   The [env] parameter can be ignored for now; it is always of type [unit]
   in this version of the algorithm. The full algorithm uses it to allow
   separating variable and invariant parts of the class initialisation code,
   making class construction cheaper under functors. By keeping this parameter,
   we allow classes compiled with this algorithm and the full one to be
   compatible, making it possible to link together files compiled with
   different versions of the algorithm.

   ### Compilation scheme

   The algorithm implemented below aims at sharing code as much as possible
   between the various similar parts of the class.

   - The code of the [obj_init] function is very similar to the code of
     the function returned by [class_init]. The main difference is that
     [obj_init] starts from scratch, allocating then initialising the object,
     while inside [class_init] we want to run initialisation code on an already
     allocated object (that we don't need to return).
     So in practice we will build a single function that, depending on the value
     of its first parameter, will either do the allocation and return the object
     (if the parameter is the integer constant 0), or assume the parameter is
     an already allocated and update it.
     The body of this function is the [obj_init] field of the record returned by
     [build_class_init].
   - The table for the current class (that [obj_init] will read from) is
     computed by allocating a basic table, then passing it to [class_init],
     and finally calling [CamlinternalOO.init_class] on it.
     This means that all the code for setting up the class (computing instance
     variable indices, calling inherited class initialisers, and so on) is only
     generated once, in the [class_init] function.
     Due to the order in which we traverse the class expression,
     [build_class_init] returns in its [class_init] field a wrapper to put
     around the [obj_init] function instead of returning the full expression
     directly. This differs from the original algorithm, which first builds
     [obj_init] and then builds [class_init] on top of it, but has the advantage
     of requiring only one traversal of the class expression and making it
     easier to synchronize the two pieces of code.

   That's all for the high-level algorithm; the rest will be detailed close to
   the corresponding code.

*)

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

let transl_val tbl create name =
  mkappl (oo_prim (if create then "new_variable" else "get_variable"),
          [Lvar tbl; transl_label name])

let transl_vals tbl create strict vals rem =
  List.fold_right
    (fun (name, id) rem ->
      Llet(strict, Pgenval, id, transl_val tbl create name, rem))
    vals rem

type inheritance_status =
  | Normal (** Not under an [inherit] construct *)
  | Inheriting of {
      must_narrow : bool;
      (** [false] if we already went through a call to [narrow] *)
      method_getters : (Ident.t * lambda) list;
      (** Ancestor methods are accessed through identifiers.
          These identifiers are bound at class initialisation time,
          by fetching the actual closures from the table just
          after setting up the inherited class. *)
      instance_vars : (string * Ident.t) list;
      (** Inherited instance variables need to have their index bound
          in the scope of the child class *)
    }

let meths_super tbl meths inh_meths =
  List.fold_right
    (fun (nm, id) rem ->
       try
         (id,
          mkappl(oo_prim "get_method",
                 [Lvar tbl; Lvar (Types.Meths.find nm meths)]))
         :: rem
       with Not_found -> assert false)
    inh_meths []

let bind_super tbl super_opt cl_init =
  match super_opt with
  | Normal -> cl_init
  | Inheriting { must_narrow = _; method_getters; instance_vars } ->
    transl_vals tbl false StrictOpt instance_vars
      (List.fold_right (fun (id, def) rem ->
           Llet(StrictOpt, Pgenval, id, def, rem))
         method_getters cl_init)

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

let output_method tbl lab code lam =
  Lsequence (mkappl(oo_prim "set_method", [Lvar tbl; lab; code]), lam)

let bind_id_as_val (id, _) = ("", id)

let name_pattern default p =
  match p.pat_desc with
  | Tpat_var (id, _, _) -> id
  | Tpat_alias(_, id, _, _) -> id
  | _ -> Ident.create_local default

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

(** Build the class and object initialisation code.
    Parameters:
    - [scopes] corresponds to the location scopes (as in the rest of the translation code)
    - [cl_table] is the variable to which the table for the current class is bound
    - [must_narrow] is [false] when called from outside, but [true] when called
      from an [inherit] field. Narrowing is necessary during inheritance to prevent
      clashes between methods/variables in the child class and private methods/variables
      in the parent.
    - [super] stores, if we're building an inherited class, the variables and methods
      exposed to the child. The variables need to have their associated index exposed,
      and methods have to be bound in case the child refers to them through the ancestor
      variables.
    - [free_ids_with_defs] stores the anonymous instance variables associated with all variables
      that occur inside the class definition but outside the [object ... end] structure:
      class parameters and class let bindings. The definition is always the identifier
      corresponding to the original variable.
    - [self_id] is the parameter of the [obj_init] function we want to create. As explained
      above at runtime it might point to either an already allocated object, when inheriting,
      or a dummy zero value, when calling [new].
    - [cl] is the class we're compiling *)
let rec build_class_init ~scopes cl_table super free_ids_with_defs self_id cl =
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
          Lprim(Pfield (2, Pointer, Mutable), [path_lam], Loc_unknown)
        in
        Llet (Strict, Pgenval, obj_init_id,
              (* Load the [class_init] field of the class,
                 and apply it to our current table and the class' environment.
                 This gets us the object initialiser. *)
              mkappl(class_init_expr, [Lvar cl_table; env_expr]),
              (* The methods and variables for this class are fully registered
                 in the table. If we are in an inheritance context, we can now
                 bind everything. *)
              bind_super cl_table super body)
      in
      (* The object initialiser is passed the current object and will
         either allocate it or update it in place, as appropriate. *)
      let obj_init = mkappl(Lvar obj_init_id, [Lvar self_id]) in
      (* Note: we don't need to bind [free_ids_with_defs] here, as they are
         only used in structures. Outside structures (in class lets or
         applications) we use the regular identifiers. *)
      { obj_init; class_init }
  | Tcl_structure str ->
      (* Initialising a concrete class structure *)
      (* [allocated_self_id] will be bound to the allocated object,
         unlike [self_id] which might be zero if called directly from
         an object creation expression. *)
      let allocated_self_id = Ident.create_local "self" in
      let init_acc =
        (* The empty class simply returns the allocated object, with no
           extra wrapper needed. *)
        { obj_init = Lvar allocated_self_id;
          class_init = Fun.id
        },
        []
      in
      let { obj_init; class_init }, instance_variables =
        List.fold_right
          (fun field ({ obj_init; class_init }, instance_variables) ->
             match field.cf_desc with
             | Tcf_inherit (_, cl, _, vals, meths) ->
               let { obj_init = obj_init_inh; class_init = class_init_inh } =
                 (* Build the initialisation code for the inherited class,
                    plus its wrappers.
                    Make sure the wrappers bind the inherited methods
                    and variables. *)
                 let super =
                   Inheriting {
                     must_narrow = true;
                     method_getters = meths_super cl_table str.cstr_meths meths;
                     instance_vars = vals
                   }
                 in
                 (* Reset [free_ids_with_defs]. The current ones will be bound
                    outside the structure. *)
                 build_class_init ~scopes cl_table super [] allocated_self_id cl
               in
               let obj_init =
                 (* Since [allocated_self_id] is bound to a concrete object,
                    only the side-effects of [obj_init_inh] are relevant. *)
                 Lsequence (obj_init_inh, obj_init)
               in
               let class_init body =
                 class_init_inh (class_init body)
               in
               (* Note: instance variables inherited from the class are bound by
                  [bind_super] in [class_init_inh] *)
               { obj_init; class_init }, instance_variables
             | Tcf_val (name, _, id, def, over) ->
                 let obj_init =
                   match def with
                   | Tcfk_concrete (_, exp) ->
                       Lsequence (set_inst_var ~scopes allocated_self_id id exp,
                                  obj_init)
                   | _ -> obj_init
                 in
                 let instance_variables =
                   (* If this is an override, the variable is the same as
                      the one from the earlier definition, and must not be
                      bound again. *)
                   if over
                   then instance_variables
                   else (name.txt, id) :: instance_variables
                 in
                 { obj_init; class_init }, instance_variables
             | Tcf_method (_, _, Tcfk_virtual _) ->
                 { obj_init; class_init }, instance_variables
             | Tcf_method (name, _, Tcfk_concrete (_, exp)) ->
                 let scopes = Debuginfo.Scoped_location.enter_method_definition ~scopes name.txt in
                 let met_code = Translcore.transl_scoped_exp ~scopes exp in
                 let met_code =
                   (* Force correct naming of method for profiles *)
                   let met = Ident.create_local ("method_" ^ name.txt) in
                   Llet(Strict, Pgenval, met, met_code, Lvar met)
                 in
                 let class_init body =
                   output_method cl_table
                     (Lvar(Types.Meths.find name.txt str.cstr_meths))
                     met_code
                     (class_init body)
                 in
                 { obj_init; class_init }, instance_variables
             | Tcf_constraint _ | Tcf_attribute _ ->
                 { obj_init; class_init }, instance_variables
             | Tcf_initializer exp ->
                 let class_init body =
                   Lsequence
                     (mkappl (oo_prim "add_initializer",
                              [Lvar cl_table; Translcore.transl_exp ~scopes exp]),
                      body)
                 in
                 { obj_init; class_init }, instance_variables
          ) str.cstr_fields init_acc
      in
      (* Set the instance variables associated to the class parameters and
         let bindings to their expected value. *)
      let obj_init =
        List.fold_right
          (fun (id, expr) obj_init ->
             begin match expr.exp_desc with
             | Texp_ident _ -> ()
             | _ -> Misc.fatal_error "effectful expression in binding for free variable"
             end;
             (* Note on Lifused: the instance variable index bound to [id] is
                only used inside methods and initializers. So any occurrences
                of [id] must be in the class initialization part, and [obj_init]
                itself will never use it directly. This is why the
                simplification of Lifused only looks for uses above the
                expression, and not after. *)
             Lsequence (Lifused (id, set_inst_var ~scopes allocated_self_id id expr),
                        obj_init))
          free_ids_with_defs obj_init
      in
      let obj_init =
        Llet(Strict, Pgenval, allocated_self_id,
             mkappl (oo_prim "create_object_opt", [Lvar self_id; Lvar cl_table]),
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
                        [Lvar self_id; Lvar allocated_self_id; Lvar cl_table])))
      in
      let class_init body =
        (* In order of execution at runtime:
           - Bind the method and variable indices for the current class
           - Run the code for setting up the individual fields
           - If we are in an inheritance context, bind the inherited variables
             and methods for use in the child *)
        bind_methods cl_table str.cstr_meths instance_variables
          (class_init (bind_super cl_table super body))
      in
      { obj_init; class_init }
  | Tcl_fun (_, pat, vals, cl, partial) ->
      let { obj_init; class_init } =
        (* [vals] maps all pattern variables to idents for use inside methods *)
        build_class_init ~scopes cl_table super (vals @ free_ids_with_defs) self_id cl
      in
      let obj_init =
        (* Build a function *)
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
      let class_init body =
        (* Create anonymous instance variables and define them in the table *)
        let vals = List.map bind_id_as_val vals in
        transl_vals cl_table true StrictOpt vals
          (class_init body)
      in
      { obj_init; class_init }
  | Tcl_apply (cl, oexprs) ->
      let { obj_init; class_init } =
        build_class_init ~scopes cl_table super free_ids_with_defs self_id cl
      in
      let obj_init =
        Translcore.transl_apply ~scopes obj_init oexprs Loc_unknown
      in
      { obj_init; class_init }
  | Tcl_let (rec_flag, defs, vals, cl) ->
      (* See comment on the [Tcl_fun] case for the meaning of [vals] *)
      let { obj_init; class_init } =
        build_class_init ~scopes cl_table super (vals @ free_ids_with_defs) self_id cl
      in
      let obj_init =
        Translcore.transl_let ~scopes rec_flag defs obj_init
      in
      let class_init body =
        let vals = List.map bind_id_as_val vals in
        transl_vals cl_table true StrictOpt vals
          (class_init body)
      in
      { obj_init; class_init }
  | Tcl_open (_, cl) ->
      (* Class local opens are restricted to paths only, so no code is generated *)
      build_class_init ~scopes cl_table super free_ids_with_defs self_id cl
  | Tcl_constraint (cl, _, vals, meths, concr_meths) ->
      (* Skip narrowing if we're not directly under [inherit] *)
      let must_narrow, super =
        match super with
        | Normal -> false, super
        | Inheriting inh ->
          (* No need to narrow further *)
          inh.must_narrow, Inheriting { inh with must_narrow = false }
      in
      let { obj_init; class_init } =
        build_class_init ~scopes cl_table super free_ids_with_defs self_id cl
      in
      (* Note: we should be passing only [meths], as [narrow] doesn't need
         to know about wirtual methods anymore *)
      let virt_meths =
        List.filter (fun lab -> not (Types.MethSet.mem lab concr_meths)) meths
      in
      let concr_meths = Types.MethSet.elements concr_meths in
      let narrow_args =
        [ Lvar cl_table;
          transl_meth_list vals;
          transl_meth_list virt_meths;
          transl_meth_list concr_meths ]
      in
      let class_init =
        if must_narrow then
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
  (* The manual specifies that toplevel lets *must* be evaluated outside of the class *)
  let rec extract_toplevel_lets wrapper cl =
    match cl.cl_desc with
    | Tcl_let (rec_flag, defs, _vals, cl) ->
        extract_toplevel_lets
          (fun lam -> wrapper (Translcore.transl_let ~scopes rec_flag defs lam))
          cl
    | Tcl_open (_, cl) -> extract_toplevel_lets wrapper cl
    | _ -> wrapper, cl
  in
  let place_toplevel_lets, cl = extract_toplevel_lets (fun lam -> lam) cl in
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
    build_class_init ~scopes table_id Normal [] self_id cl
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
  (* The toplevel lets are allowed to refer to the classes being defined,
     so we need to insert them before the free variables computation. *)
  let class_init_func = place_toplevel_lets class_init_func in
  let fv = free_variables class_init_func in
  let recursive = List.exists (fun id -> Ident.Set.mem id fv) rec_ids in
  let class_allocation, rec_kind =
    match (vflag : Asttypes.virtual_flag) with
    | Virtual ->
      (* Virtual classes only need to provide the [class_init] and [env]
         fields. [obj_init] is filled with a dummy [lambda_unit] value. *)
        Lprim(Pmakeblock(0, Immutable, None),
              [lambda_unit (* dummy *);
               class_init_func;
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
                 lambda_unit],
                Loc_unknown)
        in
        bind_table_id (bind_env_init_id (call_init_class class_block)),
        Value_rec_types.Static
  in
  Llet(Strict, Pgenval, class_init_id, class_init_func, class_allocation), rec_kind

let () =
  if Translobj.simple_version then
    Translcore.transl_object :=
      (fun ~scopes id meths cl ->
         let class_expr, _rkind = transl_class ~scopes [] id meths cl Asttypes.Concrete in
         let obj_init_expr =
           Lprim(Pfield (0, Pointer, Mutable), [class_expr], Loc_unknown)
         in
         mkappl (obj_init_expr, [lambda_unit]))

open Format_doc
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

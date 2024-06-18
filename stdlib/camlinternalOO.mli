(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Run-time support for objects and classes.
    All functions in this module are for system use only, not for the
    casual user. *)

(** {1 Classes} *)

(** Compressed representation of a method name (hash) *)
type tag

(** Used inconsistently. Alias for [int], used for indexing either into
    the method array or the object itself. *)
type label

(** The type representing the runtime layout of all the elements of a class.
    Updated in place during class initialisation, then frozen (finalised)
    before being used to create new objects. *)
type table

(** A method (a regular closure) *)
type meth

(** This should be [Obj.t] -- it is used for things that cannot be
    statically typed *)
type t

(** The type for objects (used for unrelated things in a few places) *)
type obj

(** Closures (redundant with [meth] above) *)
type closure

(* 00 *) val public_method_label : string -> tag
(* 01 *) val new_method : table -> label

(** [new_variable table var_name] returns the index into the object where
    the variable with name [var_name] will be stored.
    If [var_name] is not empty, it will reuse the index for a previously defined
    variable of the same name, otherwise it will always create a new slot. *)
(* 02 - used *) val new_variable : table -> string -> int

(** [new_methods_variables table methods variables] returns two arrays
    concatenated into a single one for efficiency.
    The first array contains, for each method in [methods], the index at
    which it can be found in the method array of the class. If the methods are
    not already present in the object (private methods), new indicces are generated
    after the indicces for public methods.
    The second array contains, for each instance variable in [variables],
    the index at which it can be found in objects of this class. The variables
    will be registered for the class using [new_variable]. *)
(* 03 - used *) val new_methods_variables :
    table -> string array -> string array -> label array

(** [get_variable table var_name] returns the index into the object for a given
    instance variable. The variable must have been previously added to the
    class table with [new_variable]. *)
(* 04 - used *) val get_variable : table -> string -> int

(* 05 *) val get_variables : table -> string array -> int array
(* 06 *) val get_method_label : table -> string -> label
(* 07 *) val get_method_labels : table -> string array -> label array

(** [get_method table label] returns the closure associated with the
    method present at the index represented by [label]. *)
(* 08 - used *) val get_method : table -> label -> meth

(** [set_method table label clos] stores the closure [clos] at the index
    represented by [label]. *)
(* 09 - used *) val set_method : table -> label -> meth -> unit

(* 10 *) val set_methods : table -> label array -> unit

(** [narrow table vars virt_methods concr_methods] restricts the methods and
    variables accessible by name to the given values. This is used for
    inheritance, and allows the parent and child classes to both define
    private methods or variables with the same name without conflicts. *)
(* 11 - used *) val narrow : table -> string array -> string array -> string array -> unit

(** [widen table] restores the set of available methods and variables to the
    state before the last call to [narrow]. *)
(* 12 - used *) val widen : table -> unit

(** [add_initializer table init] adds [init] to the functions that will be run
    whenever an object is created. Functions are called in the order in which
    they are registered. *)
(* 13 - used *) val add_initializer : table -> (obj -> unit) -> unit

(** Unused even in the original implementation *)
(* 14 *) val dummy_table : table

(** [create_table pub_meths] allocates a fresh table with the given set of
    public methods. *)
(* 15 - used *) val create_table : string array -> table

(** [init_class table] is called when the table has been fully computed.
    It finalises the relevant structures to make it possible to create objects
    from the table. *)
(* 16 - used *) val init_class : table -> unit

(* 17 *) val inherits :
    table -> string array -> string array -> string array ->
    (t * (table -> obj -> Obj.t) * obj) -> bool -> Obj.t array

(** [make_class pub_meths class_init] creates the runtime representation of a
    class from the given public methods. [class_init] contains the code to
    setup the table. *)
(* 18 - used *) val make_class :
    string array -> (table -> Obj.t -> t) ->
    (t * (table -> Obj.t -> t) * Obj.t)


type init_table
(* 19 *) val make_class_store :
    string array -> (table -> t) -> init_table -> unit

(** *)
(* 20 - used *) val dummy_class :
    string * int * int ->
    (t * (table -> Obj.t -> t) * Obj.t)

(** {1 Objects} *)

(* 21 *) val copy : (< .. > as 'a) -> 'a
(* 22 *) val create_object : table -> obj

(** [create_object_opt obj_opt table] takes an unboxed option [obj_opt],
    and if it is [None] allocates a new object from [table], otherwise
    returns the object. *)
(* 23 - used *) val create_object_opt : obj -> table -> obj

(* 24 *) val run_initializers : obj -> table -> unit

(** [run_initializers_opt initial_obj_opt new_obj table] looks at its
    first parameter [initial_obj_opt], and if it is [None] it runs the
    initializers defined in [table]. In all cases it returns [new_obj]. *)
(* 25 - used *) val run_initializers_opt : obj -> obj -> table -> obj
(* 26 *) val create_object_and_run_initializers : obj -> table -> obj
external send : obj -> tag -> t = "%send"
external sendcache : obj -> tag -> t -> int -> t = "%sendcache"
external sendself : obj -> label -> t = "%sendself"
external get_public_method : obj -> tag -> closure
    = "caml_get_public_method" [@@noalloc]

(** {1 Table cache} *)

type tables
(* 27 *) val lookup_tables : tables -> closure array -> tables

(** {1 Builtins to reduce code size} *)

(*
val get_const : t -> closure
val get_var : int -> closure
val get_env : int -> int -> closure
val get_meth : label -> closure
val set_var : int -> closure
val app_const : (t -> t) -> t -> closure
val app_var : (t -> t) -> int -> closure
val app_env : (t -> t) -> int -> int -> closure
val app_meth : (t -> t) -> label -> closure
val app_const_const : (t -> t -> t) -> t -> t -> closure
val app_const_var : (t -> t -> t) -> t -> int -> closure
val app_const_env : (t -> t -> t) -> t -> int -> int -> closure
val app_const_meth : (t -> t -> t) -> t -> label -> closure
val app_var_const : (t -> t -> t) -> int -> t -> closure
val app_env_const : (t -> t -> t) -> int -> int -> t -> closure
val app_meth_const : (t -> t -> t) -> label -> t -> closure
val meth_app_const : label -> t -> closure
val meth_app_var : label -> int -> closure
val meth_app_env : label -> int -> int -> closure
val meth_app_meth : label -> label -> closure
val send_const : tag -> obj -> int -> closure
val send_var : tag -> int -> int -> closure
val send_env : tag -> int -> int -> int -> closure
val send_meth : tag -> label -> int -> closure
*)

type impl =
    GetConst
  | GetVar
  | GetEnv
  | GetMeth
  | SetVar
  | AppConst
  | AppVar
  | AppEnv
  | AppMeth
  | AppConstConst
  | AppConstVar
  | AppConstEnv
  | AppConstMeth
  | AppVarConst
  | AppEnvConst
  | AppMethConst
  | MethAppConst
  | MethAppVar
  | MethAppEnv
  | MethAppMeth
  | SendConst
  | SendVar
  | SendEnv
  | SendMeth
  | Closure of closure

(** {1 Parameters} *)

(* currently disabled *)
type params =
  { mutable compact_table : bool;
    mutable copy_parent : bool;
    mutable clean_when_copying : bool;
    mutable retry_count : int;
    mutable bucket_small_size : int }

(* 28 *) val params : params

(** {1 Statistics} *)

type stats =
  { classes : int;
    methods : int;
    inst_vars : int }
(* 29 *) val stats : unit -> stats

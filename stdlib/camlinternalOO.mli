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

type tag
type label
type table
type meth
type t
type obj
type closure
(* 00 *) val public_method_label : string -> tag
(* 01 *) val new_method : table -> label
(* 02 *) val new_variable : table -> string -> int
(* 03 *) val new_methods_variables :
    table -> string array -> string array -> label array
(* 04 *) val get_variable : table -> string -> int
(* 05 *) val get_variables : table -> string array -> int array
(* 06 *) val get_method_label : table -> string -> label
(* 07 *) val get_method_labels : table -> string array -> label array
(* 08 *) val get_method : table -> label -> meth
(* 09 *) val set_method : table -> label -> meth -> unit
(* 10 *) val set_methods : table -> label array -> unit
(* 11 *) val narrow : table -> string array -> string array -> string array -> unit
(* 12 *) val widen : table -> unit
(* 13 *) val add_initializer : table -> (obj -> unit) -> unit
(* 14 *) val dummy_table : table
(* 15 *) val create_table : string array -> table
(* 16 *) val init_class : table -> unit
(* 17 *) val inherits :
    table -> string array -> string array -> string array ->
    (t * (table -> obj -> Obj.t) * obj) -> bool -> Obj.t array
(* 18 *) val make_class :
    string array -> (table -> Obj.t -> t) ->
    (t * (table -> Obj.t -> t) * Obj.t)
type init_table
(* 19 *) val make_class_store :
    string array -> (table -> t) -> init_table -> unit
(* 20 *) val dummy_class :
    string * int * int ->
    (t * (table -> Obj.t -> t) * Obj.t)

(** {1 Objects} *)

(* 21 *) val copy : (< .. > as 'a) -> 'a
(* 22 *) val create_object : table -> obj
(* 23 *) val create_object_opt : obj -> table -> obj
(* 24 *) val run_initializers : obj -> table -> unit
(* 25 *) val run_initializers_opt : obj -> obj -> table -> obj
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

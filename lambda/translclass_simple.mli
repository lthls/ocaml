open Typedtree
open Lambda
open Debuginfo.Scoped_location

val transl_class :
  scopes:scopes -> Ident.t list -> Ident.t ->
  string list -> class_expr -> Asttypes.virtual_flag ->
  lambda * Value_rec_types.recursive_binding_kind

type error = Tags of string * string

exception Error of Location.t * error

open Format_doc

val report_error: formatter -> error -> unit

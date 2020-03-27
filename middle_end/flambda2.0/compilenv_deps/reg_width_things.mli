(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The underlying implementation for [Variable], [Symbol], [Name],
    [Reg_width_const] and [Simple]. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module Const : sig
  type t = private Table_by_int_id.Id.t

  include Identifiable.S with type t := t

  val const_true : t
  val const_false : t

  val untagged_const_true : t
  val untagged_const_false : t

  val const_zero : t
  val const_one : t
  val const_unit : t

  val const_int : Targetint.OCaml.t -> t

  (** [naked_immediate] is similar to [naked_nativeint], but represents
      integers of width [n - 1] bits, where [n] is the native machine
      width. (By contrast, [naked_nativeint] represents integers of
      width [n] bits.) *)
  val naked_immediate : Immediate.t -> t
  val tagged_immediate : Immediate.t -> t
  val naked_float : Numbers.Float_by_bit_pattern.t -> t
  val naked_int32 : Int32.t -> t
  val naked_int64 : Int64.t -> t
  val naked_nativeint : Targetint.t -> t

  module Descr : sig
    type t = private
      | Naked_immediate of Immediate.t
      | Tagged_immediate of Immediate.t
      | Naked_float of Numbers.Float_by_bit_pattern.t
      | Naked_int32 of Int32.t
      | Naked_int64 of Int64.t
      | Naked_nativeint of Targetint.t

    include Identifiable.S with type t := t
  end

  val descr : t -> Descr.t
end

module Variable : sig
  type t = private Table_by_int_id.Id.t

  include Identifiable.S with type t := t

  val create : ?user_visible:unit -> string -> t

  val compilation_unit : t -> Compilation_unit.t

  val name : t -> string

  val name_stamp : t -> int

  val user_visible : t -> bool
end

module Symbol : sig
  type t = private Table_by_int_id.Id.t

  include Identifiable.S with type t := t

  val create : Compilation_unit.t -> Linkage_name.t -> t

  (** Create the symbol without prefixing with the compilation unit. Used for
      predefined exceptions *)
  val unsafe_create : Compilation_unit.t -> Linkage_name.t -> t

  val compilation_unit : t -> Compilation_unit.t

  val linkage_name : t -> Linkage_name.t
end

module Name : sig
  type t = private Table_by_int_id.Id.t

  include Identifiable.S with type t := t

  val var : Variable.t -> t

  val symbol : Symbol.t -> t

  val pattern_match
     : t
    -> var:(Variable.t -> 'a)
    -> symbol:(Symbol.t -> 'a)
    -> 'a
end

module Simple : sig
  type t = private Table_by_int_id.Id.t

  include Identifiable.S with type t := t

  val name : Name.t -> t

  val var : Variable.t -> t

  val vars : Variable.t list -> t list

  val symbol : Symbol.t -> t

  val const : Const.t -> t

  val rec_info : t -> Rec_info.t option

  val with_rec_info : t -> Rec_info.t -> t

  val pattern_match
     : t
    -> name:(Name.t -> 'a)
    -> const:(Const.t -> 'a)
    -> 'a

   (* [same s1 s2] returns true iff they represent the same name or const
      i.e. [same s (with_rec_info s rec_info)] returns true *)
   val same : t -> t -> bool

   (* compare_sort uses the underlying data for comparison,
      and as a result is not dependent on hashes *)
   val compare_sort : t -> t -> int
end

val initialise : unit -> unit

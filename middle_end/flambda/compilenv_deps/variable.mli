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

[@@@ocaml.warning "+a-30-40-41-42"]

(** The names of variables. *)

include module type of struct include Reg_width_things.Variable end

val create_with_same_name_as_ident : ?user_visible:unit -> Ident.t -> t * exported

(* CR mshinwell: check on gdb branch if this preserves the "original ident".
   Sometimes it should and other times it should not (eg unboxing) *)
(** [rename] always returns a variable with a compilation unit set to that
    of the current unit, not the unit of the variable passed in. *)
val rename : ?append:string -> exported -> (t * exported)

val unique_name : exported -> string

val print_list : Format.formatter -> t list -> unit
val print_data_list : Format.formatter -> exported list -> unit
val print_opt : Format.formatter -> exported option -> unit

val raw_name : exported -> string
val raw_name_stamp : exported -> int

(** If the given variable has the given stamp, call the user-supplied
    function.  For debugging purposes only. *)
val debug_when_stamp_matches : exported -> stamp:int -> f:(unit -> unit) -> unit

(* CR mshinwell: move to List.compare *)
val compare_lists : t list -> t list -> int

module List : sig
  val rename : ?append:string -> exported list -> (t * exported) list

  type nonrec t = t list
end

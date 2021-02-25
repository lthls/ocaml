(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Typing environment extensions: typing environment levels that are
    surrounded by a binder that captures defined names as existentials. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = private {
  equations : Type_grammar.t Name.Map.t;
}[@@unboxed]

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : unit -> t

val one_equation : Name.t -> Type_grammar.t -> t

val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t

val meet : Meet_env.t -> t -> t -> t Or_bottom.t

(* Note: [join] takes a [Meet_env.t] argument on purpose: Typing_env_extensions
   are created for meets, and this function is here for case where a meet
   produces a disjunction, in which case the extensions must be joined together.
*)
val join : Meet_env.t -> t -> t -> t

module With_extra_variables : sig
  type t = private {
    existential_vars : (Flambda_kind.t * Variable.exported) Variable.Map.t;
    equations : Type_grammar.t Name.Map.t;
  }

  val print : Format.formatter -> t -> unit

  val empty : unit -> t

  val add_definition
     : t
    -> (Variable.t * Variable.exported)
    -> Flambda_kind.t
    -> Type_grammar.t
    -> t

  val add_or_replace_equation : t -> Name.t -> Type_grammar.t -> t
end

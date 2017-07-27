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

(** Approximations of continuations.  Used during simplification. *)

type t

type continuation_handlers =
  | Nonrecursive of Flambda.continuation_handlers
  | Recursive of Flambda.continuation_handlers

val create
   : name:Continuation.t
  -> handlers:continuation_handlers
  -> num_params:int
  -> t

val create_unknown : name:Continuation.t -> num_params:int -> t

val name : t -> Continuation.t

val num_params : t -> int
val handlers : t -> continuation_handlers option

val print : Format.formatter -> t -> unit

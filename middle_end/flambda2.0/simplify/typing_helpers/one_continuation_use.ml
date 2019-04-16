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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module T = Flambda_type
module TE = Flambda_type.Typing_env

type t = {
  id : Apply_cont_rewrite_id.t;
  kind : Continuation_use_kind.t;
  arg_types : T.t list;
  typing_env : TE.t;
}

let create kind ~typing_env_at_use:typing_env id ~arg_types =
  { id;
    kind;
    arg_types;
    typing_env;
  }

let print ppf { typing_env = _; id = _; kind = _; arg_types; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(arg_types@ %a)@]@ \
      )@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Flambda_type.print)
    arg_types

let id t = t.id
let arg_types t = t.arg_types
let typing_env_at_use t = t.typing_env

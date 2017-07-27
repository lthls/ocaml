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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type continuation_handlers =
  | Nonrecursive of Flambda.continuation_handlers
  | Recursive of Flambda.continuation_handlers

type t = {
  name : Continuation.t;
  handlers : continuation_handlers option;
  num_params : int;
}

let create ~name ~(handlers : continuation_handlers) ~num_params =
  { name;
    handlers = Some handlers;
    num_params;
  }

let create_unknown ~name ~num_params =
  { name;
    handlers = None;
    num_params;
  }

let name t = t.name
let num_params t = t.num_params
let handlers t = t.handlers

let print ppf t =
  let print_handlers ppf = function
    | None -> Format.fprintf ppf "<handlers not known>"
    | Some handlers ->
      match handlers with
      | Nonrecursive handlers ->
        Flambda.print_let_cont_handlers ppf (Nonrecursive handlers)
      | Recursive handlers ->
        Flambda.print_let_cont_handlers ppf (Recursive handlers)
  in
  Format.fprintf ppf "@[(%a with %d params %a)@]"
    Continuation.print t.name
    t.num_params
    print_handlers t.handlers

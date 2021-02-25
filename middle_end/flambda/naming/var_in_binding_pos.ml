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

type t = {
  var : Variable.t;
  var_data : Variable.exported;
  name_mode : Name_mode.t;
}

let create (var, var_data) name_mode =
  { var;
    var_data;
    name_mode;
  }

let raw_var t = t.var
let var t = (t.var, t.var_data)
let simple t = Simple.var (raw_var t)
let name_mode t = t.name_mode

let with_var t (var, var_data) = { t with var; var_data; }
let with_raw_var t var = { t with var; }
let with_name_mode t name_mode = { t with name_mode; }

let rename t = with_var t (Variable.rename t.var_data)

let apply_name_permutation t perm =
  let new_var = Name_permutation.apply_variable perm t.var in
  if Variable.equal new_var t.var then t
  else { t with var = new_var; }

let free_names t =
  Name_occurrences.singleton_variable t.var t.name_mode

include Identifiable.Make (struct
  type nonrec t = t

(*
  let print ppf { var; name_mode; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(var@ %a)@]@ \
        @[<hov 1>(name_mode@ %a)@]\
        )@]"
      Variable.print var
      Name_mode.print name_mode
*)

  let print ppf { var = _; var_data; name_mode; } =
    match Name_mode.descr name_mode with
    | Normal -> Variable.print_data ppf var_data
    | In_types -> Format.fprintf ppf "@[%a\u{1d749}@]" Variable.print_data var_data
    | Phantom -> Variable.print_data ppf var_data
(*
    | Phantom -> Format.fprintf ppf "@[%a\u{1f47b}@]" Variable.print var
*)

  let compare
        { var = var1; name_mode = name_mode1; var_data = _; }
        { var = var2; name_mode = name_mode2; var_data = _; } =
    let c = Variable.compare var1 var2 in
    if c <> 0 then c
    else
      Name_mode.compare_total_order name_mode1 name_mode2

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

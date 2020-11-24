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

(* module A =
 *   Name_abstraction.Make_map (Bindable_variable_in_types) (Typing_env_level) *)
module FPEM = Flambda_primitive.Eligible_for_cse.Map

(* The record is here to avoid the double vision problem.  (Otherwise
   there would already be an equality
     t = Name_abstraction.Make_list ...
   meaning that the equality
     t = Typing_env_extension.t
   could not be added by the type checker.) *)
(* type t = {
 *   abst : A.t;
 * } [@@unboxed] *)
type t = {
  equations : Type_grammar.t Name.Map.t;
}[@@unboxed]

let print_equations ppf equations =
  let equations = Name.Map.bindings equations in
  match equations with
  | [] -> Format.pp_print_string ppf "()"
  | _::_ ->
    Format.pp_print_string ppf "(";
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun ppf (name, ty) ->
        Format.fprintf ppf
          "@[<hov 1>%a@ :@ %a@]"
          Name.print name
          Type_grammar.print ty)
      ppf equations;
    Format.pp_print_string ppf ")"

let print ppf t =
  Format.fprintf ppf
    "@[<hov 1>(equations@ @[<v 1>%a@])@]"
    print_equations t.equations

let invariant _ = ()

let empty () = { equations = Name.Map.empty; }

let one_equation name ty =
  { equations = Name.Map.singleton name ty; }

let add_or_replace_equation t name ty =
  { equations = Name.Map.add name ty t.equations; }

exception Bottom_meet

let rec meet0 env (t1 : t) (t2 : t) extra_extensions =
  (* A symmetrical meet would be hard to implement, as
     the inner meets can produce extra extensions that need
     to be merged with the result.
     To get around this, we'll suppose that [t2] is smaller than [t1]
     and add equations from [t2] to [t1], along with all extra equations
  *)
  let equations, extra_extensions =
    Name.Map.fold (fun name ty (eqs, extra_extensions) ->
        match Name.Map.find_opt name eqs with
        | None -> Name.Map.add name ty eqs, extra_extensions
        | Some ty0 ->
          begin match Type_grammar.meet env ty0 ty with
          | Bottom -> raise Bottom_meet
          | Ok (ty, new_ext) ->
            Name.Map.add (*replace*) name ty eqs, new_ext :: extra_extensions
          end)
      t2.equations
      (t1.equations, extra_extensions)
  in
  let ext = { equations; } in
  match extra_extensions with
  | [] -> ext
  | new_ext :: extra_extensions ->
    meet0 env ext new_ext extra_extensions

let meet env t1 t2 : _ Or_bottom.t =
  try
    Ok (meet0 env t1 t2 [])
  with Bottom_meet -> Bottom

let join env t1 t2 =
  let env = Meet_env.env env in
  let join_env =
    Meet_or_join_env.create env ~left_env:env ~right_env:env
  in
  let equations =
    Name.Map.merge (fun _name ty1_opt ty2_opt ->
        match ty1_opt, ty2_opt with
        | None, _ | _, None -> None
        | Some ty1, Some ty2 ->
          begin match Type_grammar.join join_env ty1 ty2 with
          | Known ty -> Some ty
          | Unknown -> None
          end)
      t1.equations
      t2.equations
  in
  { equations; }

let rec n_way_meet env ts =
  match ts with
  | [] -> empty ()
  | t::ts -> meet0 env t (n_way_meet env ts) []

module With_extra_variables = struct
  type t = {
    existential_vars : Flambda_kind.t Variable.Map.t;
    equations : Type_grammar.t Name.Map.t;
  }

  let print ppf { existential_vars; equations; } =
    Format.fprintf ppf
      "@[<hov 1>(\
       @[<hov 1>(variables@ @[<hov 1>%a@])@]\
       @[<hov 1>(equations@ @[<v 1>%a@])@])@ \
       @]"
      (Variable.Map.print Flambda_kind.print) existential_vars
      print_equations equations

  let empty () =
    { existential_vars = Variable.Map.empty;
      equations = Name.Map.empty;
    }

  let add_definition t var kind ty =
    let existential_vars = Variable.Map.add var kind t.existential_vars in
    let equations = Name.Map.add (Name.var var) ty t.equations in
    { existential_vars;
      equations;
    }

  let add_or_replace_equation t name ty =
    { existential_vars = t.existential_vars;
      equations = Name.Map.add name ty t.equations;
    }
end

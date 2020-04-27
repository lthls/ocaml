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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Contents of middle-end-specific portion of .cmx files when using
    Flambda. *)

module Const = Reg_width_things.Const

type table_data = {
  symbols : Symbol.exported Symbol.Map.t;
  variables : Variable.exported Variable.Map.t;
  simples : Simple.exported Simple.Map.t;
  consts : Const.exported Const.Map.t;
  code_ids : Code_id.exported Code_id.Map.t;
}

type t = {
  final_typing_env : Flambda_type.Typing_env.Serializable.t;
  all_code : Flambda.Function_params_and_body.t Code_id.Map.t;
  exported_offsets : Exported_offsets.t;
  table_data : table_data;
}

let create ~final_typing_env ~all_code ~exported_offsets =
  let typing_env_exported_ids =
    Flambda_type.Typing_env.Serializable.all_ids_for_export final_typing_env
  in
  let exported_ids =
    Code_id.Map.fold (fun code_id params_and_body ids ->
        let ids_for_params_and_body =
          Flambda.Function_params_and_body.all_ids_for_export params_and_body
        in
        Ids_for_export.add_code_id
          (Ids_for_export.union ids ids_for_params_and_body)
          code_id)
      all_code
      typing_env_exported_ids
  in
  let symbols =
    Symbol.Set.fold (fun symbol symbols ->
        Symbol.Map.add symbol (Symbol.export symbol) symbols)
      exported_ids.symbols
      Symbol.Map.empty
  in
  let variables =
    Variable.Set.fold (fun variable variables ->
        Variable.Map.add variable (Variable.export variable) variables)
      exported_ids.variables
      Variable.Map.empty
  in
  let simples =
    Simple.Set.fold (fun simple simples ->
        Simple.Map.add simple (Simple.export simple) simples)
      exported_ids.simples
      Simple.Map.empty
  in
  let consts =
    Const.Set.fold (fun const consts ->
        Const.Map.add const (Const.export const) consts)
      exported_ids.consts
      Const.Map.empty
  in
  let code_ids =
    Code_id.Set.fold (fun code_id code_ids ->
        Code_id.Map.add code_id (Code_id.export code_id) code_ids)
      exported_ids.code_ids
      Code_id.Map.empty
  in
  let table_data =
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
    }
  in
  { final_typing_env;
    all_code;
    exported_offsets;
    table_data;
  }

let import_typing_env_and_code t =
  (* First create map for data that does not contain ids, i.e. everything
     except simples *)
  let symbols = Symbol.Map.map Symbol.import t.table_data.symbols in
  let variables = Variable.Map.map Variable.import t.table_data.variables in
  let consts = Const.Map.map Const.import t.table_data.consts in
  let code_ids = Code_id.Map.map Code_id.import t.table_data.code_ids in
  (* Build a simple to simple converter from this *)
  let import_map =
    Ids_for_export.Import_map.create
      ~symbols
      ~variables
      ~simples:Simple.Map.empty
      ~consts
      ~code_ids
  in
  let map_simple = Ids_for_export.Import_map.simple import_map in
  (* Then convert the simples *)
  let simples =
    Simple.Map.map (Simple.import map_simple) t.table_data.simples
  in
  let import_map =
    Ids_for_export.Import_map.create
      ~symbols
      ~variables
      ~simples
      ~consts
      ~code_ids
  in
  let typing_env =
    Flambda_type.Typing_env.Serializable.import import_map t.final_typing_env
  in
  let all_code =
    Code_id.Map.fold (fun code_id params_and_body all_code ->
        let code_id = Ids_for_export.Import_map.code_id import_map code_id in
        let params_and_body =
          Flambda.Function_params_and_body.import import_map params_and_body
        in
        Code_id.Map.add code_id params_and_body all_code)
      t.all_code
      Code_id.Map.empty
  in
  typing_env, all_code

let exported_offsets t = t.exported_offsets
let with_exported_offsets t exported_offsets = { t with exported_offsets; }

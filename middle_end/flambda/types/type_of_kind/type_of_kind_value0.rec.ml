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

[@@@ocaml.warning "+a-30-40-41-42"]

module T = Type_grammar
module TEE = Typing_env_extension

module Blocks = Row_like.For_blocks

type t =
  | Variant of Variant.t
  (* CR mshinwell: Add constructors for the following too so we can check
     they aren't bottom? *)
  | Boxed_float of T.t
  | Boxed_int32 of T.t
  | Boxed_int64 of T.t
  | Boxed_nativeint of T.t
  | Closures of {
      by_closure_id : Row_like.For_closures_entry_by_set_of_closures_contents.t;
    }
  | String of String_info.Set.t
  | Array of { length : T.t; }

let print_with_cache ~cache ppf t =
  match t with
  | Variant variant ->
    Format.fprintf ppf "@[<hov 1>(Variant@ %a)@]"
      (Variant.print_with_cache ~cache) variant
  | Boxed_float naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_float@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_int32 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int32@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_int64 naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_int64@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Boxed_nativeint naked_ty ->
    Format.fprintf ppf "@[<hov 1>(Boxed_nativeint@ %a)@]"
      (T.print_with_cache ~cache) naked_ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.print_with_cache ~cache
      ppf by_closure_id
  | String str_infos ->
    Format.fprintf ppf "@[<hov 1>(Strings@ (%a))@]"
      String_info.Set.print str_infos
  | Array { length; } ->
    Format.fprintf ppf "@[<hov 1>(Array@ (length@ %a))@]"
      (T.print_with_cache ~cache) length

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let apply_renaming t perm =
  match t with
  | Variant variant ->
    let variant' = Variant.apply_renaming variant perm in
    if variant == variant' then t
    else Variant variant'
  | Boxed_float ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t
    else Boxed_float ty'
  | Boxed_int32 ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t
    else Boxed_int32 ty'
  | Boxed_int64 ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t
    else Boxed_int64 ty'
  | Boxed_nativeint ty ->
    let ty' = T.apply_renaming ty perm in
    if ty == ty' then t
    else Boxed_nativeint ty'
  | Closures { by_closure_id; } ->
    let by_closure_id' =
      Row_like.For_closures_entry_by_set_of_closures_contents.apply_renaming
        by_closure_id perm
    in
    if by_closure_id == by_closure_id' then t
    else Closures { by_closure_id = by_closure_id'; }
  | String _ -> t
  | Array { length; } ->
    let length' = T.apply_renaming length perm in
    if length == length' then t
    else Array { length = length'; }

let free_names t =
  match t with
  | Variant variant -> Variant.free_names variant
  | Boxed_float ty -> T.free_names ty
  | Boxed_int32 ty -> T.free_names ty
  | Boxed_int64 ty -> T.free_names ty
  | Boxed_nativeint ty -> T.free_names ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.free_names by_closure_id
  | String _ -> Name_occurrences.empty
  | Array { length; } -> T.free_names length

let all_ids_for_export t =
  match t with
  | Variant variant -> Variant.all_ids_for_export variant
  | Boxed_float ty -> T.all_ids_for_export ty
  | Boxed_int32 ty -> T.all_ids_for_export ty
  | Boxed_int64 ty -> T.all_ids_for_export ty
  | Boxed_nativeint ty -> T.all_ids_for_export ty
  | Closures { by_closure_id; } ->
    Row_like.For_closures_entry_by_set_of_closures_contents.all_ids_for_export
      by_closure_id
  | String _ -> Ids_for_export.empty
  | Array { length; } -> T.all_ids_for_export length

let apply_coercion t coercion : _ Or_bottom.t =
  match t with
  | Closures { by_closure_id; } ->
    begin match
      Row_like.For_closures_entry_by_set_of_closures_contents.
        map_function_decl_types
         by_closure_id
         ~f:(fun (decl : Function_declaration_type.t) : _ Or_bottom.t ->
           Function_declaration_type.apply_coercion decl coercion)
    with
    | Bottom -> Bottom
    | Ok by_closure_id ->
      match
        Row_like.For_closures_entry_by_set_of_closures_contents.
          map_closure_types
          by_closure_id
          ~f:(fun ty -> Type_grammar.apply_coercion ty coercion)
      with
      | Bottom -> Bottom
      | Ok by_closure_id ->
        Ok (Closures { by_closure_id; })
    end
  | Variant _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | String _
  | Array _ ->
    if Coercion.is_id coercion then Ok t
    else Bottom

let eviscerate t : _ Or_unknown.t =
  match t with
  | Boxed_float _ -> Known (Boxed_float (T.any_naked_float ()))
  | Boxed_int32 _ -> Known (Boxed_int32 (T.any_naked_int32 ()))
  | Boxed_int64 _ -> Known (Boxed_int64 (T.any_naked_int64 ()))
  | Boxed_nativeint _ -> Known (Boxed_nativeint (T.any_naked_nativeint ()))
  | Closures _
  | Variant _
  | String _
  | Array _ -> Unknown

let meet env t1 t2 : _ Or_bottom.t =
  match t1, t2 with
  | Variant variant1, Variant variant2 ->
    Or_bottom.map
      (Variant.meet env variant1 variant2)
      ~f:(fun (variant, env_extension) -> Variant variant, env_extension)
  | Boxed_float n1, Boxed_float n2 ->
    Or_bottom.map
      (T.meet env n1 n2)
      ~f:(fun (n, env_extension) -> Boxed_float n, env_extension)
  | Boxed_int32 n1, Boxed_int32 n2 ->
    Or_bottom.map
      (T.meet env n1 n2)
      ~f:(fun (n, env_extension) -> Boxed_int32 n, env_extension)
  | Boxed_int64 n1, Boxed_int64 n2 ->
    Or_bottom.map
      (T.meet env n1 n2)
      ~f:(fun (n, env_extension) -> Boxed_int64 n, env_extension)
  | Boxed_nativeint n1, Boxed_nativeint n2 ->
    Or_bottom.map
      (T.meet env n1 n2)
      ~f:(fun (n, env_extension) -> Boxed_nativeint n, env_extension)
  | Closures { by_closure_id = by_closure_id1; },
    Closures { by_closure_id = by_closure_id2; } ->
    let module C = Row_like.For_closures_entry_by_set_of_closures_contents in
    Or_bottom.map
      (C.meet env by_closure_id1 by_closure_id2)
      ~f:(fun (by_closure_id, env_extension) ->
        Closures { by_closure_id; }, env_extension)
  | String strs1, String strs2 ->
    let strs = String_info.Set.inter strs1 strs2 in
    if String_info.Set.is_empty strs then Bottom
    else Or_bottom.Ok (String strs, TEE.empty ())
  | Array { length = length1; }, Array { length = length2; } ->
    Or_bottom.map
      (T.meet env length1 length2)
      ~f:(fun (length, env_extension) -> Array { length; }, env_extension)
  | (Variant _
    | Boxed_float _
    | Boxed_int32 _
    | Boxed_int64 _
    | Boxed_nativeint _
    | Closures _
    | String _
    | Array _), _ ->
    (* CR vlaviron: This assumes that all the different constructors are
       incompatible. This could break very hard for users of Obj. *)
    Bottom

let join env t1 t2 : _ Or_unknown.t =
  match t1, t2 with
  | Variant variant1, Variant variant2 ->
    Known (Variant (Variant.join env variant1 variant2))
  | Boxed_float n1, Boxed_float n2 ->
    Or_unknown.map
      (T.join env n1 n2)
      ~f:(fun n -> Boxed_float n)
  | Boxed_int32 n1, Boxed_int32 n2 ->
    Or_unknown.map
      (T.join env n1 n2)
      ~f:(fun n -> Boxed_int32 n)
  | Boxed_int64 n1, Boxed_int64 n2 ->
    Or_unknown.map
      (T.join env n1 n2)
      ~f:(fun n -> Boxed_int64 n)
  | Boxed_nativeint n1, Boxed_nativeint n2 ->
    Or_unknown.map
      (T.join env n1 n2)
      ~f:(fun n -> Boxed_nativeint n)
  | Closures { by_closure_id = by_closure_id1; },
    Closures { by_closure_id = by_closure_id2; } ->
    let module C = Row_like.For_closures_entry_by_set_of_closures_contents in
    let by_closure_id = C.join env by_closure_id1 by_closure_id2 in
    Known (Closures { by_closure_id; })
  | String strs1, String strs2 ->
    let strs = String_info.Set.union strs1 strs2 in
    Known (String strs)
  | Array { length = length1; }, Array { length = length2; } ->
    Or_unknown.map
      (T.join env length1 length2)
      ~f:(fun length -> Array { length; })
  | (Variant _
    | Boxed_float _
    | Boxed_int32 _
    | Boxed_int64 _
    | Boxed_nativeint _
    | Closures _
    | String _
    | Array _), _ -> Unknown

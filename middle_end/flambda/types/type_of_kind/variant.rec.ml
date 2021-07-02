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
module B = Row_like.For_blocks

type t =
| Immediates of { immediates : Type_grammar.t Or_unknown.t; }
| Blocks of { blocks : Row_like.For_blocks.t Or_unknown.t; is_unique : bool; }
| Either of {
    immediates : Type_grammar.t Or_unknown.t;
    immediates_extension : Typing_env_extension.t;
    blocks : Row_like.For_blocks.t Or_unknown.t;
    blocks_extension : Typing_env_extension.t;
    is_unique : bool;
  }

let print_with_cache ~cache ppf t =
  match t with
  | Immediates { immediates; } ->
    Format.fprintf ppf
      "@[<hov 1>(tagged_imms@ %a)@]"
      (Or_unknown.print (T.print_with_cache ~cache)) immediates
  | Blocks { blocks; is_unique; } ->
    Format.fprintf ppf
      "@[<hov 1>(blocks%s@ %a)@]"
      (if is_unique then " unique" else "")
      (Or_unknown.print (B.print_with_cache ~cache)) blocks
  | Either {
      immediates;
      immediates_extension = _;
      blocks;
      blocks_extension = _;
      is_unique;
    } ->
    Format.fprintf ppf
      "@[<hov 1>(blocks%s@ %a)@]@ \
       @[<hov 1>(tagged_imms@ %a)@]"
      (if is_unique then " unique" else "")
      (Or_unknown.print (B.print_with_cache ~cache)) blocks
      (Or_unknown.print (T.print_with_cache ~cache)) immediates

(* CR mshinwell: This can now return [Or_bottom.t] *)
let create ~is_unique ~immediates ~blocks =
  let blocks_is_bottom =
    match blocks with
    | Or_unknown.Unknown -> false
    | Or_unknown.Known blocks -> B.is_bottom blocks
  in
  let immediates_is_bottom =
    match immediates with
    | Or_unknown.Unknown -> false
    | Or_unknown.Known immediates ->
      if not (K.equal (T.kind immediates) K.naked_immediate) then begin
        Misc.fatal_errorf "Cannot create [immediates] with type that is not \
            of kind [Naked_immediate]:@ %a"
          T.print immediates
      end;
      T.is_obviously_bottom immediates
  in
  if blocks_is_bottom then Immediates { immediates; }
  else if immediates_is_bottom then Blocks { blocks; is_unique; }
  else
    Either {
      immediates;
      immediates_extension = TEE.empty ();
      blocks;
      blocks_extension = TEE.empty ();
      is_unique;
    }

let meet_immediates env immediates1 immediates2 extension_opt : _ Or_bottom.t =
  let empty_ext () =
    match extension_opt with
    | None -> TEE.empty ()
    | Some ext -> ext
  in
  match (immediates1 : _ Or_unknown.t), (immediates2 : _ Or_unknown.t) with
  | Unknown, _ -> Ok (immediates2, empty_ext ())
  | _, Unknown -> Ok (immediates1, empty_ext ())
  | Known immediates1, Known immediates2 ->
    Or_bottom.bind (T.meet env immediates1 immediates2)
      ~f:(fun (immediates, env_extension) ->
        match extension_opt with
        | None -> Ok (Or_unknown.Known immediates, env_extension)
        | Some other_extension ->
          Or_bottom.map
            (TEE.meet env env_extension other_extension)
            ~f:(fun env_extension ->
              Or_unknown.Known immediates, env_extension))

let meet_blocks env blocks1 blocks2 extension_opt : _ Or_bottom.t =
  let empty_ext () =
    match extension_opt with
    | None -> TEE.empty ()
    | Some ext -> ext
  in
  match (blocks1 : _ Or_unknown.t), (blocks2 : _ Or_unknown.t) with
  | Unknown, _ -> Ok (blocks2, empty_ext ())
  | _, Unknown -> Ok (blocks1, empty_ext ())
  | Known blocks1, Known blocks2 ->
    Or_bottom.bind (B.meet env blocks1 blocks2)
      ~f:(fun (blocks, env_extension) ->
        match extension_opt with
        | None -> Ok (Or_unknown.Known blocks, env_extension)
        | Some other_extension ->
          Or_bottom.map
            (TEE.meet env env_extension other_extension)
            ~f:(fun env_extension ->
              Or_unknown.Known blocks, env_extension))

let meet env t1 t2 : _ Or_bottom.t =
  match t1, t2 with
  | Immediates _, Blocks _ | Blocks _, Immediates _ -> Bottom
  | Immediates { immediates = immediates1; },
    Immediates { immediates = immediates2; } ->
    Or_bottom.map (meet_immediates env immediates1 immediates2 None)
      ~f:(fun (immediates, env_extension) ->
        Immediates { immediates; }, env_extension)
  | Blocks { blocks = blocks1; is_unique = is_unique1; },
    Blocks { blocks = blocks2; is_unique = is_unique2; } ->
    let is_unique = is_unique1 || is_unique2 in
    Or_bottom.map (meet_blocks env blocks1 blocks2 None)
      ~f:(fun (blocks, env_extension) ->
        Blocks { blocks; is_unique; }, env_extension)
  | Immediates { immediates = immediates1; },
    Either { immediates = immediates2;
             immediates_extension = env_extension;
             blocks = _;
             blocks_extension = _;
             is_unique = _;
           }
  | Either { immediates = immediates1;
             immediates_extension = env_extension;
             blocks = _;
             blocks_extension = _;
             is_unique = _;
           },
    Immediates { immediates = immediates2; } ->
    Or_bottom.map
      (meet_immediates env immediates1 immediates2 (Some env_extension))
      ~f:(fun (immediates, env_extension) ->
        Immediates { immediates; }, env_extension)
  | Blocks { blocks = blocks1; is_unique = is_unique1; },
    Either { immediates = _;
             immediates_extension = _;
             blocks = blocks2;
             blocks_extension = env_extension;
             is_unique = is_unique2;
           }
  | Either { immediates = _;
             immediates_extension = _;
             blocks = blocks1;
             blocks_extension = env_extension;
             is_unique = is_unique1;
           },
    Blocks { blocks = blocks2; is_unique = is_unique2 } ->
    let is_unique = is_unique1 || is_unique2 in
    Or_bottom.map (meet_blocks env blocks1 blocks2 (Some env_extension))
      ~f:(fun (blocks, env_extension) ->
        Blocks { blocks; is_unique; }, env_extension)
  | Either { immediates = immediates1;
             immediates_extension = imms_extension1;
             blocks = blocks1;
             blocks_extension = blocks_extension1;
             is_unique = is_unique1;
           },
    Either { immediates = immediates2;
             immediates_extension = imms_extension2;
             blocks = blocks2;
             blocks_extension = blocks_extension2;
             is_unique = is_unique2;
           } ->
    let is_unique = is_unique1 || is_unique2 in
    let immediates_res =
      Or_bottom.bind (TEE.meet env imms_extension1 imms_extension2)
        ~f:(fun env_extension ->
          meet_immediates env immediates1 immediates2 (Some env_extension))
    in
    let blocks_res =
      Or_bottom.bind (TEE.meet env blocks_extension1 blocks_extension2)
        ~f:(fun env_extension ->
          meet_blocks env blocks1 blocks2 (Some env_extension))
    in
    begin match immediates_res, blocks_res with
    | Bottom, Bottom -> Bottom
    | Bottom, Ok (blocks, env_extension) ->
      Ok (Blocks { blocks; is_unique; }, env_extension)
    | Ok (immediates, env_extension), Bottom ->
      Ok (Immediates { immediates; }, env_extension)
    | Ok (immediates, immediates_extension),
      Ok (blocks, blocks_extension) ->
      let env = Meet_env.env env in
      let join_env =
        Join_env.create env ~left_env:env ~right_env:env
      in
      Ok (Either { immediates;
                   immediates_extension;
                   blocks;
                   blocks_extension;
                   is_unique; },
          TEE.join join_env
            immediates_extension blocks_extension)
    end

let join_immediates env immediates1 immediates2 : _ Or_unknown.t =
  match (immediates1 : _ Or_unknown.t), (immediates2 : _ Or_unknown.t) with
  | Unknown, _ | _, Unknown -> Unknown
  | Known immediates1, Known immediates2 ->
    T.join env immediates1 immediates2

let join_blocks env blocks1 blocks2 : _ Or_unknown.t =
  match (blocks1 : _ Or_unknown.t), (blocks2 : _ Or_unknown.t) with
  | Unknown, _ | _, Unknown -> Unknown
  | Known blocks1, Known blocks2 ->
    Known (B.join env blocks1 blocks2)

let join env t1 t2 =
  match t1, t2 with
  | Immediates { immediates = immediates1; },
    Immediates { immediates = immediates2; } ->
    let immediates = join_immediates env immediates1 immediates2 in
    Immediates { immediates; }
  | Blocks { blocks = blocks1; is_unique = is_unique1; },
    Blocks { blocks = blocks2; is_unique = is_unique2; } ->
    let is_unique = is_unique1 || is_unique2 in
    let blocks = join_blocks env blocks1 blocks2 in
    Blocks { blocks; is_unique; }
  | Immediates { immediates; }, Blocks { blocks; is_unique; }
  | Blocks { blocks; is_unique; }, Immediates { immediates; } ->
    (* CR vlaviron: We could actually compute extensions from the join env here.
       However I expect this to be expensive, so unless we find examples where
       it would make a difference, I think it's better to put empty extensions.
    *)
    let immediates_extension = TEE.empty () in
    let blocks_extension = TEE.empty () in
    Either { immediates;
             immediates_extension;
             blocks;
             blocks_extension;
             is_unique;
           }
  | Immediates { immediates = immediates1; },
    Either { immediates = immediates2;
             immediates_extension = _;
             blocks;
             blocks_extension;
             is_unique;
           }
  | Either { immediates = immediates1;
             immediates_extension = _;
             blocks;
             blocks_extension;
             is_unique;
           },
    Immediates { immediates = immediates2; } ->
    let immediates = join_immediates env immediates1 immediates2 in
    (* See CR above about extensions *)
    let immediates_extension = TEE.empty () in
    Either { immediates;
             immediates_extension;
             blocks;
             blocks_extension;
             is_unique;
           }
  | Blocks { blocks = blocks1; is_unique = is_unique1; },
    Either { immediates;
             immediates_extension;
             blocks = blocks2;
             blocks_extension = _;
             is_unique = is_unique2;
           }
  | Either { immediates;
             immediates_extension;
             blocks = blocks1;
             blocks_extension = _;
             is_unique = is_unique1;
           },
    Blocks { blocks = blocks2; is_unique = is_unique2 } ->
    let is_unique = is_unique1 || is_unique2 in
    let blocks = join_blocks env blocks1 blocks2 in
    (* See CR above about extensions *)
    let blocks_extension = TEE.empty () in
    Either { immediates;
             immediates_extension;
             blocks;
             blocks_extension;
             is_unique;
           }
  | Either { immediates = immediates1;
             immediates_extension = imms_extension1;
             blocks = blocks1;
             blocks_extension = blocks_extension1;
             is_unique = is_unique1;
           },
    Either { immediates = immediates2;
             immediates_extension = imms_extension2;
             blocks = blocks2;
             blocks_extension = blocks_extension2;
             is_unique = is_unique2;
           } ->
    let is_unique = is_unique1 || is_unique2 in
    let immediates = join_immediates env immediates1 immediates2 in
    let blocks = join_blocks env blocks1 blocks2 in
    let immediates_extension =
      TEE.join env imms_extension1 imms_extension2
    in
    let blocks_extension =
      TEE.join env blocks_extension1 blocks_extension2
    in
    Either { immediates;
             immediates_extension;
             blocks;
             blocks_extension;
             is_unique;
           }

let apply_renaming t perm =
  match t with
  | Immediates { immediates; } ->
    let immediates' =
      Or_unknown.map_sharing immediates ~f:(fun immediates ->
        T.apply_renaming immediates perm)
    in
    if immediates == immediates' then t
    else Immediates { immediates = immediates'; }
  | Blocks { blocks; is_unique; } ->
    let blocks' =
      Or_unknown.map_sharing blocks ~f:(fun blocks ->
        B.apply_renaming blocks perm)
    in
    if blocks == blocks' then t
    else Blocks { blocks = blocks'; is_unique; }
  | Either {
      immediates;
      immediates_extension;
      blocks;
      blocks_extension;
      is_unique;
    } ->
    let immediates' =
      Or_unknown.map_sharing immediates ~f:(fun immediates ->
        T.apply_renaming immediates perm)
    in
    let blocks' =
      Or_unknown.map_sharing blocks ~f:(fun blocks ->
        B.apply_renaming blocks perm)
    in
    let immediates_extension' =
      TEE.apply_renaming immediates_extension perm
    in
    let blocks_extension' =
      TEE.apply_renaming blocks_extension perm
    in
    if immediates == immediates'
       && blocks == blocks'
       && immediates_extension == immediates_extension'
       && blocks_extension == blocks_extension'
    then t
    else Either {
        immediates = immediates';
        immediates_extension = immediates_extension';
        blocks = blocks';
        blocks_extension = blocks_extension';
        is_unique;
      }

let free_names t =
  match t with
  | Immediates { immediates; } ->
    Or_unknown.free_names T.free_names immediates
  | Blocks { blocks; is_unique = _; } ->
    Or_unknown.free_names B.free_names blocks
  | Either {
      immediates;
      immediates_extension;
      blocks;
      blocks_extension;
      is_unique = _;
    } ->
    Name_occurrences.union
      (Name_occurrences.union
         (Or_unknown.free_names B.free_names blocks)
         (Or_unknown.free_names T.free_names immediates))
      (Name_occurrences.union
         (TEE.free_names immediates_extension)
         (TEE.free_names blocks_extension))

let all_ids_for_export t =
  match t with
  | Immediates { immediates; } ->
    Or_unknown.all_ids_for_export T.all_ids_for_export immediates
  | Blocks { blocks; is_unique = _; } ->
    Or_unknown.all_ids_for_export B.all_ids_for_export blocks
  | Either {
      immediates;
      immediates_extension;
      blocks;
      blocks_extension;
      is_unique = _;
    } ->
    Ids_for_export.union
      (Ids_for_export.union
         (Or_unknown.all_ids_for_export B.all_ids_for_export blocks)
         (Or_unknown.all_ids_for_export T.all_ids_for_export immediates))
      (Ids_for_export.union
         (TEE.all_ids_for_export immediates_extension)
         (TEE.all_ids_for_export blocks_extension))

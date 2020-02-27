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

module Int = Numbers.Int
module TEE = Typing_env_extension

module Make
  (Tag : Identifiable.S)
  (Index : sig
     include Identifiable.S
     val union : t -> t -> t
     val inter : t -> t -> t
     val subset : t -> t -> bool
     (** [subset a b] is true if [a] is a subset of [b] *)
  end)
  (Maps_to : Row_like_maps_to_intf.S
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type meet_or_join_env := Meet_or_join_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct

  (* Note: it wouldn't require much changes to change it to an interval:
     type index = { at_least : Index.t; at_most : Index.t }
     representing { x | at_least \subset x /\ x \subset at_most }
  *)
  type index =
    | Known of Index.t (** Known x represents the singleton set: { x } *)
    | At_least of Index.t (** At_least x represents the set { y | x \subset y } *)

  type case = { maps_to : Maps_to.t; index : index }

  type t = {
    known_tags : case Tag.Map.t;
    other_tags : case Or_bottom.t;
  }

  let is_bottom { known_tags; other_tags } =
    Tag.Map.is_empty known_tags
    && other_tags = Or_bottom.Bottom

  let print_index ppf = function
    | Known index ->
      Format.fprintf ppf "(Known @[<2>%a@])"
        Index.print index
    | At_least min_index ->
      Format.fprintf ppf "(At_least @[<2>%a@])"
        Index.print min_index

  let print_with_cache ~cache ppf (({ known_tags; other_tags } as t) : t) =
    if is_bottom t then
      (* CR mshinwell: factor out (also in [Type_descr]) *)
      let colour = Flambda_colours.top_or_bottom_type () in
      if !Clflags.flambda2_unicode then
        Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s"
          colour (Flambda_colours.normal ())
      else
        Format.fprintf ppf "%s_|_%s" colour (Flambda_colours.normal ())
    else
      let print ppf { maps_to; index } =
        Format.fprintf ppf "=> %a,@ %a"
          print_index index
          (Maps_to.print_with_cache ~cache) maps_to
      in
      Format.fprintf ppf
        "@[<hov 1>(\
           @[<hov 1>(known_tags@ %a)@]@ \
           @[<hov 1>(other_tags@ %a)@]\
           )@]"
        (Tag.Map.print print) known_tags
        (Or_bottom.print print) other_tags

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let _invariant _t = ()

  let create_bottom () =
    { known_tags = Tag.Map.empty;
      other_tags = Bottom;
    }

  let create_exactly tag index maps_to =
    { known_tags = Tag.Map.singleton tag { maps_to; index = Known index };
      other_tags = Bottom;
    }

  (* let create_exactly_multiple known =
   *   (\* { known_tags = Tag.Map.map (fun closure_entry -> assert false) known;
   *    *   other_tags = Bottom;
   *    * } *\)
   *   () *)

  let create_at_least tag index maps_to =
    { known_tags = Tag.Map.singleton tag { maps_to; index = At_least index };
      other_tags = Bottom;
    }

  (* let create_exactly_unknown_tag index maps_to =
   *   { known_tags = Tag.Map.empty;
   *     other_tags = Ok { maps_to; index = Known index };
   *   } *)

  let create_at_least_unknown_tag index maps_to =
    { known_tags = Tag.Map.empty;
      other_tags = Ok { maps_to; index = At_least index };
    }

  (* let create_at_least_multiple at_least = ()
   *   (\* { known_tags = Tag.Map.map (fun  { maps_to; index = At_least index };
   *    *   other_tags = Bottom;
   *    * } *\) *)

  let meet (meet_env : Meet_env.t) t1 t2 : (t * Typing_env_extension.t) Or_bottom.t =
    let ({ known_tags = known1; other_tags = other1; } : t) = t1 in
    let ({ known_tags = known2; other_tags = other2; } : t) = t2 in
    let join_env = Meet_or_join_env.create_for_meet meet_env in
    let env_extension = ref None in
    let join_env_extension ext =
      match !env_extension with
      | None -> env_extension := Some ext
      | Some ext2 ->
        env_extension := Some (TEE.join ~params:[] join_env ext2 ext)
    in
    let meet_index i1 i2 : index Or_bottom.t =
      match i1, i2 with
      | Known i1', Known i2' ->
        if Index.equal i1' i2' then
          Ok i1
        else
          Bottom
      | Known known, At_least at_least
      | At_least at_least, Known known ->
        if Index.subset at_least known then
          (* [at_least] is included in [known] hence
             [Known known] is included in [At_least at_least], hence
             [Known known] \inter [At_least at_least] = [Known known] *)
          Ok (Known known)
        else
          Bottom
      | At_least i1', At_least i2' ->
        Ok (At_least (Index.union i1' i2'))
    in
    let meet_case case1 case2 =
      match meet_index case1.index case2.index with
      | Bottom -> None
      | Ok index ->
        match Maps_to.meet meet_env case1.maps_to case2.maps_to with
        | Bottom -> None
        | Ok (maps_to, env_extension') ->
          join_env_extension env_extension';
          Some { maps_to; index }
    in
    let meet_knowns_tags case1 case2 : case option =
      match case1, case2 with
      | None, None -> None
      | Some case1, None -> begin
          match other2 with
          | Bottom -> None
          | Ok other_case ->
            meet_case case1 other_case
        end
      | None, Some case2 -> begin
          match other1 with
          | Bottom -> None
          | Ok other_case ->
            meet_case other_case case2
        end
      | Some case1, Some case2 ->
        meet_case case1 case2
    in
    let known_tags =
      Tag.Map.merge (fun _tag case1 case2 -> meet_knowns_tags case1 case2)
        known1 known2
    in
    let other_tags : case Or_bottom.t =
      match other1, other2 with
      | Bottom, _ | _, Bottom -> Bottom
      | Ok other1, Ok other2 ->
        match meet_case other1 other2 with
        | None -> Bottom
        | Some r -> Ok r
    in
    let result = { known_tags; other_tags } in
    if is_bottom result then
      Bottom
    else
      let env_extension =
        match !env_extension with
        | None -> assert false (* This should be bottom *)
        | Some ext -> ext
      in
      Ok (result, env_extension)

    let join (env : Meet_or_join_env.t) t1 t2 : t =
      let ({ known_tags = known1; other_tags = other1; } : t) = t1 in
      let ({ known_tags = known2; other_tags = other2; } : t) = t2 in
      let join_index i1 i2 : index =
        match i1, i2 with
        | Known i1', Known i2' ->
          if Index.equal i1' i2' then
            i1
          else
            (* We can't represent exactly the union,
               This is the best approximation *)
            At_least (Index.inter i1' i2')
        | Known i1', At_least i2'
        | At_least i1', Known i2'
        | At_least i1', At_least i2' ->
          At_least (Index.inter i1' i2')
      in
      let join_case env case1 case2 =
        let index = join_index case1.index case2.index in
        let maps_to = Maps_to.join env case1.maps_to case2.maps_to in
        { maps_to; index }
      in
      let join_knowns_tags case1 case2 : case option =
        match case1, case2 with
        | None, None -> None
        | Some case1, None -> begin
            match other2 with
            | Bottom ->
              (* CF Type_descr.join_head_or_unknown_or_bottom,
                 we need to join those to ensure that free variables not
                 present in the target env are cleaned out of the types.
                 Same bellow *)
              (* CR pchambart: This seams terribly inefficient. *)
              let env =
                Meet_or_join_env.create_for_join
                  (Meet_or_join_env.target_join_env env)
                  ~left_env:(Meet_or_join_env.left_join_env env)
                  ~right_env:(Meet_or_join_env.left_join_env env)
              in
              let case1 = join_case env case1 case1 in
              Some case1
            | Ok other_case ->
              Some (join_case env case1 other_case)
          end
        | None, Some case2 -> begin
            match other1 with
            | Bottom ->
              (* See at the other bottom case *)
              let env =
                Meet_or_join_env.create_for_join
                  (Meet_or_join_env.target_join_env env)
                  ~left_env:(Meet_or_join_env.right_join_env env)
                  ~right_env:(Meet_or_join_env.right_join_env env)
              in
              let case2 = join_case env case2 case2 in
              Some case2
            | Ok other_case ->
              Some (join_case env other_case case2)
          end
        | Some case1, Some case2 ->
          Some (join_case env case1 case2)
      in
      let known_tags =
          Tag.Map.merge (fun _tag case1 case2 -> join_knowns_tags case1 case2)
            known1 known2
        in
        let other_tags : case Or_bottom.t =
          match other1, other2 with
          | Bottom, Bottom ->
            Bottom
          | Ok other1, Bottom ->
            (* See the previous cases *)
            let env =
              Meet_or_join_env.create_for_join
                (Meet_or_join_env.target_join_env env)
                ~left_env:(Meet_or_join_env.left_join_env env)
                ~right_env:(Meet_or_join_env.left_join_env env)
            in
            let other1 = join_case env other1 other1 in
            Ok other1
          | Bottom, Ok other2 ->
            (* See the previous cases *)
            let env =
              Meet_or_join_env.create_for_join
                (Meet_or_join_env.target_join_env env)
                ~left_env:(Meet_or_join_env.right_join_env env)
                ~right_env:(Meet_or_join_env.right_join_env env)
            in
            let other2 = join_case env other2 other2 in
            Ok other2
          | Ok other1, Ok other2 ->
            Ok (join_case env other1 other2)
        in
        { known_tags; other_tags }

      let get_singleton { known_tags; other_tags; } =
        match other_tags with
        | Ok _ -> None
        | Bottom ->
          match Tag.Map.get_singleton known_tags with
          | None -> None
          | Some (tag, { maps_to; index }) ->
            match index with
            | At_least _ -> None
            | Known index ->
              Some ((tag, index), maps_to)

      let all_tags { known_tags; other_tags; } : Tag.Set.t Or_unknown.t =
        match other_tags with
        | Ok _ -> Unknown
        | Bottom -> Known (Tag.Map.keys known_tags)

      let all_tags_and_indexes { known_tags; other_tags; } : _ Or_unknown.t =
        match other_tags with
        | Ok _ -> Unknown
        | Bottom -> Known (Tag.Map.map (fun case -> case.index) known_tags)

      let free_names { known_tags; other_tags; } =
        let from_known_tags =
          Tag.Map.fold (fun _tag { maps_to; index = _ } free_names ->
              Name_occurrences.union free_names
                (Maps_to.free_names maps_to))
            known_tags
            Name_occurrences.empty
        in
        match other_tags with
        | Bottom ->
          from_known_tags
        | Ok { maps_to; index = _ } ->
          Name_occurrences.union
            (Maps_to.free_names maps_to)
            from_known_tags

    let map_maps_to { known_tags; other_tags; }
          ~(f : Maps_to.t -> Maps_to.t Or_bottom.t)
          : _ Or_bottom.t =
      let known_tags =
        Tag.Map.filter_map (fun _ case ->
            match f case.maps_to with
            | Bottom ->
              None
            | Ok maps_to ->
              Some { case with maps_to })
          known_tags
      in
      let other_tags : case Or_bottom.t =
        match other_tags with
        | Bottom -> Bottom
        | Ok case ->
          Or_bottom.map (f case.maps_to) ~f:(fun maps_to -> { case with maps_to })
      in
      let result = {
        known_tags;
        other_tags;
      } in
      if is_bottom result then Bottom
      else Ok result

    let apply_name_permutation ({ known_tags; other_tags; } as t) perm =
      let known_tags' =
        Tag.Map.map_sharing (fun { index; maps_to } ->
            { index; maps_to = Maps_to.apply_name_permutation maps_to perm })
          known_tags
      in
      let other_tags' : _ Or_bottom.t =
        match other_tags with
        | Bottom -> Bottom
        | Ok { index; maps_to } ->
          Ok { index; maps_to = Maps_to.apply_name_permutation maps_to perm }
      in
      if known_tags == known_tags' && other_tags == other_tags' then t
      else
        { known_tags = known_tags';
          other_tags = other_tags';
        }

  end

  module Targetint_ocaml_index = struct
    include Targetint.OCaml
    let subset t1 t2 = Stdlib.(<=) (compare t1 t2) 0
    (* An integer [i] represents all the values smaller than i, hence
      a smaller number is included in a bigger *)
    let union t1 t2 = Targetint.OCaml.max t1 t2
    let inter t1 t2 = Targetint.OCaml.min t1 t2
  end

  module For_blocks = struct
    module Tag_or_unknown = Tag_or_unknown_and_size.Tag_or_unknown
    include Make (Tag) (Targetint_ocaml_index) (Product.Int_indexed)

    type open_or_closed = Open of Tag.t Or_unknown.t | Closed of Tag.t

    let create ~(field_kind : Flambda_kind.t) ~field_tys
          (open_or_closed : open_or_closed) =
      let field_kind' =
        List.map Type_grammar.kind field_tys
        |> Flambda_kind.Set.of_list
        |> Flambda_kind.Set.get_singleton
      in
      (* CR pchambart: move to invariant check *)
      begin match field_kind' with
      | None ->
        if List.length field_tys <> 0 then begin
          Misc.fatal_error "[field_tys] must all be of the same kind"
        end
      | Some field_kind' ->
        if not (Flambda_kind.equal field_kind field_kind') then begin
          Misc.fatal_errorf "Declared field kind %a doesn't match [field_tys]"
            Flambda_kind.print field_kind
        end
      end;

      let tag : _ Or_unknown.t =
        let tag : _ Or_unknown.t =
          match open_or_closed with
          | Open (Known tag) -> Known tag
          | Open Unknown -> Unknown
          | Closed tag -> Known tag
        in
        match tag with
        | Unknown ->
          begin match field_kind with
          | Value -> Unknown
          | Naked_number Naked_float -> Known Tag.double_array_tag
          | Naked_number Naked_immediate | Naked_number Naked_int32
          | Naked_number Naked_int64 | Naked_number Naked_nativeint
          | Fabricated ->
            Misc.fatal_errorf "Bad kind %a for fields"
              Flambda_kind.print field_kind
          end
        | Known tag ->
          begin match field_kind with
          | Value ->
            begin match Tag.Scannable.of_tag tag with
            | Some _ -> Known tag
            | None ->
              Misc.fatal_error "Blocks full of [Value]s must have a tag \
                less than [No_scan_tag]"
            end
          | Naked_number Naked_float ->
            if not (Tag.equal tag Tag.double_array_tag) then begin
              Misc.fatal_error "Blocks full of naked floats must have tag \
                [Tag.double_array_tag]"
            end;
            Known tag
          | Naked_number Naked_immediate | Naked_number Naked_int32
          | Naked_number Naked_int64 | Naked_number Naked_nativeint
          | Fabricated ->
            Misc.fatal_errorf "Bad kind %a for fields"
              Flambda_kind.print field_kind
          end
      in
      let product = Product.Int_indexed.create_from_list field_kind field_tys in
      let size = Targetint.OCaml.of_int (List.length field_tys) in
      match open_or_closed with
      | Open _ -> begin
        match tag with
        | Known tag -> create_at_least tag size product
        | Unknown -> create_at_least_unknown_tag size product
      end
      | Closed _ ->
        match tag with
        | Known tag -> create_exactly tag size product
        | Unknown -> assert false  (* see above *)

    let create_blocks_with_these_tags ~field_kind tags =
      let maps_to = Product.Int_indexed.create_top field_kind in
      let case = { maps_to; index = At_least Targetint.OCaml.zero } in
      { known_tags = Tag.Map.of_set (fun _ -> case) tags;
        other_tags = Bottom;
      }

    let all_tags_and_sizes t : Targetint.OCaml.t Tag.Map.t Or_unknown.t =
      match all_tags_and_indexes t with
      | Unknown -> Unknown
      | Known tags_and_indexes ->
        let any_unknown = ref false in
        let by_tag =
          Tag.Map.map (fun index ->
              match index with
              | Known index -> index
              | At_least index ->
                any_unknown := true;
                index)
            tags_and_indexes
        in
        if !any_unknown then
          Unknown
        else
          Known by_tag

  end

  module For_closures_entry_by_set_of_closures_contents = struct

    include Make (Closure_id) (Set_of_closures_contents)
      (Closures_entry)

    let map_function_decl_types t ~f =
      map_maps_to t ~f:(fun closures_entry ->
          Closures_entry.map_function_decl_types closures_entry ~f)

    let create_exactly
        (closure_id : Closure_id.t)
        (contents : Set_of_closures_contents.t)
        (closures_entry : Closures_entry.t) : t =
      let known_tags =
        Closure_id.Map.singleton closure_id
          { index = Known contents; maps_to = closures_entry }
      in
      { known_tags;
        other_tags = Bottom;
      }

    let create_at_least
        (closure_id : Closure_id.t)
        (contents : Set_of_closures_contents.t)
        (closures_entry : Closures_entry.t) : t =
      let known_tags =
        Closure_id.Map.singleton closure_id
          { index = At_least contents; maps_to = closures_entry }
      in
      { known_tags;
        other_tags = Bottom;
      }

  end

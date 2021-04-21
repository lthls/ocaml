(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Args = struct
  type t = {
    max_inlining_depth : int;
    call_cost : float;
    alloc_cost : float;
    prim_cost : float;
    branch_cost : float;
    indirect_call_cost : float;
    poly_compare_cost : float;
    small_function_size : int;
    large_function_size : int;
    threshold : float;
  }

  let print ppf t =
    let { max_inlining_depth; call_cost; alloc_cost; prim_cost; branch_cost;
          indirect_call_cost; poly_compare_cost;
          small_function_size; large_function_size;
          threshold;
        } = t
    in
    Format.fprintf ppf
      "@[<hov 1>(\
       @[<hov 1>(max_inlining_depth@ %d)@]@ \
       @[<hov 1>(call_cost@ %f)@]@ \
       @[<hov 1>(alloc_cost@ %f)@]@ \
       @[<hov 1>(prim_cost@ %f)@]@ \
       @[<hov 1>(branch_cost@ %f)@]@ \
       @[<hov 1>(indirect_call_cost@ %f)@]@ \
       @[<hov 1>(poly_compare_cost@ %f)@]@ \
       @[<hov 1>(small_function_size@ %d)@]@ \
       @[<hov 1>(large_function_size@ %d)@]@ \
       @[<hov 1>(threshold@ %f)@]\
      )@]"
      max_inlining_depth
      call_cost
      alloc_cost
      prim_cost
      branch_cost
      indirect_call_cost
      poly_compare_cost
      small_function_size
      large_function_size
      threshold

  let equal t1 t2 =
    let {
      max_inlining_depth = t1_max_inlining_depth;
      call_cost = t1_call_cost;
      alloc_cost = t1_alloc_cost;
      prim_cost = t1_prim_cost;
      branch_cost = t1_branch_cost;
      indirect_call_cost = t1_indirect_call_cost;
      poly_compare_cost = t1_poly_compare_cost;
      small_function_size = t1_small_function_size;
      large_function_size = t1_large_function_size;
      threshold = t1_threshold;
    } = t1
    in
    let {
      max_inlining_depth = t2_max_inlining_depth;
      call_cost = t2_call_cost;
      alloc_cost = t2_alloc_cost;
      prim_cost = t2_prim_cost;
      branch_cost = t2_branch_cost;
      indirect_call_cost = t2_indirect_call_cost;
      poly_compare_cost = t2_poly_compare_cost;
      small_function_size = t2_small_function_size;
      large_function_size = t2_large_function_size;
      threshold = t2_threshold;
    } = t2
    in
    t1_max_inlining_depth = t2_max_inlining_depth
    && Float.compare t1_call_cost t2_call_cost = 0
    && Float.compare t1_alloc_cost t2_alloc_cost = 0
    && Float.compare t1_prim_cost t2_prim_cost = 0
    && Float.compare t1_branch_cost t2_branch_cost = 0
    && Float.compare t1_indirect_call_cost t2_indirect_call_cost = 0
    && Float.compare t1_poly_compare_cost t2_poly_compare_cost = 0
    && t1_small_function_size = t2_small_function_size
    && t1_large_function_size = t2_large_function_size
    && Float.compare t1_threshold t2_threshold = 0

  let (<=) t1 t2 =
    (* The comparison of two [Args.t] is a pointwise comparison. It is a
       partial order so it may happen that both [t1 <= t2] and [t2 <= t1] are
       false. For example we could have:
       t1 = { call_cost = 2; alloc_cost = 2 }
       t2 = { call_cost = 4; alloc_cost = 1 }
       In that case [(<=) t1 t2 = false] as [t1.alloc_cost > t2.alloc_cost] and
       [(<=) t2 t1 = false] as [t2.call_cost > t1.call_cost]
    *)
    let {
      max_inlining_depth = t1_max_inlining_depth;
      call_cost = t1_call_cost;
      alloc_cost = t1_alloc_cost;
      prim_cost = t1_prim_cost;
      branch_cost = t1_branch_cost;
      indirect_call_cost = t1_indirect_call_cost;
      poly_compare_cost = t1_poly_compare_cost;
      small_function_size = t1_small_function_size;
      large_function_size = t1_large_function_size;
      threshold = t1_threshold;
    } = t1
    in
    let {
      max_inlining_depth = t2_max_inlining_depth;
      call_cost = t2_call_cost;
      alloc_cost = t2_alloc_cost;
      prim_cost = t2_prim_cost;
      branch_cost = t2_branch_cost;
      indirect_call_cost = t2_indirect_call_cost;
      poly_compare_cost = t2_poly_compare_cost;
      small_function_size = t2_small_function_size;
      large_function_size = t2_large_function_size;
      threshold = t2_threshold;
    } = t2
    in
    t1_max_inlining_depth <= t2_max_inlining_depth
    && Float.compare t1_call_cost t2_call_cost <= 0
    && Float.compare t1_alloc_cost t2_alloc_cost <= 0
    && Float.compare t1_prim_cost t2_prim_cost <= 0
    && Float.compare t1_branch_cost t2_branch_cost <= 0
    && Float.compare t1_indirect_call_cost t2_indirect_call_cost <= 0
    && Float.compare t1_poly_compare_cost t2_poly_compare_cost <= 0
    && t1_small_function_size <= t2_small_function_size
    && t1_large_function_size <= t2_large_function_size
    && Float.compare t1_threshold t2_threshold <= 0

  let meet t1 t2 =
    let {
      max_inlining_depth = t1_max_inlining_depth;
      call_cost = t1_call_cost;
      alloc_cost = t1_alloc_cost;
      prim_cost = t1_prim_cost;
      branch_cost = t1_branch_cost;
      indirect_call_cost = t1_indirect_call_cost;
      poly_compare_cost = t1_poly_compare_cost;
      small_function_size = t1_small_function_size;
      large_function_size = t1_large_function_size;
      threshold = t1_threshold;
    } = t1
    in
    let {
      max_inlining_depth = t2_max_inlining_depth;
      call_cost = t2_call_cost;
      alloc_cost = t2_alloc_cost;
      prim_cost = t2_prim_cost;
      branch_cost = t2_branch_cost;
      indirect_call_cost = t2_indirect_call_cost;
      poly_compare_cost = t2_poly_compare_cost;
      small_function_size = t2_small_function_size;
      large_function_size = t2_large_function_size;
      threshold = t2_threshold;
    } = t2
    in
    {
      max_inlining_depth = min t1_max_inlining_depth t2_max_inlining_depth;
      call_cost = Float.min t1_call_cost t2_call_cost;
      alloc_cost = Float.min t1_alloc_cost t2_alloc_cost;
      prim_cost = Float.min t1_prim_cost t2_prim_cost;
      branch_cost = Float.min t1_branch_cost t2_branch_cost;
      indirect_call_cost = Float.min t1_indirect_call_cost t2_indirect_call_cost;
      poly_compare_cost = Float.min t1_poly_compare_cost t2_poly_compare_cost;
      small_function_size = min t1_small_function_size t2_small_function_size;
      large_function_size = min t1_large_function_size t2_large_function_size;
      threshold = Float.min t1_threshold t2_threshold
    }


  let cost_i (flag : Clflags.Int_arg_helper.parsed) ~round =
    Clflags.Int_arg_helper.get ~key:round flag

  let cost_f (flag : Clflags.Float_arg_helper.parsed) ~round =
    Clflags.Float_arg_helper.get ~key:round flag

  let create ~round = {
    max_inlining_depth = !Clflags.Flambda.Expert.max_inlining_depth;
    call_cost = cost_f !Clflags.inline_call_cost ~round;
    alloc_cost = cost_f !Clflags.inline_alloc_cost ~round;
    prim_cost = cost_f !Clflags.inline_prim_cost ~round;
    branch_cost = cost_f !Clflags.inline_branch_cost ~round;
    indirect_call_cost = cost_f !Clflags.inline_indirect_call_cost ~round;
    poly_compare_cost = cost_f !Clflags.inline_poly_compare_cost ~round;
    small_function_size = cost_i !Clflags.inline_small_function_size ~round;
    large_function_size = cost_i !Clflags.inline_large_function_size ~round;
    threshold = cost_f !Clflags.inline_threshold ~round;
  }
end

type t = Args.t Or_unknown.t

let unknown = Or_unknown.Unknown

let print ppf = Or_unknown.print Args.print ppf

let get_or_fail t : Args.t =
  match t with
  | Or_unknown.Unknown ->
    Misc.fatal_errorf
      "Trying to access an unknown set of inliner arguments. This should not \
       happen, usually [meet] should have been called with a known set of \
       arguments by this point."
  | Or_unknown.Known s -> s

let max_inlining_depth t = (get_or_fail t).max_inlining_depth
let call_cost t = (get_or_fail t).call_cost
let alloc_cost t = (get_or_fail t).alloc_cost
let prim_cost t = (get_or_fail t).prim_cost
let branch_cost t = (get_or_fail t).branch_cost
let indirect_call_cost t = (get_or_fail t).indirect_call_cost
let poly_compare_cost t = (get_or_fail t).poly_compare_cost
let small_function_size t = (get_or_fail t).small_function_size
let large_function_size t = (get_or_fail t).large_function_size
let threshold t = (get_or_fail t).threshold

let meet (t1 : _ Or_unknown.t) (t2 : _ Or_unknown.t) : t =
  match t1, t2 with
  | Unknown, Unknown -> Unknown
  | Known _, Unknown -> t1
  | Unknown, Known _ -> t2
  | Known args1, Known args2 ->
    (* If we are sure that args1 is lower than args2 then
       [meet args1 args2 = args1]. In that case we can avoid calling
       [Args.meet] and reuse args1 to avoid having to allocate a new Args.t.
       The same goes if the are sure that args2 is lower than args1.
    *)
    if Args.(<=) args1 args2 then
      t1
    else if Args.(<=) args2 args1 then
      t2
    else
      Known (Args.meet args1 args2)

let create ~round : t = Known (Args.create ~round)

let equal t1 t2 = Or_unknown.equal Args.equal t1 t2

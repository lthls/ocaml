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

(** Conversions of basic Lambda data types to their Flambda equivalents. *)

val value_kind : Lambda.value_kind -> Flambda_kind.t

val inline_attribute
   : Lambda.inline_attribute
  -> Inline_attribute.t

val kind_of_primitive_native_repr : Primitive.native_repr -> Flambda_kind.t

val method_kind : Lambda.meth_kind -> Call_kind.method_kind

val raise_kind : Lambda.raise_kind -> Trap_action.raise_kind

val convert_block_shape
   : Lambda.block_shape
  -> num_fields:int
  -> Flambda_primitive.Value_kind.t list

val convert_mutable_flag : Asttypes.mutable_flag -> Effects.mutable_or_immutable

val convert_integer_comparison_prim
   : Lambda.integer_comparison
  -> Flambda_primitive.binary_primitive

val convert_boxed_integer_comparison_prim
   : Lambda.boxed_integer
  -> Lambda.integer_comparison
  -> Flambda_primitive.binary_primitive

val convert_float_comparison
   : Lambda.float_comparison
  -> Flambda_primitive.comparison

val boxable_number_of_boxed_integer
   : Lambda.boxed_integer
  -> Flambda_kind.Boxable_number.t

val standard_int_of_boxed_integer
   : Lambda.boxed_integer
  -> Flambda_kind.Standard_int.t

val standard_int_or_float_of_boxed_integer
   : Lambda.boxed_integer
  -> Flambda_kind.Standard_int_or_float.t

val convert_access_kind
   : Lambda.immediate_or_pointer
  -> Flambda_primitive.Block_access_kind.t0

val convert_init_or_assign
   : Lambda.initialization_or_assignment
  -> Flambda_primitive.init_or_assign

val convert_array_kind
   : Lambda.array_kind
  -> Flambda_primitive.Block_access_kind.t

val convert_array_kind_to_duplicate_block_kind
   : Lambda.array_kind
  -> Flambda_primitive.duplicate_block_kind

val convert_bigarray_kind
   : Lambda.bigarray_kind
  -> Flambda_primitive.bigarray_kind

val convert_bigarray_layout
   : Lambda.bigarray_layout
  -> Flambda_primitive.bigarray_layout

val convert_field_read_semantics
   : Lambda.field_read_semantics
  -> Effects.mutable_or_immutable

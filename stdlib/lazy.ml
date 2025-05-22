(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Lazy]: deferred computations *)


(*
   WARNING: some purple magic is going on here.  Do not take this file
   as an example of how to program in OCaml.
*)


(* We make use of two special tags provided by the runtime:
   [lazy_tag] and [forward_tag].

   A value of type ['a Lazy.t] can be one of three things:
   1. A block of size 1 with tag [lazy_tag].  Its field is a closure of
      type [unit -> 'a] that computes the value.
   2. A block of size 1 with tag [forward_tag].  Its field is the value
      of type ['a] that was computed.
   3. Anything else except a float.  This has type ['a] and is the value
      that was computed.
   Exceptions are stored in format (1).
   The GC will magically change things from (2) to (3) according to its
   fancy.

   If OCaml was configured with the -flat-float-array option (which is
   currently the default), the following is also true:
   We cannot use representation (3) for a [float Lazy.t] because
   [caml_array_make] assumes that only a [float] value can have tag
   [Double_tag].

   We have to use the built-in type constructor [lazy_t] to
   let the compiler implement the special typing and compilation
   rules for the [lazy] keyword.
*)

type 'a t = 'a CamlinternalLazy.t

exception Undefined = CamlinternalLazy.Undefined
external make_forward : 'a -> 'a lazy_t = "caml_lazy_make_forward"
external force : 'a t -> 'a = "%lazy_force"

let force_val l = CamlinternalLazy.force_gen ~only_val:true l

let from_fun (f : unit -> 'arg) =
  let x = Obj.new_block Obj.lazy_tag 1 in
  Obj.set_field x 0 (Obj.repr f);
  (Obj.obj x : 'arg t)

let from_val (v : 'arg) =
  let t = Obj.tag (Obj.repr v) in
  if t = Obj.forward_tag || t = Obj.lazy_tag ||
     t = Obj.forcing_tag || t = Obj.double_tag then begin
    make_forward v
  end else begin
    (Obj.magic v : 'arg t)
  end

let is_val (l : 'arg t) = Obj.tag (Obj.repr l) <> Obj.lazy_tag

let map f x =
  lazy (f (force x))

let map_val f x =
  if is_val x
  then from_val (f (force x))
  else lazy (f (force x))



module Atomic_repeating = struct
  (* we define these as primitives to avoid a dependency on Printexc *)
  type raw_backtrace
  external get_raw_backtrace:
    unit -> raw_backtrace = "caml_get_exception_raw_backtrace"
  external raise_with_backtrace: exn -> raw_backtrace -> 'a
    = "%raise_with_backtrace"

  type 'a ops = {
    make : unit -> 'a;
    wait : unit -> unit;
    broadcast : unit -> unit
  }

  type race_behaviour =
    | Busy_wait
    | Synchronise of { wait : unit -> unit; broadcast : unit -> unit }
    | Fail

  type 'a state =
    | Thunk of 'a ops
    | Forcing of 'a ops
    | Val of 'a
    | Failed of exn * raw_backtrace

  type 'a t = 'a state Atomic.t

  let from_val v = Atomic.make (Val v)
  let from_fun ?(race_behaviour = Fail) f =
    let wait, broadcast =
      match race_behaviour with
      | Busy_wait ->
          Fun.id, Fun.id
      | Synchronise { wait; broadcast } -> wait, broadcast
      | Fail -> (fun () -> raise Undefined), Fun.id
    in
    Atomic.make (Thunk { make = f; wait; broadcast })

  let rec force th =
    match Atomic.get th with
    | Val v -> v
    | Failed (exn, bt) ->
      raise_with_backtrace exn bt
    | (Thunk ops) as thunk ->
      (* [compare_and_set] returns [false] when another domain has
         set the thunk to [Forcing] or a finished state. *)
      if Atomic.compare_and_set th thunk (Forcing ops)
      then begin
        match ops.make () with
        | exception exn ->
          let bt = get_raw_backtrace () in
          let failed = Failed (exn, bt) in
          (* [compare_and_set] cannot return false, as only the thread that
             managed to set to forcing can try to update it again. *)
          ignore (Atomic.compare_and_set th forcing failed);
          ops.broadcast ();
          raise_with_backtrace exn bt
        | v ->
          (* [compare_and_set] cannot return false, as only the thread that
             managed to set to forcing can try to update it again. *)
          ignore (Atomic.compare_and_set th forcing (Val v));
          v
      end
      else force th
    | (Forcing ops) as forcing ->
      ops.wait ();
      force th

  let rec force_non_blocking th =
    match Atomic.get th with
    | Val v -> Some v
    | Failed (exn, bt) ->
      raise_with_backtrace exn bt
    | (Thunk ops) as thunk ->
      (* [compare_and_set] returns [false] when another domain has
         set the thunk to [Forcing] or a finished state. *)
      if Atomic.compare_and_set th thunk (Forcing ops)
      then begin
        match ops.make () with
        | exception exn ->
          let bt = get_raw_backtrace () in
          let failed = Failed (exn, bt) in
          (* [compare_and_set] cannot return false, as only the thread that
             managed to set to forcing can try to update it again. *)
          ignore (Atomic.compare_and_set th forcing failed);
          ops.broadcast ();
          raise_with_backtrace exn bt
        | v ->
          (* [compare_and_set] cannot return false, as only the thread that
             managed to set to forcing can try to update it again. *)
          ignore (Atomic.compare_and_set th forcing (Val v));
          ops.broadcast ();
          Some v
      end
      else force th
    | (Forcing ops) as forcing ->
      None
end

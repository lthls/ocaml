
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 OCamlPro SAS                                          *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Int = Numbers.Int

(* The following invariant is relied upon (and checked to a reasonable
   extent): all applications of a given continuation must be at the same
   trap depth.
*)

(* The environment is necessary to do alpha-renaming of continuations.
   Inlining routines in Closure can duplicate the same code (including
   static exceptions) into different contexts, leading to the same
   continuation being used at different trap depths. This can be compiled
   correctly by the rest of the backend, but interferes with the previous
   invariant. *)

module Env : sig
  type t
  type r

  val empty_t : t
  val empty_r : r

  (* Stack manipulation *)
  val pushtrap : t -> cont:int -> t
  val poptrap : t -> cont:int -> t
  val return : t -> unit
  val stack : t -> Mach.trap_stack

  (* Invariant checking *)
  val add_stack : r -> cont:int -> t -> r
  val register_raise : r -> t -> r
  val cont_is_used : r -> cont:int -> bool
  val stack_at_exit_exn : r -> cont:int -> Mach.trap_stack

  (* Renaming *)
  val add_binding : t -> r -> cont:int -> t * r
  val apply_subst : t -> cont:int -> int
end = struct
  type t =
    { stack : Mach.trap_stack;
      renaming : int Int.Map.t;
    }

  type r =
    { stacks_at_exit: Mach.trap_stack Int.Map.t;
      continuations_seen: Int.Set.t;
    }

  let empty_t =
    { stack = [];
      renaming = Int.Map.empty;
    }

  let empty_r =
    { stacks_at_exit = Int.Map.empty;
      continuations_seen = Int.Set.empty;
    }

  let print_stack ppf stack =
    Format.fprintf ppf "%a"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
        (fun ppf cont -> Format.fprintf ppf "%d" cont))
      stack

  let add_stack r ~cont t =
    match Int.Map.find cont r.stacks_at_exit with
    | exception Not_found ->
      { r with stacks_at_exit = Int.Map.add cont t.stack r.stacks_at_exit }
    | stack' ->
      if t.stack <> stack' then begin
        Misc.fatal_errorf "Iexit points for continuation %d disagree on \
            the trap stack: existing = %a new = %a"
          cont
          print_stack stack'
          print_stack t.stack
      end;
      r

  let register_raise r t =
    match t.stack with
    | [] -> r  (* raise to toplevel handler *)
    | cont::_ -> add_stack r ~cont t

  let cont_is_used r ~cont =
    Int.Map.mem cont r.stacks_at_exit

  let stack_at_exit_exn r ~cont =
    Int.Map.find cont r.stacks_at_exit

  let pushtrap t ~cont =
    { t with stack = cont :: t.stack }

  let poptrap t ~cont =
    match t.stack with
    | [] ->
        Misc.fatal_errorf "Tried to poptrap %d but trap stack is empty" cont
    | cont' :: stack ->
      if cont = cont' then
        { t with stack }
      else
        Misc.fatal_errorf "Tried to poptrap %d but trap stack has %d \
            at the top"
          cont cont'

  let return t =
    match t.stack with
    | [] -> ()
    | _ -> Misc.fatal_error "Trap depth at Ireturn is non-zero"

  let stack t = t.stack

  let add_binding t r ~cont =
    let cont' =
      if Int.Set.mem cont r.continuations_seen then
        Lambda.next_raise_count ()
      else
        cont
    in
    { t with renaming = Int.Map.add cont cont' t.renaming },
    { r with continuations_seen = Int.Set.add cont' r.continuations_seen }

  let apply_subst t ~cont =
    match Int.Map.find cont t.renaming with
    | cont' -> cont'
    | exception Not_found ->
        Misc.fatal_errorf "Trap_analysis.Env.apply_subst: \
            unknown continuation %d"
          cont
end

let rec trap_stacks (env: Env.t) (r: Env.r) (insn : Mach.instruction)
      : Mach.instruction * Env.r =
  match insn.Mach.desc with
  | Iend ->
    insn, r
  | Ireturn ->
    Env.return env;
    insn, r
  | Iop op ->
    let env, r, op =
      match op with
      | Ipushtrap cont ->
        (* CR pchambart: This shouldn't keep the handler alive. If
           there is no raise the handler should be eliminated. *)
        (* CR vlaviron: We can't remove the handler without removing
           pushtrap and poptrap instructions, which would require
           an additional pass. *)
        let cont = Env.apply_subst env ~cont in
        let env = Env.pushtrap env ~cont in
        env, Env.add_stack r ~cont env, Mach.Ipushtrap cont
      | Ipoptrap cont ->
        let cont = Env.apply_subst env ~cont in
        Env.poptrap env ~cont, r, Mach.Ipoptrap cont
      | _ -> env, r, op
    in
    let desc, r =
      match op with
      | Icall_ind call ->
        let r = Env.register_raise r env in
        Mach.Iop (Icall_ind ({ call with trap_stack = Env.stack env; })),
          r
      | Icall_imm call ->
        let r = Env.register_raise r env in
        Mach.Iop (Icall_imm ({ call with trap_stack = Env.stack env; })),
          r
      | Iextcall call ->
        let r = Env.register_raise r env in
        Mach.Iop (Iextcall ({ call with trap_stack = Env.stack env; })),
          r
      | Iintop (Icheckbound check) ->
        let r = Env.register_raise r env in
        Mach.Iop (Iintop (
            Icheckbound ({ check with trap_stack = Env.stack env; }))),
          r
      | Iintop_imm (Icheckbound check, i) ->
        let r = Env.register_raise r env in
        Mach.Iop (Iintop_imm (
            Icheckbound { check with trap_stack = Env.stack env; }, i)),
          r
      | Ialloc alloc ->
        let r = Env.register_raise r env in
        Mach.Iop (Ialloc ({ alloc with trap_stack = Env.stack env; })),
          r
      | _ -> Mach.Iop op, r
    in
    let next, r =
      trap_stacks env r insn.Mach.next
    in
    { insn with
      desc;
      next; }, r
  | Iraise (kind, _) ->
    let r = Env.register_raise r env in
    let next, r =
      trap_stacks env r insn.Mach.next
    in
    { insn with desc = Iraise (kind, Env.stack env); next; }, r
  | Iifthenelse (cond, ifso, ifnot) ->
    let ifso, r = trap_stacks env r ifso in
    let ifnot, r = trap_stacks env r ifnot in
    let next, r =
      trap_stacks env r insn.Mach.next
    in
    { insn with
      desc = Iifthenelse (cond, ifso, ifnot);
      next;
    }, r
  | Iswitch (cases, insns) ->
    let r = ref r in
    let new_insns = Array.copy insns in
    for case = 0 to Array.length insns - 1 do
      let new_insn, new_r =
        trap_stacks env !r insns.(case)
      in
      new_insns.(case) <- new_insn;
      r := new_r
    done;
    let next, r =
      trap_stacks env !r insn.Mach.next
    in
    { insn with
      desc = Iswitch (cases, new_insns);
      next;
    }, r
  | Iloop body ->
    let body, r = trap_stacks env r body in
    let next, r =
      trap_stacks env r insn.Mach.next
    in
    { insn with
      desc = Iloop body;
      next;
    }, r
  | Icatch (rec_flag, is_exn_handler, handlers, body) ->
    assert (not is_exn_handler || List.length handlers = 1);
    let env_bound, r =
      List.fold_left (fun (env, r) (cont, _, _) ->
          Env.add_binding env r ~cont)
        (env, r) handlers
    in
    let body, r = trap_stacks env_bound r body in
    let handlers =
      let handlers =
        List.map (fun (cont, _trap_stack, handler) ->
            Env.apply_subst env_bound ~cont, handler)
          handlers
      in
      Int.Map.of_list handlers
    in
    let handlers_with_uses, handlers_without_uses =
      Int.Map.partition (fun cont _handler ->
          Env.cont_is_used r ~cont)
        handlers
    in
    let rec process_handlers env r ~handlers_with_uses
          ~handlers_without_uses ~output_handlers =
      (* By the invariant above, there is no need to compute a fixpoint. *)
      if Int.Map.is_empty handlers_with_uses then begin
        output_handlers, r
      end else
        let cont, handler = Int.Map.min_binding handlers_with_uses in
        let handlers_with_uses = Int.Map.remove cont handlers_with_uses in
        match Env.stack_at_exit_exn r ~cont with
        | exception Not_found -> assert false
        | stack ->
          (* [handler] is a continuation that is used.  It is called (via
             exit or raise) when the given [stack] of exception handlers are
             in scope. *)
          let stack =
            if not is_exn_handler then
              stack
            else
              match stack with
              | _::stack -> stack
              | [] ->
                Misc.fatal_errorf "Continuation %d is an exception handler \
                    whose trap-stack-at-start is empty"
                  cont
          in
          let handler, r =
            trap_stacks env r handler
          in
          let new_handlers_with_uses, handlers_without_uses =
            Int.Map.partition (fun cont _handler ->
                Env.cont_is_used r ~cont)
              handlers_without_uses
          in
          let handlers_with_uses =
            Int.Map.disjoint_union handlers_with_uses new_handlers_with_uses
          in
          process_handlers env r ~handlers_with_uses
            ~handlers_without_uses
            ~output_handlers:((cont, stack, handler) :: output_handlers)
    in
    let env_handlers =
      match rec_flag with
      | Recursive -> env_bound
      | Nonrecursive -> env
    in
    let handlers, r =
      process_handlers env_handlers r ~handlers_with_uses
        ~handlers_without_uses ~output_handlers:[]
    in
    let next, r =
      trap_stacks env r insn.Mach.next
    in
    begin match handlers with
    | [] ->
      { insn with
        desc = Icatch (Nonrecursive, false, [], body);
        next;
      }
    , r
    | handlers ->
      { insn with
        desc = Icatch (rec_flag, is_exn_handler, handlers, body);
        next;
      }, r
    end
  | Iexit cont ->
    let cont = Env.apply_subst env ~cont in
    let r = Env.add_stack r ~cont env in
    let next, r =
      trap_stacks env r insn.Mach.next
    in
    { insn with
      desc = Iexit cont;
      next; }, r

let run (fundecl : Mach.fundecl) =
  let fun_body, _r =
    trap_stacks Env.empty_t Env.empty_r fundecl.fun_body
  in
  { fundecl with
    fun_body;
  }

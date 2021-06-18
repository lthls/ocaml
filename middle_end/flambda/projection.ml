(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]
open! Int_replace_polymorphic_compare

(* CR-someday mshinwell: Move these three types into their own modules. *)

type project_closure = {
  set_of_closures : Variable.t;
  closure_id : Closure_id.t;
}

type move_within_set_of_closures = {
  closure : Variable.t;
  start_from : Closure_id.t;
  move_to : Closure_id.t;
}

type project_var = {
  closure : Variable.t;
  closure_id : Closure_id.t;
  var : Var_within_closure.t;
}

type field_read_semantics = Lambda.field_read_semantics

type field_info = Lambda.field_info

let compare_project_var
      ({ closure = closure1; closure_id = closure_id1; var = var1; }
        : project_var)
      ({ closure = closure2; closure_id = closure_id2; var = var2; }
        : project_var) =
  let c = Variable.compare closure1 closure2 in
  if c <> 0 then c
  else
    let c = Closure_id.compare closure_id1 closure_id2 in
    if c <> 0 then c
    else
      Var_within_closure.compare var1 var2

let compare_move_within_set_of_closures
      ({ closure = closure1; start_from = start_from1; move_to = move_to1; }
        : move_within_set_of_closures)
      ({ closure = closure2; start_from = start_from2; move_to = move_to2; }
        : move_within_set_of_closures) =
  let c = Variable.compare closure1 closure2 in
  if c <> 0 then c
  else
    let c = Closure_id.compare start_from1 start_from2 in
    if c <> 0 then c
    else
      Closure_id.compare move_to1 move_to2

let compare_project_closure
      ({ set_of_closures = set_of_closures1; closure_id = closure_id1; }
        : project_closure)
      ({ set_of_closures = set_of_closures2; closure_id = closure_id2; }
        : project_closure) =
  let c = Variable.compare set_of_closures1 set_of_closures2 in
  if c <> 0 then c
  else
    Closure_id.compare closure_id1 closure_id2
    
let same_field_info (* TODO : duplicate code with flambda_utils, find a good file to put it so dependencies work out (lambda/lambda.ml would work) *)
  ({ index = i1; block_info = { tag = t1; size = sz1 } } : Lambda.field_info)
  ({ index = i2; block_info = { tag = t2; size = sz2 } } : Lambda.field_info)
=
  i1 = i2 && t1 = t2 && begin
    match sz1,sz2 with
    | Unknown, Unknown -> true
    | Known a, Known b -> a=b
    | _ -> false
  end    

let compare_project_field 
  (( f1 : field_info), (rem1 : field_read_semantics), var1)
  (( f2 : field_info), (rem2 : field_read_semantics), var2) =
    let c = same_field_info f1 f2 in
    if not c then 1
    else 
      match rem1,rem2 with
      | Reads_agree,Reads_agree | Reads_vary, Reads_vary ->
          Variable.compare var1 var2
      | _ -> 1

let print_project_closure ppf (project_closure : project_closure) =
  Format.fprintf ppf "@[<2>(project_closure@ %a@ from@ %a)@]"
    Closure_id.print project_closure.closure_id
    Variable.print project_closure.set_of_closures

let print_move_within_set_of_closures ppf
      (move_within_set_of_closures : move_within_set_of_closures) =
  Format.fprintf ppf
    "@[<2>(move_within_set_of_closures@ %a <-- %a@ (closure = %a))@]"
    Closure_id.print move_within_set_of_closures.move_to
    Closure_id.print move_within_set_of_closures.start_from
    Variable.print move_within_set_of_closures.closure

let print_project_var ppf (project_var : project_var) =
  Format.fprintf ppf "@[<2>(project_var@ %a@ from %a=%a)@]"
    Var_within_closure.print project_var.var
    Closure_id.print project_var.closure_id
    Variable.print project_var.closure
    
let field_read_semantics ppf (sem : field_read_semantics) =
  match sem with
  | Reads_agree -> ()
  | Reads_vary -> Format.fprintf ppf "_mut"

type t =
  | Project_var of project_var
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Field of field_info * field_read_semantics * Variable.t

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    match t1, t2 with
    | Project_var project_var1, Project_var project_var2 ->
      compare_project_var project_var1 project_var2
    | Project_closure project_closure1, Project_closure project_closure2 ->
      compare_project_closure project_closure1 project_closure2
    | Move_within_set_of_closures move1, Move_within_set_of_closures move2 ->
      compare_move_within_set_of_closures move1 move2
    | Field (info1, rem1, var1), Field (info2, rem2, var2) -> 
      compare_project_field (info1, rem1, var1) (info2, rem2, var2)
    | Project_var _, _ -> -1
    | _, Project_var _ -> 1
    | Project_closure _, _ -> -1
    | _, Project_closure _ -> 1
    | Move_within_set_of_closures _, _ -> -1
    | _, Move_within_set_of_closures _ -> 1

  let equal t1 t2 =
    (compare t1 t2) = 0

  let hash = Hashtbl.hash

  let print ppf t =
    match t with
    | Project_closure (project_closure) ->
      print_project_closure ppf project_closure
    | Project_var (project_var) -> print_project_var ppf project_var
    | Move_within_set_of_closures (move_within_set_of_closures) ->
      print_move_within_set_of_closures ppf move_within_set_of_closures
    | Field ({ index = index;block_info = _ }, rem, var) ->
      Format.fprintf ppf "Field%a %d of %a" field_read_semantics rem index Variable.print var

  let output _ _ = failwith "Projection.output: not yet implemented"
end)

let projecting_from t =
  match t with
  | Project_var { closure; _ } -> closure
  | Project_closure { set_of_closures; _ } -> set_of_closures
  | Move_within_set_of_closures { closure; _ } -> closure
  | Field (_, _, var) -> var

let map_projecting_from t ~f : t =
  match t with
  | Project_var project_var ->
    let project_var : project_var =
      { project_var with
        closure = f project_var.closure;
      }
    in
    Project_var project_var
  | Project_closure project_closure ->
    let project_closure : project_closure =
      { project_closure with
        set_of_closures = f project_closure.set_of_closures;
      }
    in
    Project_closure project_closure
  | Move_within_set_of_closures move ->
    let move : move_within_set_of_closures =
      { move with
        closure = f move.closure;
      }
    in
    Move_within_set_of_closures move
  | Field (field_index, read_semantics, var) -> Field (field_index, read_semantics, f var)

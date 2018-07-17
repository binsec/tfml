(**************************************************************************)
(*  This file is part of Tfml.                                            *)
(*                                                                        *)
(*  Copyright (C) 2017-2018                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  You can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful, but WITHOUT     *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General       *)
(*  Public License version 2.1 for more details.                          *)
(**************************************************************************)

open Formula
open Smtlib_utils


let visit_unop : type a b. (a,b) unop -> Smtlib.identifier =
  function
  | BlNot -> mk_id_symbol (mk_symbol "not")

  | BvNot -> mk_id_symbol (mk_symbol "bvnot")
  | BvNeg -> mk_id_symbol (mk_symbol "bvneg")
  | BvRepeat i -> mk_id_underscore (mk_symbol "repeat") [mk_idx_num i]
  | BvZeroExtend i -> mk_id_underscore (mk_symbol "zero_extend") [mk_idx_num i]
  | BvSignExtend i -> mk_id_underscore (mk_symbol "sign_extend") [mk_idx_num i]
  | BvRotateLeft i -> mk_id_underscore (mk_symbol "rotate_left") [mk_idx_num i]
  | BvRotateRight i -> mk_id_underscore (mk_symbol "rotate_right") [mk_idx_num i]
  | BvExtract i -> mk_id_underscore (mk_symbol "extract")
                     Interval.([mk_idx_num i.hi; mk_idx_num i.lo])

  | IrNeg -> mk_id_symbol (mk_symbol "-")
  | IrAbs -> mk_id_symbol (mk_symbol "abs")

  | RlNeg -> mk_id_symbol (mk_symbol "-")


let visit_bnop : type a b c. (a,b,c) bnop -> Smtlib.identifier =
  function
  | Equal   -> mk_id_symbol (mk_symbol "=")
  | Distinct-> mk_id_symbol (mk_symbol "distinct")

  | BlImply-> mk_id_symbol (mk_symbol "=>")
  | BlAnd  -> mk_id_symbol (mk_symbol "and")
  | BlOr   -> mk_id_symbol (mk_symbol "or")
  | BlXor  -> mk_id_symbol (mk_symbol "xor")

  | BvConcat -> mk_id_symbol (mk_symbol "concat")
  | BvAnd  -> mk_id_symbol (mk_symbol "bvand")
  | BvNand -> mk_id_symbol (mk_symbol "bvnand")
  | BvOr   -> mk_id_symbol (mk_symbol "bvor")
  | BvNor  -> mk_id_symbol (mk_symbol "bvnor")
  | BvXor  -> mk_id_symbol (mk_symbol "bvxor")
  | BvXnor -> mk_id_symbol (mk_symbol "bvxnor")
  | BvCmp  -> mk_id_symbol (mk_symbol "bvcomp")
  | BvAdd  -> mk_id_symbol (mk_symbol "bvadd")
  | BvSub  -> mk_id_symbol (mk_symbol "bvsub")
  | BvMul  -> mk_id_symbol (mk_symbol "bvmul")
  | BvUdiv -> mk_id_symbol (mk_symbol "bvudiv")
  | BvSdiv -> mk_id_symbol (mk_symbol "bvsdiv")
  | BvUrem -> mk_id_symbol (mk_symbol "bvurem")
  | BvSrem -> mk_id_symbol (mk_symbol "bvsrem")
  | BvSmod -> mk_id_symbol (mk_symbol "bvsmod")
  | BvShl  -> mk_id_symbol (mk_symbol "bvshl")
  | BvAshr -> mk_id_symbol (mk_symbol "bvashr")
  | BvLshr -> mk_id_symbol (mk_symbol "bvlshr")

  | BvUlt  -> mk_id_symbol (mk_symbol "bvult")
  | BvUle  -> mk_id_symbol (mk_symbol "bvule")
  | BvUgt  -> mk_id_symbol (mk_symbol "bvugt")
  | BvUge  -> mk_id_symbol (mk_symbol "bvuge")
  | BvSlt  -> mk_id_symbol (mk_symbol "bvslt")
  | BvSle  -> mk_id_symbol (mk_symbol "bvsle")
  | BvSgt  -> mk_id_symbol (mk_symbol "bvsgt")
  | BvSge  -> mk_id_symbol (mk_symbol "bvsge")

  | IrAdd  -> mk_id_symbol (mk_symbol "+")
  | IrSub  -> mk_id_symbol (mk_symbol "-")
  | IrMul  -> mk_id_symbol (mk_symbol "*")
  | IrDiv  -> mk_id_symbol (mk_symbol "div")
  | IrMod  -> mk_id_symbol (mk_symbol "mod")

  | IrLt   -> mk_id_symbol (mk_symbol "<")
  | IrLe   -> mk_id_symbol (mk_symbol "<=")
  | IrGt   -> mk_id_symbol (mk_symbol ">")
  | IrGe   -> mk_id_symbol (mk_symbol ">=")

  | RlAdd  -> mk_id_symbol (mk_symbol "+")
  | RlSub  -> mk_id_symbol (mk_symbol "-")
  | RlMul  -> mk_id_symbol (mk_symbol "*")
  | RlDiv  -> mk_id_symbol (mk_symbol "/")

  | RlLt   -> mk_id_symbol (mk_symbol "<")
  | RlLe   -> mk_id_symbol (mk_symbol "<=")
  | RlGt   -> mk_id_symbol (mk_symbol ">")
  | RlGe   -> mk_id_symbol (mk_symbol ">=")


let rec visit_sort : type a. a sort -> string =
  fun { sort_desc; _ } ->
    match sort_desc with
    | BlSort -> "Bool"
    | BvSort i -> Printf.sprintf "(_ BitVec %i)" i
    | IrSort -> "Int"
    | RlSort -> "Real"
    | AxSort (i,j) ->
      Printf.sprintf "(Array %s %s)"
        (visit_sort i) (visit_sort j)

let visit_sort sort = mk_sort_identifier (mk_symbol (visit_sort sort))

let sort st = visit_sort st


let rec visit_term_desc : type a. a term_desc -> Smtlib.term =
  function
  | Bool bl ->
    mk_term_spec_constant (Smtlib.CstBool bl)

  | BitVec bv ->
    mk_term_spec_constant
      (Smtlib.CstDecimalSize
         (Z.to_string (Bitvector.value_of bv),
          string_of_int (Bitvector.size_of bv)))

  | Integer ir ->
    mk_term_spec_constant
      (Smtlib.CstNumeral
         (Z.to_string ir))

  | Real rl ->
    if Z.equal (Q.den rl) Z.one
    then
      mk_term_spec_constant
        (Smtlib.CstNumeral
           (Z.to_string (Q.num rl)))
    else
      mk_term_spec_constant
        (Smtlib.CstNumeral
           (Printf.sprintf "(/ %s %s)"
              (Z.to_string (Q.num rl))
              (Z.to_string (Q.den rl))))

  | Fun (v,ls) ->
    let ls = List.map (fun (AnyTerm tm) -> visit_term tm) ls in
    (match ls with
     | [] ->
       mk_term_qual_identifier
         (mk_qual_identifier_identifier
            (mk_id_symbol
               (mk_symbol v.var_name)))
     | _ ->
       mk_term_qual_identifier_terms
         (mk_qual_identifier_identifier
            (mk_id_symbol
               (mk_symbol v.var_name)))
         ls)

  | Bind (bn,tm) ->
    let tm = visit_term tm in
    visit_bind bn tm

  | Unop (u,tm) ->
    let tm = visit_term tm in
    mk_term_qual_identifier_terms
      (mk_qual_identifier_identifier
         (visit_unop u))
      [tm]

  | Bnop (b,tm1,tm2) ->
    let tm1 = visit_term tm1 in
    let tm2 = visit_term tm2 in
    mk_term_qual_identifier_terms
      (mk_qual_identifier_identifier
         (visit_bnop b))
      [tm1;tm2]

  | Ite (bl,tm1,tm2) ->
    let bl = visit_term bl in
    let tm1 = visit_term tm1 in
    let tm2 = visit_term tm2 in
    mk_term_qual_identifier_terms
      (mk_qual_identifier_identifier
         (mk_id_symbol
            (mk_symbol "ite")))
      [bl;tm1;tm2]

  | Select (ax,tm) ->
    let ax = visit_term ax in
    let tm = visit_term tm in
    mk_term_qual_identifier_terms
      (mk_qual_identifier_identifier
         (mk_id_symbol
            (mk_symbol "select")))
      [ax;tm]

  | Store (ax,tm1,tm2) ->
    let ax = visit_term ax in
    let tm1 = visit_term tm1 in
    let tm2 = visit_term tm2 in
    mk_term_qual_identifier_terms
      (mk_qual_identifier_identifier
         (mk_id_symbol
            (mk_symbol "store")))
      [ax;tm1;tm2]

and visit_term : type a. a term -> Smtlib.term =
  fun { term_desc; _ } -> visit_term_desc term_desc


and visit_bind_desc term = function
  | Let ls ->
    let ls =
      List.map
        (fun (symbol, _sort, sorts, term) ->
           assert (sorts = []);
           mk_var_binding symbol term)
        (List.map visit_def ls)
    in
    mk_term_let_term ls term

  | Exists ls ->
    let ls =
      List.map
        (fun (symbol, sort, sorts) ->
           assert (sorts = []);
           mk_sorted_var symbol sort)
        (List.map visit_decl ls)
    in
    mk_term_exists_term ls term

  | Forall ls ->
    let ls =
      List.map
        (fun (symbol, sort, sorts) ->
           assert (sorts = []);
           mk_sorted_var symbol sort)
        (List.map visit_decl ls)
    in
    mk_term_forall_term ls term

and visit_bind { bind_desc; _ } term =
  visit_bind_desc term bind_desc


and visit_def_desc = function
  | Def (v,ls,tm) ->
    mk_symbol v.var_name,
    visit_sort v.var_sort,
    List.map (fun (AnyVar v) -> mk_symbol v.var_name, visit_sort v.var_sort) ls,
    visit_term tm

and visit_def { def_desc; _ } =
  visit_def_desc def_desc


and visit_decl_desc = function
  | Decl (v,ls) ->
    mk_symbol v.var_name,
    visit_sort v.var_sort,
    List.map (fun (AnySort s) -> visit_sort s) ls

and visit_decl { decl_desc; _ } =
  visit_decl_desc decl_desc


and visit_entry_desc = function
  | Declare dc ->
    let symbol, sort, sorts = visit_decl dc in
    mk_cmd_declare_fun symbol sorts sort
  | Define df ->
    let symbol, sort, sorts, term = visit_def df in
    let sorted_vars =
      List.map
        (fun (symbol, sort) -> mk_sorted_var symbol sort)
        sorts
    in
    mk_cmd_define_fun (mk_fun_def symbol sort sorted_vars term)
  | Assert bl ->
    let bl = visit_term bl in
    mk_command (Smtlib.CmdAssert bl)
  | Comment c -> mk_command (Smtlib.CmdComment c)

and visit_entry { entry_desc; _ } =
  visit_entry_desc entry_desc


and visit_formula { entries; _ } =
  Sequence.map_forward visit_entry entries


let term term = visit_term term
let entry entry = visit_entry entry

let list_of_sequence seq =
  Sequence.fold_backward (fun x acc -> x :: acc) seq []

let set_logic ast fml =
  let open Smtlib in
  match ast.logic with
  | "" -> fml
  | l -> Sequence.push_back (mk_command (CmdSetLogic (mk_symbol l))) fml

let formula ast =
  let open Smtlib in
  let script_commands =
    visit_formula ast
    |> set_logic ast
    |> Sequence.push_front (mk_command CmdCheckSat)
    |> Sequence.push_front (mk_command CmdExit)
    |> list_of_sequence
  in
  let script_loc = Locations.dummy_loc in
  { script_commands; script_loc }


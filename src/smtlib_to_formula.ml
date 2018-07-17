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

open Smtlib

module BindEnv =
struct
  open Formula
  open Utils

  let create n = StringHashtbl.create n

  let lookup t s =
   try StringHashtbl.find t s
   with Not_found -> failwith ("Unbound variable " ^ s)

  let decl t { decl_desc = Decl (v,_); _ } = StringHashtbl.add t v.var_name (any_var v)
  let def  t { def_desc = Def (v,_,_); _ } = StringHashtbl.add t v.var_name (any_var v)

  let bind t { bind_desc; _ } =
    match bind_desc with
    | Let ls -> List.iter (def t) ls
    | Exists ls -> List.iter (decl t) ls
    | Forall ls -> List.iter (decl t) ls

  let undecl t { decl_desc = Decl (v,_); _ } = StringHashtbl.remove t v.var_name
  let undef  t { def_desc = Def (v,_,_); _ } = StringHashtbl.remove t v.var_name

  let unbind t { bind_desc; _ } =
    match bind_desc with
    | Let ls -> List.iter (undef t) ls
    | Exists ls -> List.iter (undecl t) ls
    | Forall ls -> List.iter (undecl t) ls
end


let is_bl_term : type a. a Formula.term -> Formula.bl Formula.term option =
  fun tm ->
    match tm.Formula.term_sort.Formula.sort_desc with
    | Formula.BlSort -> Some tm
    | _ -> None

let is_bv_term : type a. a Formula.term -> Formula.bv Formula.term option =
  fun tm ->
    match tm.Formula.term_sort.Formula.sort_desc with
    | Formula.BvSort _ -> Some tm
    | _ -> None

let is_ir_term : type a. a Formula.term -> Formula.ir Formula.term option =
  fun tm ->
    match tm.Formula.term_sort.Formula.sort_desc with
    | Formula.IrSort -> Some tm
    | _ -> None

let is_rl_term : type a. a Formula.term -> Formula.rl Formula.term option =
  fun tm ->
    match tm.Formula.term_sort.Formula.sort_desc with
    | Formula.RlSort -> Some tm
    | Formula.IrSort ->
      (match Formula_utils.is_integer tm with
       | Some ir -> Some (Formula.mk_real (Q.of_bigint ir))
       | None -> None)
    | _ -> None

let ensure_bl_term : type a. a Formula.term -> Formula.bl Formula.term =
  fun tm ->
    match is_bl_term tm with
    | Some tm -> tm
    | None -> assert false

let bl_unop : type a.
  (Formula.bl Formula.term -> a Formula.term) -> Formula.any_term -> Formula.any_term =
  fun f (Formula.AnyTerm tm) ->
    match is_bl_term tm with
    | Some tm -> Formula.any_term (f tm)
    | None -> assert false

let bv_unop : type a.
  (Formula.bv Formula.term -> a Formula.term) -> Formula.any_term -> Formula.any_term =
  fun f (Formula.AnyTerm tm) ->
    match is_bv_term tm with
    | Some tm -> Formula.any_term (f tm)
    | None -> assert false

let ir_unop : type a.
  (Formula.ir Formula.term -> a Formula.term) -> Formula.any_term -> Formula.any_term =
  fun f (Formula.AnyTerm tm) ->
    match is_ir_term tm with
    | Some tm -> Formula.any_term (f tm)
    | None -> assert false
(*
let rl_unop : type a.
  (Formula.rl Formula.term -> a Formula.term) -> Formula.any_term -> Formula.any_term =
  fun f (Formula.AnyTerm tm) ->
    match is_rl_term tm with
    | Some tm -> Formula.any_term (f tm)
    | None -> assert false
*)
let ir_or_rl_unop : type a b.
  (Formula.ir Formula.term -> a Formula.term) ->
  (Formula.rl Formula.term -> b Formula.term) ->
  Formula.any_term -> Formula.any_term =
  fun f g (Formula.AnyTerm tm) ->
    match is_ir_term tm with
    | Some tm -> Formula.any_term (f tm)
    | None ->
      match is_rl_term tm with
      | Some tm -> Formula.any_term (g tm)
      | None -> assert false

let bl_bnop : type a.
  (Formula.bl Formula.term -> Formula.bl Formula.term -> a Formula.term) ->
  Formula.any_term  -> Formula.any_term -> Formula.any_term =
  fun f (Formula.AnyTerm tm1) (Formula.AnyTerm tm2) ->
    match is_bl_term tm1, is_bl_term tm2 with
    | Some tm1, Some tm2 -> Formula.any_term (f tm1 tm2)
    | _ -> assert false

let bv_bnop : type a.
  (Formula.bv Formula.term -> Formula.bv Formula.term -> a Formula.term) ->
  Formula.any_term  -> Formula.any_term -> Formula.any_term =
  fun f (Formula.AnyTerm tm1) (Formula.AnyTerm tm2) ->
    match is_bv_term tm1, is_bv_term tm2 with
    | Some tm1, Some tm2 -> Formula.any_term (f tm1 tm2)
    | _ -> assert false

let ir_bnop : type a.
  (Formula.ir Formula.term -> Formula.ir Formula.term -> a Formula.term) ->
  Formula.any_term  -> Formula.any_term -> Formula.any_term =
  fun f (Formula.AnyTerm tm1) (Formula.AnyTerm tm2) ->
    match is_ir_term tm1, is_ir_term tm2 with
    | Some tm1, Some tm2 -> Formula.any_term (f tm1 tm2)
    | _ -> assert false

let rl_bnop : type a.
  (Formula.rl Formula.term -> Formula.rl Formula.term -> a Formula.term) ->
  Formula.any_term  -> Formula.any_term -> Formula.any_term =
  fun f (Formula.AnyTerm tm1) (Formula.AnyTerm tm2) ->
    match is_rl_term tm1, is_rl_term tm2 with
    | Some tm1, Some tm2 -> Formula.any_term (f tm1 tm2)
    | _ -> assert false

let ir_or_rl_bnop : type a b.
  (Formula.ir Formula.term -> Formula.ir Formula.term -> a Formula.term) ->
  (Formula.rl Formula.term -> Formula.rl Formula.term -> b Formula.term) ->
  Formula.any_term  -> Formula.any_term -> Formula.any_term =
  fun f g (Formula.AnyTerm tm1) (Formula.AnyTerm tm2) ->
    match is_ir_term tm1, is_ir_term tm2 with
    | Some tm1, Some tm2 -> Formula.any_term (f tm1 tm2)
    | _ ->
      match is_rl_term tm1, is_rl_term tm2 with
      | Some tm1, Some tm2 -> Formula.any_term (g tm1 tm2)
      | _ -> assert false


let is_built_in qi =
  match Smtlib_utils.string_of_qual_identifier qi with
  | "=" | "distinct" | "ite"
  | "true" | "false" | "not"
  | "=>" | "and" | "or" | "xor"

  | "+" | "-" | "*" | "/"
  | "div" | "mod" | "abs"
  | "<" | "<=" | ">" | ">="

  | "concat" | "repeat" | "extract"
  | "sign_extend" | "zero_extend"
  | "rotate_left" | "rotate_right"
  | "bvnot" | "bvneg"
  | "bvand" | "bvnand"
  | "bvor"  | "bvnor"
  | "bvxor" | "bvxnor"
  | "bvcomp" | "bvadd"  | "bvsub"
  | "bvmul"  | "bvudiv" | "bvsdiv"
  | "bvurem" | "bvsrem" | "bvsmod"
  | "bvshl"  | "bvashr" | "bvlshr"

  | "bvuge" | "bvugt" | "bvule" | "bvult"
  | "bvslt" | "bvsle" | "bvsgt" | "bvsge"

  | "select" | "store" -> true

  | _ -> false


let is_uidx = function
  | [IdxNum s] -> int_of_string s
  | _ -> assert false

let is_bidx = function
  | [IdxNum s1; IdxNum s2] -> int_of_string s1, int_of_string s2
  | _ -> assert false


let visit_constant = function
  | CstString _ -> assert false

  | CstNumeral ir ->
    let ir = Z.of_string ir in
    Formula.(any_term (mk_integer ir))

  | CstDecimal rl ->
    let rl = Q.of_float (float_of_string rl) in
    Formula.any_term (Formula.mk_real rl)

  | CstDecimalSize (bv, sz) ->
    let sz = int_of_string sz in
    let n  = Z.of_string bv in
    let bv = Bitvector.create n sz in
    Formula.(any_term (mk_bitvec bv))

  | CstHexadecimal s ->
    let sz = 4 * String.length s in
    let n  = Z.of_string ("0x"^s) in
    let bv = Bitvector.create n sz in
    Formula.(any_term (mk_bitvec bv))

  | CstBinary s ->
    let sz = String.length s in
    let n  = Z.of_string ("0b"^s) in
    let bv = Bitvector.create n sz in
    Formula.(any_term  (mk_bitvec bv))

  | CstBool bl -> Formula.(any_term (Formula.mk_bool bl))


let visit_sort_identifier { id_desc; _ } ls =
  match id_desc with
  | IdSymbol sy ->
    (let open Formula in
     match Smtlib_utils.string_of_symbol sy with
     | "Bool" -> assert (ls = []); any_sort mk_bl_sort
     | "Int"  -> assert (ls = []); any_sort mk_ir_sort
     | "Real" -> assert (ls = []); any_sort mk_rl_sort
     | "Array" ->
       (match ls with
        | [AnySort s1; AnySort s2] -> any_sort (mk_ax_sort s1 s2)
        | _ -> assert false)
     | s -> failwith ("Unhandled sort " ^ s))

  | IdUnderscore (sy, idx) ->
    (let open Formula in
     match Smtlib_utils.string_of_symbol sy with
     | "BitVec" -> assert (ls = []); any_sort (mk_bv_sort (is_uidx idx))
     | s -> failwith ("Unhandled sort " ^ s))

let rec visit_sort { sort_desc; _ } =
  match sort_desc with
  | SortIdentifier id -> visit_sort_identifier id []
  | SortFun (id,ls) -> visit_sort_identifier id (List.map visit_sort ls)

let sort st = visit_sort st


let visit_sorted_var { sorted_var_desc; _ } =
  let open Formula in
  match sorted_var_desc with
  | SortedVar (sy, st) ->
    let (AnySort st) = visit_sort st in
    any_var (var (Smtlib_utils.string_of_symbol sy) st)


let visit_var_decl sv =
  let open Formula in
  let (AnyVar v) = visit_sorted_var sv in
  mk_decl v []


let visit_qual_identifier env { qual_identifier_desc; _ } =
  match qual_identifier_desc with
  | QualIdentifierAs (id, st) ->
    let open Formula in
    let (AnySort st) = visit_sort st in
    any_var (var (Smtlib_utils.string_of_identifier id) st)
  | QualIdentifierIdentifier id ->
    BindEnv.lookup env (Smtlib_utils.string_of_identifier id)


let visit_unop { id_desc; _ } tm1 =
  match id_desc with
  | IdSymbol sy ->
    (let open Formula in
     match Smtlib_utils.string_of_symbol sy with
     | "not"   -> bl_unop mk_bl_not tm1
     | "bvnot" -> bv_unop mk_bv_not tm1
     | "bvneg" -> bv_unop mk_bv_neg tm1
     | "-"     -> ir_or_rl_unop mk_ir_neg mk_rl_neg tm1
     | "abs"   -> ir_unop mk_ir_abs tm1
     | s -> failwith ("Unhandled symbol " ^ s))

  | IdUnderscore (sy, idx) ->
    (let open Formula in
     match Smtlib_utils.string_of_symbol sy with
     | "repeat"       -> bv_unop (mk_bv_repeat       (is_uidx idx)) tm1
     | "zero_extend"  -> bv_unop (mk_bv_zero_extend  (is_uidx idx)) tm1
     | "sign_extend"  -> bv_unop (mk_bv_sign_extend  (is_uidx idx)) tm1
     | "rotate_left"  -> bv_unop (mk_bv_rotate_left  (is_uidx idx)) tm1
     | "rotate_right" -> bv_unop (mk_bv_rotate_right (is_uidx idx)) tm1
     | "extract"      -> let (hi,lo) = is_bidx idx in
       bv_unop (mk_bv_extract Interval.{hi; lo}) tm1
     | s -> failwith ("Unhandled symbol " ^ s))

let visit_bnop { id_desc; _ } (Formula.AnyTerm tm1 as any1) (Formula.AnyTerm tm2 as any2) =
  match id_desc with
  | IdUnderscore (_,_) -> assert false
  | IdSymbol sy ->
    (let open Formula in
     let open Formula_utils in
     match Smtlib_utils.string_of_symbol sy with
     | "=" ->
       (match equal_sort tm1.term_sort tm2.term_sort with
        | Eq _ -> any_term (mk_equal tm1 tm2)
        | Nq _ -> assert false)

     | "distinct" ->
       (match equal_sort tm1.term_sort tm2.term_sort with
        | Eq _ -> any_term (mk_distinct tm1 tm2)
        | Nq _ -> assert false)

     | "=>"  -> bl_bnop mk_bl_imply any1 any2
     | "and" -> bl_bnop mk_bl_and   any1 any2
     | "or"  -> bl_bnop mk_bl_or    any1 any2
     | "xor" -> bl_bnop mk_bl_xor   any1 any2

     | "+"   -> ir_or_rl_bnop mk_ir_add mk_rl_add any1 any2
     | "-"   -> ir_or_rl_bnop mk_ir_sub mk_rl_sub any1 any2
     | "*"   -> ir_or_rl_bnop mk_ir_mul mk_rl_mul any1 any2

     | "/"   -> rl_bnop mk_rl_div any1 any2
     | "div" -> ir_bnop mk_ir_div any1 any2
     | "mod" -> ir_bnop mk_ir_mod any1 any2

     | "<"   -> ir_or_rl_bnop mk_ir_lt mk_rl_lt any1 any2
     | "<="  -> ir_or_rl_bnop mk_ir_le mk_rl_le any1 any2
     | ">"   -> ir_or_rl_bnop mk_ir_gt mk_rl_gt any1 any2
     | ">="  -> ir_or_rl_bnop mk_ir_ge mk_rl_ge any1 any2

     | "concat" -> bv_bnop mk_bv_concat any1 any2
     | "bvand"  -> bv_bnop mk_bv_and    any1 any2
     | "bvnand" -> bv_bnop mk_bv_nand   any1 any2
     | "bvor"   -> bv_bnop mk_bv_or     any1 any2
     | "bvnor"  -> bv_bnop mk_bv_nor    any1 any2
     | "bvxor"  -> bv_bnop mk_bv_xor    any1 any2
     | "bvxnor" -> bv_bnop mk_bv_xnor   any1 any2
     | "bvcomp" -> bv_bnop mk_bv_cmp    any1 any2
     | "bvadd"  -> bv_bnop mk_bv_add    any1 any2
     | "bvsub"  -> bv_bnop mk_bv_sub    any1 any2
     | "bvmul"  -> bv_bnop mk_bv_mul    any1 any2
     | "bvudiv" -> bv_bnop mk_bv_udiv   any1 any2
     | "bvsdiv" -> bv_bnop mk_bv_sdiv   any1 any2
     | "bvurem" -> bv_bnop mk_bv_urem   any1 any2
     | "bvsrem" -> bv_bnop mk_bv_srem   any1 any2
     | "bvsmod" -> bv_bnop mk_bv_smod   any1 any2
     | "bvshl"  -> bv_bnop mk_bv_shl    any1 any2
     | "bvashr" -> bv_bnop mk_bv_ashr   any1 any2
     | "bvlshr" -> bv_bnop mk_bv_lshr   any1 any2

     | "bvuge" -> bv_bnop mk_bv_uge any1 any2
     | "bvugt" -> bv_bnop mk_bv_ugt any1 any2
     | "bvule" -> bv_bnop mk_bv_ule any1 any2
     | "bvult" -> bv_bnop mk_bv_ult any1 any2
     | "bvsge" -> bv_bnop mk_bv_sge any1 any2
     | "bvsgt" -> bv_bnop mk_bv_sgt any1 any2
     | "bvsle" -> bv_bnop mk_bv_sle any1 any2
     | "bvslt" -> bv_bnop mk_bv_slt any1 any2

     | "select" ->
       (match tm1.term_sort.Formula.sort_desc with
        | AxSort (idx,_) ->
          (match equal_sort idx tm2.term_sort with
           | Eq _ -> any_term (mk_select tm1 tm2)
           | Nq _ -> assert false)
        | _ -> assert false)
     | s -> failwith ("Unhandled symbol " ^ s))

let visit_tnop { id_desc; _ } (Formula.AnyTerm tm1) (Formula.AnyTerm tm2) (Formula.AnyTerm tm3) =
  match id_desc with
  | IdUnderscore (_,_) -> assert false
  | IdSymbol sy ->
    (let open Formula in
     let open Formula_utils in
     match Smtlib_utils.string_of_symbol sy with
     | "ite" ->
       (match equal_sort tm2.term_sort tm3.term_sort with
        | Eq _ -> any_term (mk_ite (ensure_bl_term tm1) tm2 tm3)
        | Nq _ -> assert false)

     | "store" ->
       (match tm1.term_sort.Formula.sort_desc with
        | AxSort (idx,elt) ->
          (match equal_sort idx tm2.term_sort, equal_sort elt tm3.term_sort with
           | Eq _, Eq _ -> any_term (mk_store tm1 tm2 tm3)
           | _, _ -> assert false)
        | _ -> assert false)
     | s -> failwith ("Unhandled symbol " ^ s))

let visit_built_in { qual_identifier_desc; _ } ls =
  let open Formula in
  let id =
    match qual_identifier_desc with
    | QualIdentifierIdentifier id
    | QualIdentifierAs (id,_) -> id
  in
  match ls with
  | [tm1] -> visit_unop id tm1
  | [tm1; tm2] -> visit_bnop id tm1 tm2
  | [tm1; tm2; tm3] -> visit_tnop id tm1 tm2 tm3
  | _ -> failwith ("Unhandled arity for symbol " ^ (Smtlib_utils.string_of_identifier id))


let rec left_assoc qi acc ls =
  let open Smtlib_utils in
  match ls with
  | [tm] -> qi, [acc; tm]
  | tm :: ls -> left_assoc qi (mk_term_qual_identifier_terms qi [acc; tm]) ls
  | [] -> assert false

let left_assoc qi ls =
  let open Smtlib_utils in
  match ls  with
  | [] | [_] | [_; _] -> qi, ls
  | tm1 :: tm2 :: ls -> left_assoc qi (mk_term_qual_identifier_terms qi [tm1; tm2]) ls

let rec right_assoc qi ls =
  let open Smtlib_utils in
  match ls with
  | [_; _] -> mk_term_qual_identifier_terms qi ls
  | tm :: ls -> mk_term_qual_identifier_terms qi [tm; right_assoc qi ls]
  | [] -> assert false

let right_assoc qi ls =
  match ls with
  | [] | [_] | [_; _] -> qi, ls
  | tm :: ls -> qi, [tm; right_assoc qi ls]

let normalize qi ls =
  match Smtlib_utils.string_of_qual_identifier qi with
  | "and" | "or" | "xor" | "+" | "-" | "*" | "/" | "div" -> left_assoc qi ls
  | "=>" -> right_assoc qi ls
  | _ -> qi, ls


let rec visit_term env { term_desc; _ } =
  match term_desc with
  | TermSpecConstant cs -> visit_constant cs

  | TermQualIdentifier qi ->
    let open Formula in
    let (AnyVar v) = visit_qual_identifier env qi in
    any_term (mk_fun v [])

  | TermQualIdentifierTerms (qi,ls) ->
    let open Formula in
    if is_built_in qi then
      let qi,ls = normalize qi ls in
      visit_built_in qi (List.map (visit_term env) ls)
    else
      let ls = List.map (visit_term env) ls in
      let (AnyVar v) = visit_qual_identifier env qi in
      any_term (mk_fun v ls)

  | TermLetTerm (vb,tm) ->
    let open Formula in
    let bn = mk_let (List.map (visit_var_binding env) vb) in
    BindEnv.bind env bn;
    let (AnyTerm tm) = visit_term env tm in
    BindEnv.unbind env bn;
    any_term (mk_bind bn tm)

  | TermForallTerm (sv,tm) ->
    let open Formula in
    let bn = mk_forall (List.map visit_var_decl sv) in
    BindEnv.bind env bn;
    let (AnyTerm tm) = visit_term env tm in
    BindEnv.unbind env bn;
    any_term (mk_bind bn tm)

  | TermExistsTerm (sv,tm) ->
    let open Formula in
    let bn = mk_exists (List.map visit_var_decl sv) in
    BindEnv.bind env bn;
    let (AnyTerm tm) = visit_term env tm in
    BindEnv.unbind env bn;
    any_term (mk_bind bn tm)

  | TermAnnotatedTerm (_,_) -> failwith "Unhandled annotated term"


and visit_var_binding env { var_binding_desc; _ } =
  visit_var_binding_desc env var_binding_desc

and visit_var_binding_desc env = function
    VarBinding (sy,tm) ->
    let open Formula in
    let (AnyTerm tm) = visit_term env tm in
    mk_def (var (Smtlib_utils.string_of_symbol sy) tm.term_sort) [] tm

let term tm = visit_term (BindEnv.create 17) tm


let visit_fun_def env { fun_def_desc; _ } =
  let open Formula in
  let open Formula_utils in
  match fun_def_desc with
  | FunDef (sy,_,ls,st,tm) ->
    let (AnySort st) = visit_sort st in
    let (AnyTerm tm) = visit_term env tm in
    match equal_sort st tm.term_sort with
    | Eq _ -> mk_def (var (Smtlib_utils.string_of_symbol sy) st) (List.map visit_sorted_var ls) tm
    | Nq _ -> assert false

let visit_command env fml { command_desc; _ } =
  let open Formula in
  match command_desc with
  | CmdAssert tm ->
    let (AnyTerm tm) = visit_term env tm in
    push_front_assert (ensure_bl_term tm) fml

  | CmdComment s -> push_front_comment s fml

  | CmdDeclareFun (sy,_,ls,st) ->
    let (AnySort st) = visit_sort st in
    let dc = mk_decl (var (Smtlib_utils.string_of_symbol sy) st) (List.map visit_sort ls) in
    BindEnv.decl env dc;
    push_front_declare dc fml

  | CmdDefineFun fd ->
    let df = visit_fun_def env fd in
    BindEnv.def env df;
    push_front_define df fml

  | CmdDeclareConst _ -> failwith "Unhandled constant declaration"
  | CmdDeclareSort  _ -> failwith "Unhandled sort declaration"
  | CmdDefineFunRec _ -> failwith "Unhandled recursive definition"
  | CmdDefineSort   _ -> failwith "Unhandled sort definition"

  | CmdSetLogic { symbol_desc = SimpleSymbol l | QuotedSymbol l; _ } -> set_logic l fml

  | CmdCheckSat
  | CmdCheckSatAssuming _
  | CmdEcho _
  | CmdExit
  | CmdGetAssertions
  | CmdGetAssignment
  | CmdGetInfo _
  | CmdGetModel
  | CmdGetOption _
  | CmdGetProof
  | CmdGetUnsatAssumptions
  | CmdGetUnsatCore
  | CmdGetValue _
  | CmdMetaInfo _
  | CmdPop _
  | CmdPush _
  | CmdReset
  | CmdResetAssertions
  | CmdSetInfo _
  | CmdSetOption _ -> fml

let script { script_commands; _ } =
  let env = BindEnv.create 17 in
  List.fold_left (visit_command env) Formula.empty script_commands

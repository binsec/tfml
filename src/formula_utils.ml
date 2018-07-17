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

let is_bool { term_desc; _ } =
  match term_desc with
  | Bool bl -> Some bl
  | _ -> None

let is_bitvec { term_desc; _ } =
  match term_desc with
  | BitVec bv -> Some bv
  | _ -> None

let is_integer { term_desc; _ } =
  match term_desc with
  | Integer ir -> Some ir
  | _ -> None

let is_real { term_desc; _ } =
  match term_desc with
  | Real rl -> Some rl
  | _ -> None

let is_var { term_desc; _ } =
  match term_desc with
  | Fun (v,[]) -> Some v
  | _ -> None

let is_fun { term_desc; _ } =
  match term_desc with
  | Fun (v,(_::_ as ls)) -> Some (v,ls)
  | _ -> None

let any_var_hash (AnyVar v) = v.var_hash
let any_var_name (AnyVar v) = v.var_name
let any_var_sort (AnyVar v) = any_sort v.var_sort

let any_term_hash (AnyTerm v) = v.term_hash
let any_term_sort (AnyTerm v) = any_sort v.term_sort

let decl_var { decl_desc = Decl (v,_); _ } = any_var v
let def_var  { def_desc = Def (v,_,_); _ } = any_var v

let decl_name { decl_desc = Decl (v,_); _ } = v.var_name
let def_name  { def_desc = Def (v,_,_); _ } = v.var_name

let string_of_unop : type a b. (a,b) unop -> string =
  function
  | BlNot -> "not"

  | BvNot -> "bvnot"
  | BvNeg -> "bvneg"

  | BvRepeat _ -> "repeat"
  | BvZeroExtend _ -> "zero_extend"
  | BvSignExtend _ -> "sign_extend"
  | BvRotateLeft _ -> "rotate_left"
  | BvRotateRight _ -> "rotate_right"
  | BvExtract _ -> "extract"

  | IrNeg -> "-"
  | IrAbs -> "abs"

  | RlNeg -> "-"

let string_of_bnop : type a b c. (a,b,c) bnop -> string =
  function
  | Equal   -> "="
  | Distinct-> "distinct"

  | BlImply-> "=>"
  | BlAnd  -> "and"
  | BlOr   -> "or"
  | BlXor  -> "xor"

  | BvConcat -> "concat"
  | BvAnd  -> "bvand"
  | BvNand -> "bvnand"
  | BvOr   -> "bvor"
  | BvNor  -> "bvnor"
  | BvXor  -> "bvxor"
  | BvXnor -> "bvxnor"
  | BvCmp  -> "bvcomp"
  | BvAdd  -> "bvadd"
  | BvSub  -> "bvsub"
  | BvMul  -> "bvmul"
  | BvUdiv -> "bvudiv"
  | BvSdiv -> "bvsdiv"
  | BvUrem -> "bvurem"
  | BvSrem -> "bvsrem"
  | BvSmod -> "bvsmod"
  | BvShl  -> "bvshl"
  | BvAshr -> "bvashr"
  | BvLshr -> "bvlshr"

  | BvUlt  -> "bvult"
  | BvUle  -> "bvule"
  | BvUgt  -> "bvugt"
  | BvUge  -> "bvuge"
  | BvSlt  -> "bvslt"
  | BvSle  -> "bvsle"
  | BvSgt  -> "bvsgt"
  | BvSge  -> "bvsge"

  | IrAdd  -> "+"
  | IrSub  -> "-"
  | IrMul  -> "*"
  | IrDiv  -> "div"
  | IrMod  -> "mod"

  | IrLt   -> "<"
  | IrLe   -> "<="
  | IrGt   -> ">"
  | IrGe   -> ">="

  | RlAdd  -> "+"
  | RlSub  -> "-"
  | RlMul  -> "*"
  | RlDiv  -> "/"

  | RlLt   -> "<"
  | RlLe   -> "<="
  | RlGt   -> ">"
  | RlGe   -> ">="


type (_,_) eq =
  | Eq : 'a sort * 'a sort -> ('a,'a) eq
  | Nq : 'a sort * 'b sort -> ('a,'b) eq

let rec equal_sort : type a b. a sort -> b sort -> (a,b) eq =
  fun s1 s2 ->
    match s1.sort_desc, s2.sort_desc with
    | BlSort, BlSort -> Eq (s1,s2)
    | BvSort _, BvSort _ -> Eq (s1,s2)
    | IrSort, IrSort -> Eq (s1,s2)
    | RlSort, RlSort -> Eq (s1,s2)
    | AxSort (i1,e1), AxSort (i2,e2) ->
      (match equal_sort i1 i2 with
       | Nq _ -> Nq (s1,s2)
       | Eq _ ->
         match equal_sort e1 e2 with
         | Nq _ -> Nq (s1,s2)
         | Eq _ -> Eq (s1,s2))
    | _ -> Nq (s1,s2)


module BindEnv =
struct

  type elt = Dc of decl_desc | Df of def_desc

  type t = {
    bind : elt VarHashtbl.t;
    back : def_desc TermHashtbl.t;
  }

  let create n = {
    bind = VarHashtbl.create n;
    back = TermHashtbl.create n;
  }

  let decl t { decl_desc = Decl (v,_) as dc; _ } = VarHashtbl.add t.bind (any_var v) (Dc dc)

  let def_aux t (Def (v,_,tm) as df) =
    VarHashtbl.add t.bind (any_var v) (Df df);
    TermHashtbl.add t.back (any_term tm) df

  let rec def t { def_desc = Def (v,_,tm) as df; _ } =
    match is_var tm with
    | None -> def_aux t df
    | Some v' ->
      match VarHashtbl.find t.bind (any_var v') with
      | Dc _  -> def_aux t df
      | Df (Def (v',ls,tm)) ->
        (match equal_sort v.var_sort v'.var_sort with
         | Eq _ -> if v <> v' then def t (mk_def v ls tm)
         | Nq _ -> assert false)
      | exception Not_found -> def_aux t df

  let bind t { bind_desc; _ } =
    match bind_desc with
    | Let ls -> List.iter (def t) ls
    | Exists ls -> List.iter (decl t) ls
    | Forall ls -> List.iter (decl t) ls

  let undecl t { decl_desc = Decl (v,_); _ } = VarHashtbl.remove t.bind (any_var v)
  let undef  t { def_desc = Def (v,_,tm); _ } =
    VarHashtbl.remove t.bind (any_var v);
    TermHashtbl.remove t.back (any_term tm)

  let unbind t { bind_desc; _ } =
    match bind_desc with
    | Let ls -> List.iter (undef t) ls
    | Exists ls -> List.iter (undecl t) ls
    | Forall ls -> List.iter (undecl t) ls

  type 'a status =
    | Free
    | Declared of 'a var * any_sort list
    | Defined  of 'a var * any_var list * 'a term

  let lookup : type a. t -> a var -> a status =
    fun t u ->
      match VarHashtbl.find t.bind (any_var u) with
      | Dc (Decl (v,ls)) ->
        (match equal_sort u.var_sort v.var_sort with
         | Eq _ -> Declared (v,ls)
         | Nq _ -> assert false)
      | Df (Def (v,ls,tm)) ->
        (match equal_sort u.var_sort v.var_sort with
         | Eq _ -> Defined (v,ls,tm)
         | Nq _ -> assert false)
      | exception Not_found -> Free

  let is_binded : type a. t -> a term -> (a var * any_var list * a term) option
    = fun t tm ->
      try
        let Def (v,ls,tm') = TermHashtbl.find t.back (any_term tm) in
        match equal_sort tm.term_sort tm'.term_sort with
        | Eq _ -> Some (v,ls,tm')
        | Nq _ -> assert false
      with Not_found -> None

  let is_bool env bl =
    match is_bool bl with
    | Some _ as opt -> opt
    | None ->
      match is_var bl with
      | None -> None
      | Some v ->
        match lookup env v with
        | Free | Declared _ -> None
        | Defined (_,_,bl) -> is_bool bl

  let is_bitvec env bv =
    match is_bitvec bv with
    | Some _ as opt -> opt
    | None ->
      match is_var bv with
      | None -> None
      | Some v ->
        match lookup env v with
        | Free | Declared _ -> None
        | Defined (_,_,bv) -> is_bitvec bv

  let is_integer env ir =
    match is_integer ir with
    | Some _ as opt -> opt
    | None ->
      match is_var ir with
      | None -> None
      | Some v ->
        match lookup env v with
        | Free | Declared _ -> None
        | Defined (_,_,ir) -> is_integer ir

  let is_real env rl =
    match is_real rl with
    | Some _ as opt -> opt
    | None ->
      match is_var rl with
      | None -> None
      | Some v ->
        match lookup env v with
        | Free | Declared _ -> None
        | Defined (_,_,rl) -> is_real rl

  let is_var env tm =
    match is_var tm with
    | None -> None
    | Some v ->
      match lookup env v with
      | Free -> None
      | Declared (v,_) -> Some v
      | Defined (_,_,tm) -> is_var tm

end

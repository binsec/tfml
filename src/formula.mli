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

type result =
  | SAT
  | UNSAT
  | TIMEOUT
  | UNKNOWN

type bl         = private Bl
type bv         = private Bv
type ir         = private Ir
type rl         = private Rl
type ('a,'b) ax = private Ax

type (_,_) unop =
  | BlNot         : (bl,bl) unop

  | BvNot         : (bv,bv) unop
  | BvNeg         : (bv,bv) unop
  | BvRepeat      : int -> (bv,bv) unop
  | BvZeroExtend  : int -> (bv,bv) unop
  | BvSignExtend  : int -> (bv,bv) unop
  | BvRotateLeft  : int -> (bv,bv) unop
  | BvRotateRight : int -> (bv,bv) unop
  | BvExtract     : int Interval.t -> (bv,bv) unop

  | IrNeg         : (ir,ir) unop
  | IrAbs         : (ir,ir) unop
  | RlNeg         : (rl,rl) unop

type (_,_,_) bnop =
  | Equal         : ('a,'a,bl) bnop
  | Distinct      : ('a,'a,bl) bnop

  | BlImply       : (bl,bl,bl) bnop
  | BlAnd         : (bl,bl,bl) bnop
  | BlOr          : (bl,bl,bl) bnop
  | BlXor         : (bl,bl,bl) bnop

  | BvConcat      : (bv,bv,bv) bnop
  | BvAnd         : (bv,bv,bv) bnop
  | BvNand        : (bv,bv,bv) bnop
  | BvOr          : (bv,bv,bv) bnop
  | BvNor         : (bv,bv,bv) bnop
  | BvXor         : (bv,bv,bv) bnop
  | BvXnor        : (bv,bv,bv) bnop
  | BvCmp         : (bv,bv,bv) bnop
  | BvAdd         : (bv,bv,bv) bnop
  | BvSub         : (bv,bv,bv) bnop
  | BvMul         : (bv,bv,bv) bnop
  | BvUdiv        : (bv,bv,bv) bnop
  | BvSdiv        : (bv,bv,bv) bnop
  | BvUrem        : (bv,bv,bv) bnop
  | BvSrem        : (bv,bv,bv) bnop
  | BvSmod        : (bv,bv,bv) bnop
  | BvShl         : (bv,bv,bv) bnop
  | BvAshr        : (bv,bv,bv) bnop
  | BvLshr        : (bv,bv,bv) bnop

  | BvUlt         : (bv,bv,bl) bnop
  | BvUle         : (bv,bv,bl) bnop
  | BvUgt         : (bv,bv,bl) bnop
  | BvUge         : (bv,bv,bl) bnop
  | BvSlt         : (bv,bv,bl) bnop
  | BvSle         : (bv,bv,bl) bnop
  | BvSgt         : (bv,bv,bl) bnop
  | BvSge         : (bv,bv,bl) bnop

  | IrAdd         : (ir,ir,ir) bnop
  | IrSub         : (ir,ir,ir) bnop
  | IrMul         : (ir,ir,ir) bnop
  | IrDiv         : (ir,ir,ir) bnop
  | IrMod         : (ir,ir,ir) bnop

  | IrLt          : (ir,ir,bl) bnop
  | IrLe          : (ir,ir,bl) bnop
  | IrGt          : (ir,ir,bl) bnop
  | IrGe          : (ir,ir,bl) bnop

  | RlAdd         : (rl,rl,rl) bnop
  | RlSub         : (rl,rl,rl) bnop
  | RlMul         : (rl,rl,rl) bnop
  | RlDiv         : (rl,rl,rl) bnop

  | RlLt          : (rl,rl,bl) bnop
  | RlLe          : (rl,rl,bl) bnop
  | RlGt          : (rl,rl,bl) bnop
  | RlGe          : (rl,rl,bl) bnop

type _ sort_desc = private
  | BlSort : bl sort_desc
  | BvSort : int -> bv sort_desc
  | IrSort : ir sort_desc
  | RlSort : rl sort_desc
  | AxSort : 'a sort * 'b sort -> ('a,'b) ax sort_desc

and 'a sort = private {
  sort_hash : int;
  sort_desc : 'a sort_desc;
}

type any_sort = private AnySort : _ sort -> any_sort

type 'a var = private {
  var_hash : int;
  var_name : string;
  var_sort : 'a sort;
}

type any_var = private AnyVar : 'a var -> any_var

type 'a term_desc = private
  | Bool    : bool -> bl term_desc
  | BitVec  : Bitvector.t -> bv term_desc
  | Integer : Z.t -> ir term_desc
  | Real    : Q.t -> rl term_desc
  | Fun     : 'a var * any_term list -> 'a term_desc
  | Bind    : bind * 'a term -> 'a term_desc
  | Unop    : ('a,'b) unop * 'a term -> 'b term_desc
  | Bnop    : ('a,'b,'c) bnop * 'a term * 'b term -> 'c term_desc
  | Ite     : bl term * 'a term * 'a term -> 'a term_desc
  | Select  : ('a,'b) ax term * 'a term -> 'b term_desc
  | Store   : ('a,'b) ax term * 'a term * 'b term -> ('a,'b) ax term_desc

and 'a term = private {
  term_hash : int;
  term_desc : 'a term_desc;
  term_sort : 'a sort;
}

and any_term = private AnyTerm : 'a term -> any_term

and bind_desc =
  | Let    of def list
  | Exists of decl list
  | Forall of decl list

and bind = private {
  bind_hash : int;
  bind_desc : bind_desc;
}

and def_desc = private Def : 'a var * any_var list * 'a term -> def_desc

and def = private {
  def_hash : int;
  def_desc : def_desc
}

and decl_desc = private Decl : 'a var * any_sort list -> decl_desc

and decl = private {
  decl_hash : int;
  decl_desc : decl_desc;
}

type entry_desc = private
  | Declare of decl
  | Define  of def
  | Assert  of bl term
  | Comment of string

type entry = private {
  entry_hash : int;
  entry_desc : entry_desc;
}

type formula = private {
  logic   : string;
  entries : entry Sequence.t;
}

val empty  : formula
val length : formula -> int
val append : formula -> formula -> formula

val set_logic : string -> formula -> formula

val push_front : entry -> formula -> formula
val push_back  : entry -> formula -> formula

val peek_front : formula -> entry option
val peek_back  : formula -> entry option

val pop_front : formula -> formula option
val pop_back  : formula -> formula option

val map_forward  : (entry -> entry) -> formula -> formula
val map_backward : (entry -> entry) -> formula -> formula

val iter_forward  : (entry -> unit) -> formula -> unit
val iter_backward : (entry -> unit) -> formula -> unit

val fold_forward  : (entry -> 'a -> 'a) -> formula -> 'a -> 'a
val fold_backward : (entry -> 'a -> 'a) -> formula -> 'a -> 'a

val push_front_declare : decl    -> formula -> formula
val push_front_define  : def     -> formula -> formula
val push_front_assert  : bl term -> formula -> formula
val push_front_comment : string  -> formula -> formula

val push_back_declare  : decl    -> formula -> formula
val push_back_define   : def     -> formula -> formula
val push_back_assert   : bl term -> formula -> formula
val push_back_comment  : string  -> formula -> formula

(*val equal_sort : 'a sort -> 'a sort -> bool*)
val equal_term : 'a term -> 'a term -> bool

(* Smart constructors *)

val sort : 'a sort_desc -> 'a sort
val var  : string -> 'a sort -> 'a var
val term : 'a term_desc -> 'a term

val bind : bind_desc -> bind
val def  : def_desc  -> def
val decl : decl_desc -> decl

val any_sort : 'a sort -> any_sort
val any_var  : 'a var  -> any_var
val any_term : 'a term -> any_term

val mk_bl_sort : bl sort
val mk_bv_sort : int -> bv sort
val mk_ir_sort : ir sort
val mk_rl_sort : rl sort
val mk_ax_sort : 'a sort -> 'b sort -> ('a,'b) ax sort

val mk_bl_var : string -> bl var
val mk_bv_var : string -> int -> bv var
val mk_ax_var : string -> 'a sort -> 'b sort -> ('a,'b) ax var

val mk_bool    : bool -> bl term
val mk_bitvec  : Bitvector.t -> bv term
val mk_integer : Z.t -> ir term
val mk_real    : Q.t -> rl term
val mk_fun     : 'a var -> any_term list -> 'a term
val mk_bind    : bind -> 'a term -> 'a term
val mk_unop    : ('a,'b) unop -> 'a term -> 'b term
val mk_bnop    : ('a,'b,'c) bnop -> 'a term -> 'b term -> 'c term
val mk_ite     : bl term -> 'a term -> 'a term -> 'a term
val mk_select  : ('a,'b) ax term -> 'a term -> 'b term
val mk_store   : ('a,'b) ax term -> 'a term -> 'b term -> ('a,'b) ax term

val mk_let    : def list  -> bind
val mk_forall : decl list -> bind
val mk_exists : decl list -> bind

val mk_def  : 'a var -> any_var list -> 'a term -> def
val mk_decl : 'a var -> any_sort list -> decl

val mk_declare : decl -> entry
val mk_define  : def -> entry
val mk_assert  : bl term -> entry
val mk_comment : string -> entry

val mk_bl_true : bl term
val mk_bl_false: bl term

val mk_bv_zero : bv term
val mk_bv_one  : bv term

val mk_bv_zeros : int -> bv term
val mk_bv_ones  : int -> bv term
val mk_bv_fill  : int -> bv term

val mk_ir_zero : ir term
val mk_ir_one  : ir term

val mk_rl_zero : rl term
val mk_rl_one  : rl term

val mk_bl_not : bl term -> bl term

val mk_bv_not : bv term -> bv term
val mk_bv_neg : bv term -> bv term

val mk_bv_repeat       : int -> bv term -> bv term
val mk_bv_zero_extend  : int -> bv term -> bv term
val mk_bv_sign_extend  : int -> bv term -> bv term
val mk_bv_rotate_left  : int -> bv term -> bv term
val mk_bv_rotate_right : int -> bv term -> bv term
val mk_bv_extract : int Interval.t -> bv term -> bv term

val mk_ir_neg   : ir term -> ir term
val mk_ir_abs   : ir term -> ir term

val mk_rl_neg   : rl term -> rl term

val mk_equal    : 'a term -> 'a term -> bl term
val mk_distinct : 'a term -> 'a term -> bl term

val mk_bl_imply : bl term -> bl term -> bl term
val mk_bl_and   : bl term -> bl term -> bl term
val mk_bl_or    : bl term -> bl term -> bl term
val mk_bl_xor   : bl term -> bl term -> bl term

val mk_bv_concat : bv term -> bv term -> bv term
val mk_bv_and    : bv term -> bv term -> bv term
val mk_bv_nand   : bv term -> bv term -> bv term
val mk_bv_or     : bv term -> bv term -> bv term
val mk_bv_nor    : bv term -> bv term -> bv term
val mk_bv_xor    : bv term -> bv term -> bv term
val mk_bv_xnor   : bv term -> bv term -> bv term
val mk_bv_cmp    : bv term -> bv term -> bv term
val mk_bv_add    : bv term -> bv term -> bv term
val mk_bv_sub    : bv term -> bv term -> bv term
val mk_bv_mul    : bv term -> bv term -> bv term
val mk_bv_udiv   : bv term -> bv term -> bv term
val mk_bv_sdiv   : bv term -> bv term -> bv term
val mk_bv_urem   : bv term -> bv term -> bv term
val mk_bv_srem   : bv term -> bv term -> bv term
val mk_bv_smod   : bv term -> bv term -> bv term
val mk_bv_shl    : bv term -> bv term -> bv term
val mk_bv_ashr   : bv term -> bv term -> bv term
val mk_bv_lshr   : bv term -> bv term -> bv term

val mk_ir_add    : ir term -> ir term -> ir term
val mk_ir_sub    : ir term -> ir term -> ir term
val mk_ir_mul    : ir term -> ir term -> ir term
val mk_ir_div    : ir term -> ir term -> ir term
val mk_ir_mod    : ir term -> ir term -> ir term

val mk_rl_add    : rl term -> rl term -> rl term
val mk_rl_sub    : rl term -> rl term -> rl term
val mk_rl_mul    : rl term -> rl term -> rl term
val mk_rl_div    : rl term -> rl term -> rl term

val mk_bl_equal    : bl term -> bl term -> bl term
val mk_bl_distinct : bl term -> bl term -> bl term
val mk_bv_equal    : bv term -> bv term -> bl term
val mk_bv_distinct : bv term -> bv term -> bl term
val mk_ir_equal    : ir term -> ir term -> bl term
val mk_ir_distinct : ir term -> ir term -> bl term
val mk_rl_equal    : rl term -> rl term -> bl term
val mk_rl_distinct : rl term -> rl term -> bl term
val mk_ax_equal    : ('a,'b) ax term -> ('a,'b) ax term -> bl term
val mk_ax_distinct : ('a,'b) ax term -> ('a,'b) ax term -> bl term

val mk_bv_ult : bv term -> bv term -> bl term
val mk_bv_ule : bv term -> bv term -> bl term
val mk_bv_ugt : bv term -> bv term -> bl term
val mk_bv_uge : bv term -> bv term -> bl term
val mk_bv_slt : bv term -> bv term -> bl term
val mk_bv_sle : bv term -> bv term -> bl term
val mk_bv_sgt : bv term -> bv term -> bl term
val mk_bv_sge : bv term -> bv term -> bl term

val mk_ir_lt : ir term -> ir term -> bl term
val mk_ir_le : ir term -> ir term -> bl term
val mk_ir_gt : ir term -> ir term -> bl term
val mk_ir_ge : ir term -> ir term -> bl term

val mk_rl_lt : rl term -> rl term -> bl term
val mk_rl_le : rl term -> rl term -> bl term
val mk_rl_gt : rl term -> rl term -> bl term
val mk_rl_ge : rl term -> rl term -> bl term

val mk_bv_add_int : bv term -> int -> bv term
val mk_bv_sub_int : bv term -> int -> bv term

module VarHashtbl  : Hashtbl.S with type key = any_var
module VarMap      : Map.S     with type key = any_var
module VarSet      : Set.S     with type elt = any_var

module TermHashtbl : Hashtbl.S with type key = any_term
module TermMap     : Map.S     with type key = any_term
module TermSet     : Set.S     with type elt = any_term


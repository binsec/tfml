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

type _ sort_desc =
  | BlSort : bl sort_desc
  | BvSort : int -> bv sort_desc
  | IrSort : ir sort_desc
  | RlSort : rl sort_desc
  | AxSort : 'a sort * 'b sort -> ('a,'b) ax sort_desc

and 'a sort = {
  sort_hash : int;
  sort_desc : 'a sort_desc;
}

type any_sort = AnySort : _ sort -> any_sort

type 'a var = {
  var_hash : int;
  var_name : string;
  var_sort : 'a sort;
}

type any_var = AnyVar : 'a var -> any_var

type 'a term_desc =
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

and 'a term = {
  term_hash : int;
  term_desc : 'a term_desc;
  term_sort : 'a sort;
}

and any_term = AnyTerm : 'a term -> any_term

and bind_desc =
  | Let    of def list
  | Exists of decl list
  | Forall of decl list

and bind = {
  bind_hash : int;
  bind_desc : bind_desc;
}

and def_desc = Def : 'a var * any_var list * 'a term -> def_desc

and def = {
  def_hash : int;
  def_desc : def_desc
}

and decl_desc = Decl : 'a var * any_sort list -> decl_desc

and decl = {
  decl_hash : int;
  decl_desc : decl_desc;
}

type entry_desc =
  | Declare of decl
  | Define  of def
  | Assert  of bl term
  | Comment of string

type entry = {
  entry_hash : int;
  entry_desc : entry_desc;
}

type formula = {
  logic   : string;
  entries : entry Sequence.t;
}

(* Some utilities *)

let rec equal_sort : type a. a sort -> a sort -> bool =
  fun s1 s2 ->
    s1.sort_hash = s2.sort_hash &&
    match s1.sort_desc, s2.sort_desc with
    | BlSort, BlSort -> true
    | IrSort, IrSort -> true
    | RlSort, RlSort -> true
    | BvSort n1, BvSort n2 -> n1 = n2
    | AxSort (i1,e1), AxSort (i2,e2) ->
      equal_sort i1 i2 && equal_sort e1 e2

let equal_term tm1 tm2 = tm1 = tm2

let is_bool bl =
  match bl.term_desc with
  | Bool bl -> Some bl
  | _ -> None

let is_bitvec bv =
  match bv.term_desc with
  | BitVec bv -> Some bv
  | _ -> None

let is_integer ir =
  match ir.term_desc with
  | Integer ir -> Some ir
  | _ -> None

let is_real rl =
  match rl.term_desc with
  | Real rl -> Some rl
  | _ -> None

(* Smart constructors *)

let sort : type a. a sort_desc -> a sort =
  fun sort_desc ->
    match sort_desc with
    | BlSort ->
      let sort_hash = Hashtbl.hash (sort_desc) in
      { sort_hash; sort_desc }
    | BvSort _ ->
      let sort_hash = Hashtbl.hash (sort_desc) in
      { sort_hash; sort_desc }
    | IrSort ->
      let sort_hash = Hashtbl.hash (sort_desc) in
      { sort_hash; sort_desc }
    | RlSort ->
      let sort_hash = Hashtbl.hash (sort_desc) in
      { sort_hash; sort_desc }
    | AxSort (idx,elt) ->
      let sort_hash = Hashtbl.hash (idx.sort_hash, elt.sort_hash) in
      { sort_hash; sort_desc }

let any_sort sort = AnySort sort

let mk_bl_sort = sort BlSort
let mk_bv_sort size = sort (BvSort size)
let mk_ir_sort = sort IrSort
let mk_rl_sort = sort RlSort
let mk_ax_sort idx elt = sort (AxSort (idx, elt))

let var var_name var_sort =
  let var_hash = Hashtbl.hash (var_name, var_sort) in
  { var_hash; var_name; var_sort }

let mk_bl_var name = var name mk_bl_sort
let mk_bv_var name size = var name (mk_bv_sort size)
let mk_ax_var name idx elt = var name (mk_ax_sort idx elt)

let any_var var = AnyVar var

let list_hash (f: 'a -> int) (l: 'a list) =
  Hashtbl.hash (List.map f l)

let bv_term_size (bv: bv term) : int =
  match bv.term_sort.sort_desc with
  | BvSort i -> i

let sort_unop : type a b. (a,b) unop -> a term -> b sort =
  fun u ({ term_sort = ts; _ } as tm) ->
    match u with
    | BlNot -> ts
    | BvNot -> ts
    | BvNeg -> ts
    | BvRepeat i -> assert (i > 0); mk_bv_sort (i * bv_term_size tm)
    | BvZeroExtend i -> assert (i >= 0); mk_bv_sort (i + bv_term_size tm)
    | BvSignExtend i -> assert (i >= 0); mk_bv_sort (i + bv_term_size tm)
    | BvRotateLeft _ -> ts
    | BvRotateRight _ -> ts
    | BvExtract i -> mk_bv_sort Interval.(i.hi - i.lo + 1)
    | IrNeg -> ts
    | IrAbs -> ts
    | RlNeg -> ts

let sort_bnop : type a b c. (a,b,c) bnop -> a term -> b term -> c sort =
  fun b ({ term_sort = ts1; _ } as tm1) ({ term_sort = ts2; _ } as tm2) ->
    match b with
    | Equal    -> assert (equal_sort ts1 ts2); mk_bl_sort
    | Distinct -> assert (equal_sort ts1 ts2); mk_bl_sort

    | BlImply  -> ts1
    | BlAnd    -> ts1
    | BlOr     -> ts1
    | BlXor    -> ts1

    | BvConcat -> mk_bv_sort (bv_term_size tm1 + bv_term_size tm2)
    | BvAnd    -> assert (equal_sort ts1 ts2); ts1
    | BvNand   -> assert (equal_sort ts1 ts2); ts1
    | BvOr     -> assert (equal_sort ts1 ts2); ts1
    | BvNor    -> assert (equal_sort ts1 ts2); ts1
    | BvXor    -> assert (equal_sort ts1 ts2); ts1
    | BvXnor   -> assert (equal_sort ts1 ts2); ts1
    | BvCmp    -> assert (equal_sort ts1 ts2); mk_bv_sort 1
    | BvAdd    -> assert (equal_sort ts1 ts2); ts1
    | BvSub    -> assert (equal_sort ts1 ts2); ts1
    | BvMul    -> assert (equal_sort ts1 ts2); ts1
    | BvUdiv   -> assert (equal_sort ts1 ts2); ts1
    | BvSdiv   -> assert (equal_sort ts1 ts2); ts1
    | BvUrem   -> assert (equal_sort ts1 ts2); ts1
    | BvSrem   -> assert (equal_sort ts1 ts2); ts1
    | BvSmod   -> assert (equal_sort ts1 ts2); ts1
    | BvShl    -> assert (equal_sort ts1 ts2); ts1
    | BvAshr   -> assert (equal_sort ts1 ts2); ts1
    | BvLshr   -> assert (equal_sort ts1 ts2); ts1

    | BvUlt    -> assert (equal_sort ts1 ts2); mk_bl_sort
    | BvUle    -> assert (equal_sort ts1 ts2); mk_bl_sort
    | BvUgt    -> assert (equal_sort ts1 ts2); mk_bl_sort
    | BvUge    -> assert (equal_sort ts1 ts2); mk_bl_sort
    | BvSlt    -> assert (equal_sort ts1 ts2); mk_bl_sort
    | BvSle    -> assert (equal_sort ts1 ts2); mk_bl_sort
    | BvSgt    -> assert (equal_sort ts1 ts2); mk_bl_sort
    | BvSge    -> assert (equal_sort ts1 ts2); mk_bl_sort

    | IrAdd    -> ts1
    | IrSub    -> ts1
    | IrMul    -> ts1
    | IrDiv    -> ts1
    | IrMod    -> ts1

    | IrLt     -> mk_bl_sort
    | IrLe     -> mk_bl_sort
    | IrGt     -> mk_bl_sort
    | IrGe     -> mk_bl_sort

    | RlAdd    -> ts1
    | RlSub    -> ts1
    | RlMul    -> ts1
    | RlDiv    -> ts1

    | RlLt     -> mk_bl_sort
    | RlLe     -> mk_bl_sort
    | RlGt     -> mk_bl_sort
    | RlGe     -> mk_bl_sort

let mk_bool : bool -> bl term =
  fun bl ->
    let term_desc = Bool bl in
    let term_hash = -(Hashtbl.hash bl) in
    let term_sort = mk_bl_sort in
    { term_hash; term_sort; term_desc }

let mk_bl_true  = mk_bool true
let mk_bl_false = mk_bool false

let mk_bitvec : Bitvector.t -> bv term =
  fun bv ->
    let term_desc = BitVec bv in
    let term_hash = -(Hashtbl.hash bv) in
    let term_sort = mk_bv_sort (Bitvector.size_of bv) in
    { term_hash; term_sort; term_desc }

let mk_integer : Z.t -> ir term =
  fun ir ->
    let term_desc = Integer ir in
    let term_hash = -(Z.hash ir) in
    let term_sort = mk_ir_sort in
    { term_hash; term_sort; term_desc }

let mk_real : Q.t -> rl term =
  fun ir ->
    let term_desc = Real ir in
    let term_hash = -(Hashtbl.hash ir) in
    let term_sort = mk_rl_sort in
    { term_hash; term_sort; term_desc }

let mk_bv_zero = mk_bitvec Bitvector.zero
let mk_bv_one  = mk_bitvec Bitvector.one

let mk_bv_zeros n = mk_bitvec (Bitvector.zeros n)
let mk_bv_ones  n = mk_bitvec (Bitvector.ones n)
let mk_bv_fill  n = mk_bitvec (Bitvector.fill n)

let mk_ir_zero = mk_integer Z.zero
let mk_ir_one  = mk_integer Z.one

let mk_rl_zero = mk_real Q.zero
let mk_rl_one  = mk_real Q.one

let rec term : type a. a term_desc -> a term =
  fun term_desc ->
    match term_desc with
    | Bool bl -> mk_bool bl
    | BitVec bv -> mk_bitvec bv
    | Integer ir -> mk_integer ir
    | Real rl -> mk_real rl

    | Fun (({ var_sort = term_sort; _ } as v),ls) ->
      let term_hash =
        Hashtbl.hash (v.var_hash, list_hash (fun (AnyTerm tm) -> tm.term_hash) ls)
      in { term_hash; term_sort; term_desc }

    | Bind (bn,({ term_sort; _ } as tm)) ->
      if (match bn.bind_desc with
          | Let l -> l = []
          | Exists l -> l = []
          | Forall l -> l = [])
      then tm else
        let term_hash = Hashtbl.hash (bn.bind_hash, tm.term_hash) in
        { term_hash; term_sort; term_desc }

    | Unop (u,tm) ->
      let term_hash = Hashtbl.hash (u,tm.term_hash) in
      let term_sort = sort_unop u tm in

      (match tm.term_sort.sort_desc, term_sort.sort_desc with
       | BlSort, BlSort ->
         (match is_bool tm with
          | None ->
            (match u with
             | BlNot ->
               match tm.term_desc with
               | Unop (BlNot, bl) -> bl
               | Bnop (Equal, tm1, tm2) -> mk_bnop Distinct tm1 tm2
               | Bnop (Distinct, tm1, tm2) -> mk_bnop Equal tm1 tm2
               | _ -> { term_hash; term_sort; term_desc })

          | Some bool ->
            match u with
            | BlNot -> mk_bool (not bool))

       | BvSort size, BvSort _ ->
         (match is_bitvec tm with
          | None ->
            (match u with
             | BvNot ->
               (match tm.term_desc with
                | Unop (BvNot, bv) -> bv
                | _ -> { term_hash; term_sort; term_desc })

             | BvNeg ->
               (match tm.term_desc with
                | Unop (BvNeg, bv) -> bv
                | Bnop (BvAdd, bv1, bv2) ->
                  (match is_bitvec bv1, is_bitvec bv2 with
                   | Some bv1, None -> mk_bnop BvSub (mk_bitvec (Bitvector.neg bv1)) bv2
                   | None, Some bv2 -> mk_bnop BvSub (mk_bitvec (Bitvector.neg bv2)) bv1
                   | _ -> { term_hash; term_sort; term_desc })
                | Bnop (BvSub, bv1, bv2) -> mk_bnop BvSub bv2 bv1
                | _ -> { term_hash; term_sort; term_desc })

             | BvRepeat i ->
               if i = 1 then tm
               else
                 (match tm.term_desc with
                  | Unop (BvRepeat j, bv) ->
                    mk_unop (BvRepeat (i*j)) bv
                  |  _ -> { term_hash; term_sort; term_desc })

             | BvZeroExtend i ->
               if i = 0 then tm
               else
                 (match tm.term_desc with
                  | Unop (BvZeroExtend j, bv) ->
                    mk_unop (BvZeroExtend (i+j)) bv
                  |  _ -> { term_hash; term_sort; term_desc })

             | BvSignExtend i ->
               if i = 0 then tm
               else
                 (match tm.term_desc with
                  | Unop (BvSignExtend j, bv) ->
                    mk_unop (BvSignExtend (i+j)) bv
                  |  _ -> { term_hash; term_sort; term_desc })

             | BvRotateLeft i ->
               if i = 0 then tm
               else
                 (match tm.term_desc with
                  | Unop (BvRotateLeft j, bv) ->
                    mk_unop (BvRotateLeft ((i+j) mod size)) bv
                  | Unop (BvRotateRight j, bv) ->
                    if i > j then mk_unop (BvRotateLeft (i-j)) bv
                    else mk_unop (BvRotateRight (j-i)) bv
                  | _ -> { term_hash; term_sort; term_desc })

             | BvRotateRight i ->
               if i = 0 then tm
               else
                 (match tm.term_desc with
                  | Unop (BvRotateLeft j, bv) ->
                    if i > j then mk_unop (BvRotateRight (i-j)) bv
                    else mk_unop (BvRotateLeft (j-i)) bv
                  | Unop (BvRotateRight j, bv) ->
                    mk_unop (BvRotateRight ((i+j) mod size)) bv
                  | _ -> { term_hash; term_sort; term_desc })

             | BvExtract i ->
               if i.Interval.lo = 0 && i.Interval.hi = size - 1 then tm
               else
                 match tm.term_desc with
                 | Unop (BvZeroExtend _, bv) ->
                   if i.Interval.hi < bv_term_size bv
                   then mk_unop (BvExtract i) bv
                   else if i.Interval.lo >= bv_term_size bv
                   then mk_bv_zeros size
                   else { term_hash; term_sort; term_desc }
                 | Unop (BvSignExtend _, bv) ->
                   if i.Interval.hi < bv_term_size bv
                   then mk_unop (BvExtract i) bv
                   else { term_hash; term_sort; term_desc }
                 | Unop (BvExtract j, bv) ->
                   let lo = i.Interval.lo + j.Interval.lo in
                   let hi = i.Interval.hi + j.Interval.lo in
                   mk_unop (BvExtract Interval.{lo; hi}) bv
                 | Bnop (BvConcat, bv1, bv2) ->
                   if i.Interval.hi < bv_term_size bv2
                   then mk_unop (BvExtract i) bv2
                   else if i.Interval.lo >= bv_term_size bv2
                   then
                     let lo = i.Interval.lo - bv_term_size bv2 in
                     let hi = i.Interval.hi - bv_term_size bv2 in
                     mk_unop (BvExtract Interval.{lo; hi}) bv1
                   else { term_hash; term_sort; term_desc }
                 | _ -> { term_hash; term_sort; term_desc })

          | Some bv ->
            match u with
            | BvNot -> mk_bitvec (Bitvector.lognot bv)
            | BvNeg -> mk_bitvec (Bitvector.neg bv)
            | BvRepeat i -> mk_bitvec (Bitvector.concat (Utils.list_init i (fun _ -> bv)))
            | BvZeroExtend i -> mk_bitvec Bitvector.(extend bv (size_of bv + i))
            | BvSignExtend i -> mk_bitvec Bitvector.(extend_signed bv (size_of bv + i))
            | BvRotateLeft i -> mk_bitvec (Bitvector.rotate_left bv i)
            | BvRotateRight i -> mk_bitvec (Bitvector.rotate_right bv i)
            | BvExtract i -> mk_bitvec (Bitvector.extract bv i))

       | IrSort, IrSort ->
         (match is_integer tm with
          | None -> { term_hash; term_sort; term_desc }
          | Some ir ->
            match u with
            | IrNeg -> mk_integer (Z.neg ir)
            | IrAbs -> mk_integer (Z.abs ir))

       | RlSort, RlSort ->
         (match is_real tm with
          | None -> { term_hash; term_sort; term_desc }
          | Some rl ->
            match u with
            | RlNeg -> mk_real (Q.neg rl))

       | _ -> { term_hash; term_sort; term_desc })

    | Bnop (b,tm1,tm2) ->
      let term_hash = Hashtbl.hash (b,tm1.term_hash,tm2.term_hash) in
      let term_sort = sort_bnop b tm1 tm2 in
      (match tm1.term_sort.sort_desc, tm2.term_sort.sort_desc, term_sort.sort_desc with
       | BlSort, BlSort, BlSort ->
         (match is_bool tm1, is_bool tm2 with
          | None, None ->
            (* syntactic equality *)
            if equal_term tm1 tm2 then
              match b with
              | BlImply  -> mk_bl_true
              | BlAnd    -> tm1
              | BlOr     -> tm1
              | BlXor    -> mk_bl_false
              | Equal    -> mk_bl_true
              | Distinct -> mk_bl_false
            else { term_hash; term_sort; term_desc }

          | Some bl1, None ->
            (match b with
             | BlImply  -> if not bl1 then mk_bl_true else tm2
             | BlAnd    -> if bl1 then tm2 else mk_bl_false
             | BlOr     -> if bl1 then mk_bl_true else tm2
             | BlXor    -> if bl1 then mk_unop BlNot tm2 else tm2
             | Equal    -> if bl1 then tm2 else mk_unop BlNot tm2
             | Distinct -> if bl1 then mk_unop BlNot tm2 else tm2)

          | None, Some bl2 ->
            (match b with
             | BlImply  -> if bl2 then mk_bl_true else tm1
             | BlAnd    -> if bl2 then tm1 else mk_bl_false
             | BlOr     -> if bl2 then mk_bl_true else tm1
             | BlXor    -> if bl2 then mk_unop BlNot tm1 else tm1
             | Equal    -> if bl2 then tm1 else mk_unop BlNot tm1
             | Distinct -> if bl2 then mk_unop BlNot tm1 else tm1)

          | Some bl1, Some bl2 ->
            match b with
            | BlImply  -> mk_bool (not bl1 || bl2)
            | BlAnd    -> mk_bool (bl1 && bl2)
            | BlOr     -> mk_bool (bl1 || bl2)
            | BlXor    -> mk_bool (bl1 <> bl2)
            | Equal    -> mk_bool (bl1 =  bl2)
            | Distinct -> mk_bool (bl1 <> bl2))

       | BvSort _, BvSort _, BlSort ->
         (match is_bitvec tm1, is_bitvec tm2 with
          | None, None ->
            (* syntactic equality *)
            (match is_bitvec (mk_bnop BvSub tm1 tm2) with
             | None -> { term_hash; term_sort; term_desc }
             | Some bv ->
               let open Bitvector in
               match b with
               | Equal -> mk_bool (is_zeros bv)
               | Distinct -> mk_bool (not (is_zeros bv))
               | _ -> { term_hash; term_sort; term_desc })
          (*| BvUlt -> mk_bool (ult bv (zeros (size_of bv)))
            | BvUle -> mk_bool (ule bv (zeros (size_of bv)))
            | BvUgt -> mk_bool (ugt bv (zeros (size_of bv)))
            | BvUge -> mk_bool (uge bv (zeros (size_of bv)))
            | BvSlt -> mk_bool (slt bv (zeros (size_of bv)))
            | BvSle -> mk_bool (sle bv (zeros (size_of bv)))
            | BvSgt -> mk_bool (sgt bv (zeros (size_of bv)))
            | BvSge -> mk_bool (sge bv (zeros (size_of bv)))*)

          | Some _, None ->
            (match b with
             | Equal    -> mk_bnop Equal tm2 tm1
             | Distinct -> mk_bnop Distinct tm2 tm1
             | BvUlt    -> mk_bnop BvUgt tm2 tm1
             | BvUle    -> mk_bnop BvUge tm2 tm1
             | BvUgt    -> mk_bnop BvUlt tm2 tm1
             | BvUge    -> mk_bnop BvUle tm2 tm1
             | BvSlt    -> mk_bnop BvSgt tm2 tm1
             | BvSle    -> mk_bnop BvSge tm2 tm1
             | BvSgt    -> mk_bnop BvSlt tm2 tm1
             | BvSge    -> mk_bnop BvSle tm2 tm1)

          | None, Some bv ->
            (match b with
             | Equal ->
               (match tm1.term_desc with
                | Unop (BvNot,tm) -> mk_bnop Equal tm (mk_bitvec (Bitvector.lognot bv))
                | Bnop (BvCmp,bv1,bv2) ->
                  if Bitvector.is_one bv
                  then mk_bnop Equal bv1 bv2
                  else mk_bnop Distinct bv1 bv2
                | Bnop (BvAdd,bv1,bv2) ->
                  (match is_bitvec bv2 with
                   | Some bv2 -> mk_bnop Equal bv1 (mk_bitvec (Bitvector.sub bv bv2))
                   | None -> { term_hash; term_sort; term_desc })
                | Bnop (BvSub,bv1,bv2) ->
                  (match is_bitvec bv1 with
                   | Some bv1 -> mk_bnop Equal bv2 (mk_bitvec (Bitvector.sub bv1 bv))
                   | None ->
                     match is_bitvec bv2 with
                     | Some bv2 -> mk_bnop Equal bv1 (mk_bitvec (Bitvector.add bv bv2))
                     | None -> { term_hash; term_sort; term_desc })
                | Ite (bl,bv1,bv2) ->
                  (match is_bitvec bv1, is_bitvec bv2 with
                   | None, None | None, Some _ | Some _, None -> { term_hash; term_sort; term_desc }
                   | Some bv1, Some bv2 ->
                     if Bitvector.(equal bv bv1 && not (equal bv bv2)) then bl
                     else if Bitvector.(not (equal bv bv1) && equal bv bv2) then mk_unop BlNot bl
                     else { term_hash; term_sort; term_desc })
                | _ -> { term_hash; term_sort; term_desc })

             | Distinct ->
               (match tm1.term_desc with
                | Unop (BvNot,tm) -> mk_bnop Distinct tm (mk_bitvec (Bitvector.lognot bv))
                | Bnop (BvCmp,bv1,bv2) ->
                  if Bitvector.is_zero bv
                  then mk_bnop Equal bv1 bv2
                  else mk_bnop Distinct bv1 bv2
                | Ite (bl,bv1,bv2) ->
                  (match is_bitvec bv1, is_bitvec bv2 with
                   | None, None | None, Some _ | Some _, None -> { term_hash; term_sort; term_desc }
                   | Some bv1, Some bv2 ->
                     if Bitvector.(not (equal bv bv1) && equal bv bv2) then bl
                     else if Bitvector.(equal bv bv1 && not (equal bv bv2)) then mk_unop BlNot bl
                     else { term_hash; term_sort; term_desc })
                | _ -> { term_hash; term_sort; term_desc })

             | _ -> { term_hash; term_sort; term_desc })

          | Some bv1, Some bv2 ->
            let open Bitvector in
            match b with
            | Equal -> mk_bool (equal bv1 bv2)
            | Distinct -> mk_bool (diff bv1 bv2)
            | BvUlt -> mk_bool (ult bv1 bv2)
            | BvUle -> mk_bool (ule bv1 bv2)
            | BvUgt -> mk_bool (ugt bv1 bv2)
            | BvUge -> mk_bool (uge bv1 bv2)
            | BvSlt -> mk_bool (slt bv1 bv2)
            | BvSle -> mk_bool (sle bv1 bv2)
            | BvSgt -> mk_bool (sgt bv1 bv2)
            | BvSge -> mk_bool (sge bv1 bv2))

       | BvSort _, BvSort _, BvSort size  ->
         (match is_bitvec tm1, is_bitvec tm2 with
          | None, None ->
            (match b with
             | BvAnd | BvOr ->
               if equal_term tm1 tm2 then tm1
               else { term_hash; term_sort; term_desc }
             | BvNand | BvNor ->
               if equal_term tm1 tm2 then mk_unop BvNot tm1
               else { term_hash; term_sort; term_desc }
             | BvXor ->
               if equal_term tm1 tm2 then mk_bv_zeros size
               else { term_hash; term_sort; term_desc }
             | BvXnor ->
               if equal_term tm1 tm2 then mk_bv_fill size
               else { term_hash; term_sort; term_desc }
             | BvCmp ->
               if equal_term tm1 tm2 then mk_bv_one
               else { term_hash; term_sort; term_desc }

             | BvAdd ->
               (match tm1.term_desc, tm2.term_desc with
                | _, Unop (BvNeg, bv2) -> mk_bnop BvSub tm1 bv2
                | Unop (BvNeg, bv1), _ -> mk_bnop BvSub tm2 bv1

                | Bnop (BvAdd, bv11, bv12), Bnop (BvAdd, bv21, bv22) -> (* (w+x) + (y+z) *)
                  (match is_bitvec bv11, is_bitvec bv12,
                         is_bitvec bv21, is_bitvec bv22 with
                  | Some bv11, None, Some bv21, None ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv12 bv22) (mk_bitvec (Bitvector.add bv11 bv21))
                  | Some bv11, None, None, Some bv22 ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv12 bv21) (mk_bitvec (Bitvector.add bv11 bv22))
                  | None, Some bv12, Some bv21, None ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv11 bv22) (mk_bitvec (Bitvector.add bv12 bv21))
                  | None, Some bv12, None, Some bv22 ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv11 bv21) (mk_bitvec (Bitvector.add bv12 bv22))
                  | Some _, None, None, None ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv12 tm2) bv11
                  | None, Some _, None, None ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv11 tm2) bv12
                  | None, None, Some _, None ->
                    mk_bnop BvAdd (mk_bnop BvAdd tm1 bv22) bv21
                  | None, None, None, Some _ ->
                    mk_bnop BvAdd (mk_bnop BvAdd tm1 bv21) bv22
                  | _ -> { term_hash; term_sort; term_desc })

                | Bnop (BvSub, bv11, bv12), Bnop (BvSub, bv21, bv22) -> (* (w-x) + (y-z) *)
                  mk_bnop BvSub (mk_bnop BvAdd bv11 bv21) (mk_bnop BvAdd bv12 bv22)

                | Bnop (BvAdd, bv11, bv12), Bnop (BvSub, bv21, bv22) -> (* (w+x) + (y-z) *)
                  (match is_bitvec bv11, is_bitvec bv12,
                         is_bitvec bv21, is_bitvec bv22 with
                  | Some bv11, None, Some bv21, None ->
                    mk_bnop BvAdd (mk_bnop BvSub bv12 bv22) (mk_bitvec (Bitvector.add bv11 bv21))
                  | Some bv11, None, None, Some bv22 ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv12 bv21) (mk_bitvec (Bitvector.sub bv11 bv22))
                  | None, Some bv12, Some bv21, None ->
                    mk_bnop BvAdd (mk_bnop BvSub bv11 bv22) (mk_bitvec (Bitvector.add bv12 bv21))
                  | None, Some bv12, None, Some bv22 ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv11 bv21) (mk_bitvec (Bitvector.sub bv12 bv22))
                  | Some _, None, None, None ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv12 tm2) bv11
                  | None, Some _, None, None ->
                    mk_bnop BvAdd (mk_bnop BvAdd bv11 tm2) bv12
                  | None, None, Some _, None ->
                    mk_bnop BvAdd (mk_bnop BvSub tm1 bv22) bv21
                  | None, None, None, Some _ ->
                    mk_bnop BvSub (mk_bnop BvAdd tm1 bv21) bv22
                  | _ -> { term_hash; term_sort; term_desc })

                | Bnop (BvSub, _, _), Bnop (BvAdd, _, _) -> (* (w-x) + (y+z) *)
                  mk_bnop BvAdd tm2 tm1

                | _, _ -> { term_hash; term_sort; term_desc })

             | BvSub ->
               if equal_term tm1 tm2 then mk_bv_zeros size
               else
                 (match tm1.term_desc, tm2.term_desc with
                  | _, Unop (BvNeg, bv2) -> mk_bnop BvAdd tm1 bv2

                  | _, Bnop (BvSub, bv21, bv22) -> (* x - (y-z) *)
                    mk_bnop BvAdd tm1 (mk_bnop BvSub bv22 bv21)

                  | Bnop (BvAdd, bv11, bv12), Bnop (BvAdd, bv21, bv22) -> (* (w+x) - (y+z) *)
                    (match is_bitvec bv11, is_bitvec bv12,
                           is_bitvec bv21, is_bitvec bv22 with
                    | Some bv11, None, Some bv21, None ->
                      mk_bnop BvAdd (mk_bnop BvSub bv12 bv22) (mk_bitvec (Bitvector.sub bv11 bv21))
                    | Some bv11, None, None, Some bv22 ->
                      mk_bnop BvAdd (mk_bnop BvSub bv21 bv12) (mk_bitvec (Bitvector.sub bv11 bv22))
                    | None, Some bv12, Some bv21, None ->
                      mk_bnop BvAdd (mk_bnop BvSub bv11 bv22) (mk_bitvec (Bitvector.sub bv21 bv12))
                    | None, Some bv12, None, Some bv22 ->
                      mk_bnop BvAdd (mk_bnop BvSub bv11 bv21) (mk_bitvec (Bitvector.sub bv12 bv22))
                    | Some _, None, None, None ->
                      mk_bnop BvAdd (mk_bnop BvSub bv12 tm2) bv11
                    | None, Some _, None, None ->
                      mk_bnop BvAdd (mk_bnop BvSub bv11 tm2) bv12
                    | None, None, Some _, None ->
                      mk_bnop BvSub (mk_bnop BvSub tm1 bv22) bv21
                    | None, None, None, Some _ ->
                      mk_bnop BvSub (mk_bnop BvSub tm1 bv21) bv22
                    | _ -> { term_hash; term_sort; term_desc })

                  | Bnop (BvSub, bv11, bv12), Bnop (BvAdd, bv21, bv22) ->
                    (match is_bitvec bv11, is_bitvec bv12,
                           is_bitvec bv21, is_bitvec bv22 with
                    | Some bv11, None, Some bv21, None ->
                      mk_bnop BvSub (mk_bitvec (Bitvector.sub bv11 bv21)) (mk_bnop BvAdd bv12 bv22)
                    | Some bv11, None, None, Some bv22 ->
                      mk_bnop BvSub (mk_bitvec (Bitvector.sub bv11 bv22)) (mk_bnop BvAdd bv12 bv21)
                    | None, Some bv12, Some bv21, None ->
                      mk_bnop BvSub (mk_bnop BvSub bv11 bv22) (mk_bitvec (Bitvector.add bv12 bv21))
                    | None, Some bv12, None, Some bv22 ->
                      mk_bnop BvSub (mk_bnop BvSub bv11 bv21) (mk_bitvec (Bitvector.add bv12 bv22))
                    | Some _, None, None, None ->
                      mk_bnop BvSub bv11 (mk_bnop BvAdd tm2 bv12)
                    | None, Some _, None, None ->
                      mk_bnop BvSub (mk_bnop BvSub bv11 tm2) bv12
                    | None, None, Some _, None ->
                      mk_bnop BvSub (mk_bnop BvSub tm1 bv22) bv21
                    | None, None, None, Some _ ->
                      mk_bnop BvSub (mk_bnop BvSub tm1 bv21) bv22
                    | _, _, _, _ -> { term_hash; term_sort; term_desc })

                  | Unop (BvNeg, bv), Bnop (BvAdd, bv1, bv2) -> (* -x - (y+z) *)
                    (match is_bitvec bv1, is_bitvec bv2 with
                     | Some bv1, None ->
                       mk_bnop BvSub (mk_bitvec (Bitvector.neg bv1)) (mk_bnop BvAdd bv bv2)
                     | None, Some bv2 ->
                       mk_bnop BvSub (mk_bitvec (Bitvector.neg bv2)) (mk_bnop BvAdd bv bv1)
                     | _, _ -> { term_hash; term_sort; term_desc })

                  | _, _ -> { term_hash; term_sort; term_desc })

             | BvConcat ->
               (match tm1.term_desc, tm2.term_desc with
                | Unop (BvExtract i, b1), Unop (BvExtract j, b2) ->
                  if i.Interval.lo = j.Interval.hi + 1
                  then
                    if equal_term b1 b2
                    then mk_unop (BvExtract Interval.{lo = j.lo; hi = i.hi}) b1
                    else
                      match b1.term_desc with
                      | Unop (BvZeroExtend _, bv) ->
                        if j.Interval.hi < bv_term_size bv && equal_term bv tm2
                        then mk_unop (BvExtract Interval.{lo = j.lo; hi = i.hi}) b1
                        else { term_hash; term_sort; term_desc }
                      | Unop (BvSignExtend _, bv) ->
                        if j.Interval.hi < bv_term_size bv && equal_term bv tm2
                        then mk_unop (BvExtract Interval.{lo = j.lo; hi = i.hi}) b1
                        else { term_hash; term_sort; term_desc }
                      | _ -> { term_hash; term_sort; term_desc }
                  else { term_hash; term_sort; term_desc }
                | _, _ -> { term_hash; term_sort; term_desc })

             | BvMul  | BvUdiv | BvSdiv
             | BvUrem | BvSrem | BvSmod
             | BvShl  | BvAshr | BvLshr ->
               { term_hash; term_sort; term_desc })

          | None, Some bv ->
            (match b with
             | BvAnd ->
               if Bitvector.is_zeros bv then tm2
               else if Bitvector.is_fill bv then tm1
               else { term_hash; term_sort; term_desc }
             | BvNand ->
               if Bitvector.is_zeros bv then mk_bv_fill size
               else if Bitvector.is_fill bv then mk_unop BvNot tm1
               else { term_hash; term_sort; term_desc }
             | BvOr ->
               if Bitvector.is_fill bv then tm2
               else if Bitvector.is_zeros bv then tm1
               else { term_hash; term_sort; term_desc }
             | BvNor ->
               if Bitvector.is_fill bv then mk_bv_zeros size
               else if Bitvector.is_zeros bv then mk_unop BvNot tm1
               else { term_hash; term_sort; term_desc }
             | BvXor ->
               if Bitvector.is_zeros bv then tm1
               else if Bitvector.is_fill bv then mk_unop BvNot tm1
               else { term_hash; term_sort; term_desc }
             | BvXnor ->
               if Bitvector.is_zeros bv then mk_unop BvNot tm1
               else if Bitvector.is_fill bv then tm1
               else { term_hash; term_sort; term_desc }

             | BvAdd ->
               if Bitvector.is_zeros bv then tm1
               else if Bitvector.is_neg bv then
                 mk_bnop BvSub tm1 (mk_bitvec (Bitvector.neg bv))
               else
                 (match tm1.term_desc with
                  | Unop (BvNeg, bv1) -> mk_bnop BvSub tm2 bv1
                  | Bnop (BvAdd, bv1, bv2) ->
                    (match is_bitvec bv1, is_bitvec bv2 with
                     | Some bv1, None -> mk_bnop BvAdd bv2 (mk_bitvec (Bitvector.add bv bv1))
                     | None, Some bv2 -> mk_bnop BvAdd bv1 (mk_bitvec (Bitvector.add bv bv2))
                     | _ -> { term_hash; term_sort; term_desc })
                  | Bnop (BvSub, bv1, bv2) ->
                    (match is_bitvec bv1, is_bitvec bv2 with
                     | Some bv1, None -> mk_bnop BvSub (mk_bitvec (Bitvector.add bv bv1)) bv2
                     | None, Some bv2 -> mk_bnop BvAdd bv1 (mk_bitvec (Bitvector.sub bv bv2))
                     | _ -> { term_hash; term_sort; term_desc })
                  | _ -> { term_hash; term_sort; term_desc })

             | BvSub ->
               if Bitvector.is_zeros bv then tm1
               else if Bitvector.is_neg bv then
                 mk_bnop BvAdd tm1 (mk_bitvec (Bitvector.neg bv))
               else
                 (match tm1.term_desc with
                  | Unop (BvNeg, bv1) -> mk_bnop BvSub (mk_bitvec (Bitvector.neg bv)) bv1
                  | Bnop (BvAdd, bv1, bv2) ->
                    (match is_bitvec bv1, is_bitvec bv2 with
                     | Some bv1, None -> mk_bnop BvAdd bv2 (mk_bitvec (Bitvector.sub bv1 bv))
                     | None, Some bv2 -> mk_bnop BvAdd bv1 (mk_bitvec (Bitvector.sub bv2 bv))
                     | _ -> { term_hash; term_sort; term_desc })
                  (*| Bnop (BvSub, bv1, bv2) -> mk_bnop BvSub bv1 (mk_bnop BvAdd bv2 (mk_bitvec bv))*)
                  | _ -> { term_hash; term_sort; term_desc })

             | BvMul ->
               if Bitvector.is_zeros bv then tm2
               else { term_hash; term_sort; term_desc }

             | BvConcat | BvCmp
             | BvUdiv | BvSdiv
             | BvUrem | BvSrem | BvSmod
             | BvShl  | BvAshr | BvLshr -> { term_hash; term_sort; term_desc })

          | Some bv, None ->
            (match b with
             | BvAnd | BvNand
             | BvOr  | BvNor
             | BvXor | BvXnor
             | BvAdd | BvMul
             | BvCmp -> mk_bnop b tm2 tm1
             | BvSub ->
               if Bitvector.is_zeros bv then mk_unop BvNeg tm2
               else
                 (match tm2.term_desc with
                  | Unop (BvNeg, bv2) -> mk_bnop BvAdd tm1 bv2
                  | Bnop (BvAdd, bv1, bv2) ->
                    (match is_bitvec bv1, is_bitvec bv2 with
                     | Some bv1, None -> mk_bnop BvSub (mk_bitvec (Bitvector.sub bv bv1)) bv2
                     | None, Some bv2 -> mk_bnop BvSub (mk_bitvec (Bitvector.sub bv bv2)) bv1
                     | _ -> { term_hash; term_sort; term_desc })
                  | Bnop (BvSub, bv1, bv2) -> mk_bnop BvSub (mk_bnop BvAdd bv2 (mk_bitvec bv)) bv1
                  | _ -> { term_hash; term_sort; term_desc })
             | BvConcat
             | BvUdiv | BvSdiv
             | BvUrem | BvSrem | BvSmod
             | BvShl  | BvAshr | BvLshr -> { term_hash; term_sort; term_desc })

          | Some bv1, Some bv2 ->
            let open Bitvector in
            match b with
            | BvConcat -> mk_bitvec (append bv1 bv2)
            | BvAnd  -> mk_bitvec (logand bv1 bv2)
            | BvNand -> mk_bitvec (lognot (logand bv1 bv2))
            | BvOr   -> mk_bitvec (logor bv1 bv2)
            | BvNor  -> mk_bitvec (lognot (logor bv1 bv2))
            | BvXor  -> mk_bitvec (logxor bv1 bv2)
            | BvXnor -> mk_bitvec (lognot (logxor bv1 bv2))
            | BvCmp  -> mk_bitvec (if equal bv1 bv2 then one else zero)
            | BvAdd  -> mk_bitvec (add bv1 bv2)
            | BvSub  -> mk_bitvec (sub bv1 bv2)
            | BvMul  -> mk_bitvec (mul bv1 bv2)
            | BvUdiv -> mk_bitvec (udiv bv1 bv2)
            | BvSdiv -> mk_bitvec (sdiv bv1 bv2)
            | BvUrem -> mk_bitvec (urem bv1 bv2)
            | BvSrem -> mk_bitvec (srem bv1 bv2)
            | BvSmod -> mk_bitvec (smod bv1 bv2)
            | BvShl  -> mk_bitvec (shift_left bv1 (Z.to_int (value_of bv2)))
            | BvAshr -> mk_bitvec (shift_right_signed bv1 (Z.to_int (value_of bv2)))
            | BvLshr -> mk_bitvec (shift_right bv1 (Z.to_int (value_of bv2))))

       | IrSort, IrSort, BlSort ->
         (match is_integer tm1, is_integer tm2 with
          | None, None ->
            (* syntactic equality *)
            (match is_integer (mk_bnop IrSub tm1 tm2) with
             | None -> { term_hash; term_sort; term_desc }
             | Some ir ->
               match b with
               | Equal -> mk_bool (Z.equal ir Z.zero)
               | Distinct -> mk_bool (not (Z.equal ir Z.zero))
               | IrLt -> mk_bool (Z.lt  ir Z.zero)
               | IrLe -> mk_bool (Z.leq ir Z.zero)
               | IrGt -> mk_bool (Z.gt  ir Z.zero)
               | IrGe -> mk_bool (Z.geq ir Z.zero))

          | Some _, None ->
            (match b with
             | Equal    -> mk_bnop Equal tm2 tm1
             | Distinct -> mk_bnop Distinct tm2 tm1
             | IrLt     -> mk_bnop IrGt tm2 tm1
             | IrLe     -> mk_bnop IrGe tm2 tm1
             | IrGt     -> mk_bnop IrLt tm2 tm1
             | IrGe     -> mk_bnop IrLe tm2 tm1)

          | None, Some ir ->
            (match b with
             | Equal ->
               (match tm1.term_desc with
                | Bnop (IrAdd,ir1,ir2) ->
                  (match is_integer ir2 with
                   | Some ir2 -> mk_bnop Equal ir1 (mk_integer (Z.sub ir ir2))
                   | None -> { term_hash; term_sort; term_desc })
                | Bnop (IrSub,ir1,ir2) ->
                  (match is_integer ir1 with
                   | Some ir1 -> mk_bnop Equal ir2 (mk_integer (Z.sub ir1 ir))
                   | None ->
                     match is_integer ir2 with
                     | Some ir2 -> mk_bnop Equal ir1 (mk_integer (Z.add ir ir2))
                     | None -> { term_hash; term_sort; term_desc })
                | _ -> { term_hash; term_sort; term_desc })

             | _ -> { term_hash; term_sort; term_desc })

          | Some ir1, Some ir2 ->
            let open Z in
            match b with
            | Equal -> mk_bool (equal ir1 ir2)
            | Distinct -> mk_bool (not (equal ir1 ir2))
            | IrLt -> mk_bool (lt  ir1 ir2)
            | IrLe -> mk_bool (leq ir1 ir2)
            | IrGt -> mk_bool (gt  ir1 ir2)
            | IrGe -> mk_bool (geq ir1 ir2))

       | IrSort, IrSort, IrSort ->
         (match is_integer tm1, is_integer tm2 with
          | None, None ->
            (match b with
             | IrAdd ->
               (match tm1.term_desc, tm2.term_desc with
                | _, Unop (IrNeg, ir2) -> mk_bnop IrSub tm1 ir2
                | Unop (IrNeg, ir1), _ -> mk_bnop IrSub tm2 ir1

                | Bnop (IrAdd, ir11, ir12), Bnop (IrAdd, ir21, ir22) -> (* (w+x) + (y+z) *)
                  (match is_integer ir11, is_integer ir12,
                         is_integer ir21, is_integer ir22 with
                  | Some ir11, None, Some ir21, None ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir12 ir22) (mk_integer (Z.add ir11 ir21))
                  | Some ir11, None, None, Some ir22 ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir12 ir21) (mk_integer (Z.add ir11 ir22))
                  | None, Some ir12, Some ir21, None ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir11 ir22) (mk_integer (Z.add ir12 ir21))
                  | None, Some ir12, None, Some ir22 ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir11 ir21) (mk_integer (Z.add ir12 ir22))
                  | Some _, None, None, None ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir12 tm2) ir11
                  | None, Some _, None, None ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir11 tm2) ir12
                  | None, None, Some _, None ->
                    mk_bnop IrAdd (mk_bnop IrAdd tm1 ir22) ir21
                  | None, None, None, Some _ ->
                    mk_bnop IrAdd (mk_bnop IrAdd tm1 ir21) ir22
                  | _ -> { term_hash; term_sort; term_desc })

                | Bnop (IrSub, ir11, ir12), Bnop (IrSub, ir21, ir22) -> (* (w-x) + (y-z) *)
                  mk_bnop IrSub (mk_bnop IrAdd ir11 ir21) (mk_bnop IrAdd ir12 ir22)

                | Bnop (IrAdd, ir11, ir12), Bnop (IrSub, ir21, ir22) -> (* (w+x) + (y-z) *)
                  (match is_integer ir11, is_integer ir12,
                         is_integer ir21, is_integer ir22 with
                  | Some ir11, None, Some ir21, None ->
                    mk_bnop IrAdd (mk_bnop IrSub ir12 ir22) (mk_integer (Z.add ir11 ir21))
                  | Some ir11, None, None, Some ir22 ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir12 ir21) (mk_integer (Z.sub ir11 ir22))
                  | None, Some ir12, Some ir21, None ->
                    mk_bnop IrAdd (mk_bnop IrSub ir11 ir22) (mk_integer (Z.add ir12 ir21))
                  | None, Some ir12, None, Some ir22 ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir11 ir21) (mk_integer (Z.sub ir12 ir22))
                  | Some _, None, None, None ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir12 tm2) ir11
                  | None, Some _, None, None ->
                    mk_bnop IrAdd (mk_bnop IrAdd ir11 tm2) ir12
                  | None, None, Some _, None ->
                    mk_bnop IrAdd (mk_bnop IrSub tm1 ir22) ir21
                  | None, None, None, Some _ ->
                    mk_bnop IrSub (mk_bnop IrAdd tm1 ir21) ir22
                  | _ -> { term_hash; term_sort; term_desc })

                | Bnop (IrSub, _, _), Bnop (IrAdd, _, _) -> (* (w-x) + (y+z) *)
                  mk_bnop IrAdd tm2 tm1

                | _, _ -> { term_hash; term_sort; term_desc })

             | IrSub ->
               if equal_term tm1 tm2 then mk_ir_zero
               else
                 (match tm1.term_desc, tm2.term_desc with
                  | _, Unop (IrNeg, ir2) -> mk_bnop IrAdd tm1 ir2

                  | _, Bnop (IrSub, ir21, ir22) -> (* x - (y-z) *)
                    mk_bnop IrAdd tm1 (mk_bnop IrSub ir22 ir21)

                  | Bnop (IrAdd, ir11, ir12), Bnop (IrAdd, ir21, ir22) -> (* (w+x) - (y+z) *)
                    (match is_integer ir11, is_integer ir12,
                           is_integer ir21, is_integer ir22 with
                    | Some ir11, None, Some ir21, None ->
                      mk_bnop IrAdd (mk_bnop IrSub ir12 ir22) (mk_integer (Z.sub ir11 ir21))
                    | Some ir11, None, None, Some ir22 ->
                      mk_bnop IrAdd (mk_bnop IrSub ir21 ir12) (mk_integer (Z.sub ir11 ir22))
                    | None, Some ir12, Some ir21, None ->
                      mk_bnop IrAdd (mk_bnop IrSub ir11 ir22) (mk_integer (Z.sub ir21 ir12))
                    | None, Some ir12, None, Some ir22 ->
                      mk_bnop IrAdd (mk_bnop IrSub ir11 ir21) (mk_integer (Z.sub ir12 ir22))
                    | Some _, None, None, None ->
                      mk_bnop IrAdd (mk_bnop IrSub ir12 tm2) ir11
                    | None, Some _, None, None ->
                      mk_bnop IrAdd (mk_bnop IrSub ir11 tm2) ir12
                    | None, None, Some _, None ->
                      mk_bnop IrSub (mk_bnop IrSub tm1 ir22) ir21
                    | None, None, None, Some _ ->
                      mk_bnop IrSub (mk_bnop IrSub tm1 ir21) ir22
                    | _ -> { term_hash; term_sort; term_desc })

                  | Bnop (IrSub, ir11, ir12), Bnop (IrAdd, ir21, ir22) ->
                    (match is_integer ir11, is_integer ir12,
                           is_integer ir21, is_integer ir22 with
                    | Some ir11, None, Some ir21, None ->
                      mk_bnop IrSub (mk_integer (Z.sub ir11 ir21)) (mk_bnop IrAdd ir12 ir22)
                    | Some ir11, None, None, Some ir22 ->
                      mk_bnop IrSub (mk_integer (Z.sub ir11 ir22)) (mk_bnop IrAdd ir12 ir21)
                    | None, Some ir12, Some ir21, None ->
                      mk_bnop IrSub (mk_bnop IrSub ir11 ir22) (mk_integer (Z.add ir12 ir21))
                    | None, Some ir12, None, Some ir22 ->
                      mk_bnop IrSub (mk_bnop IrSub ir11 ir21) (mk_integer (Z.add ir12 ir22))
                    | Some _, None, None, None ->
                      mk_bnop IrSub ir11 (mk_bnop IrAdd tm2 ir12)
                    | None, Some _, None, None ->
                      mk_bnop IrSub (mk_bnop IrSub ir11 tm2) ir12
                    | None, None, Some _, None ->
                      mk_bnop IrSub (mk_bnop IrSub tm1 ir22) ir21
                    | None, None, None, Some _ ->
                      mk_bnop IrSub (mk_bnop IrSub tm1 ir21) ir22
                    | _, _, _, _ -> { term_hash; term_sort; term_desc })

                  | Unop (IrNeg, ir), Bnop (IrAdd, ir1, ir2) -> (* -x - (y+z) *)
                    (match is_integer ir1, is_integer ir2 with
                     | Some ir1, None ->
                       mk_bnop IrSub (mk_integer (Z.neg ir1)) (mk_bnop IrAdd ir ir2)
                     | None, Some ir2 ->
                       mk_bnop IrSub (mk_integer (Z.neg ir2)) (mk_bnop IrAdd ir ir1)
                     | _, _ -> { term_hash; term_sort; term_desc })

                  | _, _ -> { term_hash; term_sort; term_desc })

             | _ -> { term_hash; term_sort; term_desc })

          | None, Some ir ->
            (match b with
             | IrAdd ->
               if Z.equal ir Z.zero then tm1
               else if Z.lt ir Z.zero then
                 mk_bnop IrSub tm1 (mk_integer (Z.neg ir))
               else
                 (match tm1.term_desc with
                  | Unop (IrNeg, ir1) -> mk_bnop IrSub tm2 ir1
                  | Bnop (IrAdd, ir1, ir2) ->
                    (match is_integer ir1, is_integer ir2 with
                     | Some ir1, None -> mk_bnop IrAdd ir2 (mk_integer (Z.add ir ir1))
                     | None, Some ir2 -> mk_bnop IrAdd ir1 (mk_integer (Z.add ir ir2))
                     | _ -> { term_hash; term_sort; term_desc })
                  | Bnop (IrSub, ir1, ir2) ->
                    (match is_integer ir1, is_integer ir2 with
                     | Some ir1, None -> mk_bnop IrSub (mk_integer (Z.add ir ir1)) ir2
                     | None, Some ir2 -> mk_bnop IrAdd ir1 (mk_integer (Z.sub ir ir2))
                     | _ -> { term_hash; term_sort; term_desc })
                  | _ -> { term_hash; term_sort; term_desc })

             | IrSub ->
               if Z.equal ir Z.zero then tm1
               else if Z.lt ir Z.zero then
                 mk_bnop IrAdd tm1 (mk_integer (Z.neg ir))
               else
                 (match tm1.term_desc with
                  | Unop (IrNeg, ir1) -> mk_bnop IrSub (mk_integer (Z.neg ir)) ir1
                  | Bnop (IrAdd, ir1, ir2) ->
                    (match is_integer ir1, is_integer ir2 with
                     | Some ir1, None -> mk_bnop IrAdd ir2 (mk_integer (Z.sub ir1 ir))
                     | None, Some ir2 -> mk_bnop IrAdd ir1 (mk_integer (Z.sub ir2 ir))
                     | _ -> { term_hash; term_sort; term_desc })
                  | Bnop (IrSub, ir1, ir2) -> mk_bnop IrSub ir1 (mk_bnop IrAdd ir2 (mk_integer ir))
                  | _ -> { term_hash; term_sort; term_desc })

             | IrMul ->
               if Z.equal ir Z.zero then tm2
               else { term_hash; term_sort; term_desc }

             | IrDiv ->
               if Z.equal ir Z.one then tm1
               else { term_hash; term_sort; term_desc }

             | _ -> { term_hash; term_sort; term_desc })

          | Some ir, None ->
            (match b with
             | IrAdd | IrMul -> mk_bnop b tm2 tm1
             | IrSub ->
               if Z.equal ir Z.zero then mk_unop IrNeg tm2
               else
                 (match tm2.term_desc with
                  | Unop (IrNeg, ir2) -> mk_bnop IrAdd tm1 ir2
                  | Bnop (IrAdd, ir1, ir2) ->
                    (match is_integer ir1, is_integer ir2 with
                     | Some ir1, None -> mk_bnop IrSub (mk_integer (Z.sub ir ir1)) ir2
                     | None, Some ir2 -> mk_bnop IrSub (mk_integer (Z.sub ir ir2)) ir1
                     | _ -> { term_hash; term_sort; term_desc })
                  | Bnop (IrSub, ir1, ir2) -> mk_bnop IrSub (mk_bnop IrAdd ir2 (mk_integer ir)) ir1
                  | _ -> { term_hash; term_sort; term_desc })
             | _ -> { term_hash; term_sort; term_desc })

          | Some ir1, Some ir2 ->
            let open Z in
            match b with
            | IrAdd -> mk_integer (add ir1 ir2)
            | IrSub -> mk_integer (sub ir1 ir2)
            | IrMul -> mk_integer (mul ir1 ir2)
            | IrDiv -> mk_integer (div ir1 ir2)
            | IrMod -> mk_integer (rem ir1 ir2))

       | RlSort, RlSort, BlSort ->
         (match is_real tm1, is_real tm2 with
          | None, None | Some _, None | None, Some _ -> { term_hash; term_sort; term_desc }
          | Some rl1, Some rl2 ->
            let open Q in
            match b with
            | Equal -> mk_bool (equal rl1 rl2)
            | Distinct -> mk_bool (not (equal rl1 rl2))
            | RlLt -> mk_bool (lt  rl1 rl2)
            | RlLe -> mk_bool (leq rl1 rl2)
            | RlGt -> mk_bool (gt  rl1 rl2)
            | RlGe -> mk_bool (geq rl1 rl2))

       | RlSort, RlSort, RlSort ->
         (match is_real tm1, is_real tm2 with
          | None, None | Some _, None | None, Some _ -> { term_hash; term_sort; term_desc }
          | Some rl1, Some rl2 ->
            let open Q in
            match b with
            | RlAdd -> mk_real (add rl1 rl2)
            | RlSub -> mk_real (sub rl1 rl2)
            | RlMul -> mk_real (mul rl1 rl2)
            | RlDiv -> mk_real (div rl1 rl2))

       | AxSort _, AxSort _, BlSort ->
         (* syntactic equality *)
         (match b with
          | Equal ->
            if equal_term tm1 tm2 then mk_bl_true
            else { term_hash; term_sort; term_desc }
          | Distinct ->
            if equal_term tm1 tm2 then mk_bl_false
            else { term_hash; term_sort; term_desc })

       | _ -> { term_hash; term_sort; term_desc })

    | Ite (bl,({ term_sort; _ } as tm1),tm2) ->
      assert (equal_sort tm1.term_sort tm2.term_sort);
      (match is_bool bl with
       | Some bl -> if bl then tm1 else tm2
       | None ->
         if equal_term tm1 tm2 then tm1
         else
           let term_hash = Hashtbl.hash (bl.term_hash,tm1.term_hash,tm2.term_hash) in
           match term_sort.sort_desc with
           | BvSort 1 ->
             (match is_bitvec tm1, is_bitvec tm2 with
              | None, None | None, Some _ | Some _, None -> { term_hash; term_sort; term_desc }
              | Some b1, Some b2 ->
                if Bitvector.is_zero b1 && Bitvector.is_one b2
                then term (Ite (mk_unop BlNot bl, tm1, tm2))
                else if Bitvector.is_one b1 && Bitvector.is_zero b2
                then
                  match bl.term_desc with
                  | Bnop (Equal, tm1, tm2) ->
                    (match tm1.term_sort.sort_desc with
                     | BvSort _ -> mk_bnop BvCmp tm1 tm2
                     | _ -> { term_hash; term_sort; term_desc })
                  | Bnop (Distinct, tm1, tm2) ->
                    (match tm1.term_sort.sort_desc with
                     | BvSort _ -> mk_unop BvNot (mk_bnop BvCmp tm1 tm2)
                     | _ -> { term_hash; term_sort; term_desc })
                  | _ -> { term_hash; term_sort; term_desc }
                else { term_hash; term_sort; term_desc })


           | _ -> { term_hash; term_sort; term_desc })

    | Select (ax,tm) ->
      let AxSort (idx_sort,term_sort) = ax.term_sort.sort_desc in
      assert (equal_sort idx_sort tm.term_sort);
      let term_hash = Hashtbl.hash (ax.term_hash,tm.term_hash) in
      { term_hash; term_sort; term_desc }

    | Store (({ term_sort; _ } as ax),tm1,tm2) ->
      let AxSort (idx_sort,elt_sort) = term_sort.sort_desc in
      assert (equal_sort idx_sort tm1.term_sort && equal_sort elt_sort tm2.term_sort);
      let term_hash = Hashtbl.hash (ax.term_hash,tm1.term_hash,tm2.term_hash) in
      { term_hash; term_sort; term_desc }

and mk_fun : type a. a var -> any_term list -> a term =
  fun v ls -> term (Fun (v, ls))

and mk_bind : type a. bind -> a term -> a term =
  fun bn tm -> term (Bind (bn, tm))

and mk_unop : type a b. (a,b) unop -> a term -> b term =
  fun u tm -> term (Unop (u, tm))

and mk_bnop : type a b c. (a,b,c) bnop -> a term -> b term -> c term =
  fun b tm1 tm2 -> term (Bnop (b, tm1, tm2))

and mk_ite : type a. bl term -> a term -> a term -> a term =
  fun bl tm1 tm2 -> term (Ite (bl, tm1, tm2))

and mk_select : type a b. (a,b) ax term -> a term -> b term =
  fun ax tm -> term (Select (ax, tm))

and mk_store : type a b. (a,b) ax term -> a term -> b term -> (a,b) ax term =
  fun ax tm1 tm2 -> term (Store (ax, tm1, tm2))

let any_term term = AnyTerm term

let bind bind_desc =
  match bind_desc with
  | Let ls ->
    let bind_hash = Hashtbl.hash (list_hash (fun df -> df.def_hash) ls) in
    { bind_hash; bind_desc }
  | Exists ls ->
    let bind_hash = Hashtbl.hash (true, list_hash (fun dc -> dc.decl_hash) ls) in
    { bind_hash; bind_desc }
  | Forall ls ->
    let bind_hash = Hashtbl.hash (false, list_hash (fun dc -> dc.decl_hash) ls) in
    { bind_hash; bind_desc }

let mk_let ls = bind (Let ls)
let mk_exists ls = bind (Exists ls)
let mk_forall ls = bind (Forall ls)

let def (Def (v,ls,tm) as def_desc) =
  let def_hash =
    Hashtbl.hash (v.var_hash, list_hash (fun (AnyVar v) -> v.var_hash) ls, tm.term_hash)
  in { def_hash; def_desc }

let mk_def v ls tm = def (Def (v, ls, tm))

let decl (Decl (v,ls) as decl_desc) =
  let decl_hash =
    Hashtbl.hash (v.var_hash, list_hash (fun (AnySort s) -> s.sort_hash) ls)
  in { decl_hash; decl_desc }

let mk_decl v ls = decl (Decl (v, ls))

let entry_desc_hash = function
  | Declare dc -> Hashtbl.hash dc.decl_hash
  | Define  df -> Hashtbl.hash df.def_hash
  | Assert  bl -> Hashtbl.hash bl.term_hash
  | Comment s  -> Hashtbl.hash s

let entry entry_desc =
  let entry_hash = entry_desc_hash entry_desc in
  { entry_hash; entry_desc }

let mk_declare dc = entry (Declare dc)
let mk_define  df = entry (Define df)
let mk_assert  bl = entry (Assert bl)
let mk_comment s  = entry (Comment s)

(* Some helpers *)

let mk_bl_not bl = mk_unop BlNot bl

let mk_bv_not bv = mk_unop BvNot bv
let mk_bv_neg bv = mk_unop BvNeg bv

let mk_ir_neg bv = mk_unop IrNeg bv
let mk_ir_abs bv = mk_unop IrAbs bv

let mk_rl_neg bv = mk_unop RlNeg bv

let mk_bv_repeat       i bv = mk_unop (BvRepeat i) bv
let mk_bv_zero_extend  i bv = mk_unop (BvZeroExtend i) bv
let mk_bv_sign_extend  i bv = mk_unop (BvSignExtend i) bv
let mk_bv_rotate_left  i bv = mk_unop (BvRotateLeft i) bv
let mk_bv_rotate_right i bv = mk_unop (BvRotateRight i) bv
let mk_bv_extract      i bv = mk_unop (BvExtract i) bv

let mk_equal    tm1 tm2 = mk_bnop Equal    tm1 tm2
let mk_distinct tm1 tm2 = mk_bnop Distinct tm1 tm2

let mk_bl_imply bl1 bl2 = mk_bnop BlImply bl1 bl2
let mk_bl_and   bl1 bl2 = mk_bnop BlAnd bl1 bl2
let mk_bl_or    bl1 bl2 = mk_bnop BlOr  bl1 bl2
let mk_bl_xor   bl1 bl2 = mk_bnop BlXor bl1 bl2

let mk_bv_concat bv1 bv2 = mk_bnop BvConcat bv1 bv2
let mk_bv_and    bv1 bv2 = mk_bnop BvAnd  bv1 bv2
let mk_bv_nand   bv1 bv2 = mk_bnop BvNand bv1 bv2
let mk_bv_or     bv1 bv2 = mk_bnop BvOr   bv1 bv2
let mk_bv_nor    bv1 bv2 = mk_bnop BvNor  bv1 bv2
let mk_bv_xor    bv1 bv2 = mk_bnop BvXor  bv1 bv2
let mk_bv_xnor   bv1 bv2 = mk_bnop BvXnor bv1 bv2
let mk_bv_cmp    bv1 bv2 = mk_bnop BvCmp  bv1 bv2
let mk_bv_add    bv1 bv2 = mk_bnop BvAdd  bv1 bv2
let mk_bv_sub    bv1 bv2 = mk_bnop BvSub  bv1 bv2
let mk_bv_mul    bv1 bv2 = mk_bnop BvMul  bv1 bv2
let mk_bv_udiv   bv1 bv2 = mk_bnop BvUdiv bv1 bv2
let mk_bv_sdiv   bv1 bv2 = mk_bnop BvSdiv bv1 bv2
let mk_bv_urem   bv1 bv2 = mk_bnop BvUrem bv1 bv2
let mk_bv_srem   bv1 bv2 = mk_bnop BvSrem bv1 bv2
let mk_bv_smod   bv1 bv2 = mk_bnop BvSmod bv1 bv2
let mk_bv_shl    bv1 bv2 = mk_bnop BvShl  bv1 bv2
let mk_bv_ashr   bv1 bv2 = mk_bnop BvAshr bv1 bv2
let mk_bv_lshr   bv1 bv2 = mk_bnop BvLshr bv1 bv2

let mk_ir_add    ir1 ir2 = mk_bnop IrAdd  ir1 ir2
let mk_ir_sub    ir1 ir2 = mk_bnop IrSub  ir1 ir2
let mk_ir_mul    ir1 ir2 = mk_bnop IrMul  ir1 ir2
let mk_ir_div    ir1 ir2 = mk_bnop IrDiv  ir1 ir2
let mk_ir_mod    ir1 ir2 = mk_bnop IrMod  ir1 ir2

let mk_rl_add    rl1 rl2 = mk_bnop RlAdd  rl1 rl2
let mk_rl_sub    rl1 rl2 = mk_bnop RlSub  rl1 rl2
let mk_rl_mul    rl1 rl2 = mk_bnop RlMul  rl1 rl2
let mk_rl_div    rl1 rl2 = mk_bnop RlDiv  rl1 rl2

let mk_bl_equal    bl1 bl2 = mk_bnop Equal    bl1 bl2
let mk_bl_distinct bl1 bl2 = mk_bnop Distinct bl1 bl2
let mk_bv_equal    bv1 bv2 = mk_bnop Equal    bv1 bv2
let mk_bv_distinct bv1 bv2 = mk_bnop Distinct bv1 bv2
let mk_ir_equal    ir1 ir2 = mk_bnop Equal    ir1 ir2
let mk_ir_distinct ir1 ir2 = mk_bnop Distinct ir1 ir2
let mk_rl_equal    rl1 rl2 = mk_bnop Equal    rl1 rl2
let mk_rl_distinct rl1 rl2 = mk_bnop Distinct rl1 rl2
let mk_ax_equal    ax1 ax2 = mk_bnop Equal    ax1 ax2
let mk_ax_distinct ax1 ax2 = mk_bnop Distinct ax1 ax2

let mk_bv_ult bv1 bv2 = mk_bnop BvUlt bv1 bv2
let mk_bv_ule bv1 bv2 = mk_bnop BvUle bv1 bv2
let mk_bv_ugt bv1 bv2 = mk_bnop BvUgt bv1 bv2
let mk_bv_uge bv1 bv2 = mk_bnop BvUge bv1 bv2
let mk_bv_slt bv1 bv2 = mk_bnop BvSlt bv1 bv2
let mk_bv_sle bv1 bv2 = mk_bnop BvSle bv1 bv2
let mk_bv_sgt bv1 bv2 = mk_bnop BvSgt bv1 bv2
let mk_bv_sge bv1 bv2 = mk_bnop BvSge bv1 bv2

let mk_ir_lt  ir1 ir2 = mk_bnop IrLt ir1 ir2
let mk_ir_le  ir1 ir2 = mk_bnop IrLe ir1 ir2
let mk_ir_gt  ir1 ir2 = mk_bnop IrGt ir1 ir2
let mk_ir_ge  ir1 ir2 = mk_bnop IrGe ir1 ir2

let mk_rl_lt  rl1 rl2 = mk_bnop RlLt rl1 rl2
let mk_rl_le  rl1 rl2 = mk_bnop RlLe rl1 rl2
let mk_rl_gt  rl1 rl2 = mk_bnop RlGt rl1 rl2
let mk_rl_ge  rl1 rl2 = mk_bnop RlGe rl1 rl2

let mk_bv_add_int bv i =
  mk_bv_add bv
    (mk_bitvec (Bitvector.create (Z.of_int i) (bv_term_size bv)))

let mk_bv_sub_int bv i =
  mk_bv_sub bv
    (mk_bitvec (Bitvector.create (Z.of_int i) (bv_term_size bv)))

(* Sequence reification *)

let empty = { logic = ""; entries = Sequence.empty }
let length fm = Sequence.length fm.entries
let append fm1 fm2 = {
  logic = if String.compare fm1.logic fm2.logic = 0 then fm1.logic else "";
  entries = Sequence.append fm1.entries fm2.entries;
}

let set_logic logic fm = { fm with logic }

let push_front en fm = { fm with entries = Sequence.push_front en fm.entries }
let push_back  en fm = { fm with entries = Sequence.push_back  en fm.entries }

let push_front_declare dc fm = push_front (mk_declare dc) fm
let push_front_define  df fm = push_front (mk_define  df) fm
let push_front_assert  bl fm = push_front (mk_assert  bl) fm
let push_front_comment s  fm = push_front (mk_comment s)  fm

let push_back_declare dc fm = push_back (mk_declare dc) fm
let push_back_define  df fm = push_back (mk_define  df) fm
let push_back_assert  bl fm = push_back (mk_assert  bl) fm
let push_back_comment s  fm = push_back (mk_comment s)  fm

let peek_front fm = Sequence.peek_front fm.entries
let peek_back  fm = Sequence.peek_back  fm.entries

let pop_front fm =
  match Sequence.pop_front fm.entries with
  | Some entries -> Some { fm with entries }
  | None -> None
let pop_back fm =
  match Sequence.pop_back fm.entries with
  | Some entries -> Some { fm with entries }
  | None -> None

let map_forward  f fm = { fm with entries = Sequence.map_forward  f fm.entries }
let map_backward f fm = { fm with entries = Sequence.map_backward f fm.entries }

let iter_forward  f fm = Sequence.iter_forward  f fm.entries
let iter_backward f fm = Sequence.iter_backward f fm.entries

let fold_forward  f fm acc = Sequence.fold_forward  f fm.entries acc
let fold_backward f fm acc = Sequence.fold_backward f fm.entries acc

module VarHashtbl = Hashtbl.Make
    (struct
      type t = any_var
      let hash (AnyVar t) = t.var_hash
      let equal t1 t2 = t1 = t2
    end)

module VarMap = Map.Make
    (struct
      type t = any_var
      let compare t1 t2 = compare t1 t2
    end)

module VarSet = Set.Make
    (struct
      type t = any_var
      let compare t1 t2 = compare t1 t2
    end)

module TermHashtbl = Hashtbl.Make
    (struct
      type t = any_term
      let hash (AnyTerm t) = t.term_hash
      let equal t1 t2 = t1 = t2
    end)

module TermMap = Map.Make
    (struct
      type t = any_term
      let compare t1 t2 = compare t1 t2
    end)

module TermSet = Set.Make
    (struct
      type t = any_term
      let compare t1 t2 = compare t1 t2
    end)


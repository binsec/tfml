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

module Internal :
sig
  type t

  val create : Z.t -> int -> t

  val value_of  : t -> Z.t
  val signed_of : t -> Z.t
  val size_of   : t -> int

  val compare : t -> t -> int
end =
struct
  type t =
    { value: Z.t;
      size: int;
      ulimit: Z.t;
      slimit: Z.t }

  let create value size =
    if size <= 0 then invalid_arg "Negative bitvector size";
    let ulimit = Z.shift_left Z.one size in
    let slimit = Z.ediv ulimit (Z.of_int 2) in
    let value  = Z.erem value ulimit in
    { value; size; ulimit; slimit }

  let value_of bv = bv.value

  let signed_of bv =
    if Z.lt bv.value bv.slimit then bv.value
    else Z.sub bv.value bv.ulimit

  let size_of bv = bv.size

  let compare t1 t2 =
    let cmp = compare t1.size t2.size in
    if cmp = 0
    then Z.compare t1.value t2.value
    else cmp
end

include Internal

let create value size = create value size
let create_from_tuple (value, size) = create value size

let resize bv size  = create (value_of bv) size
let update bv value = create value (size_of bv)

let equal bv1 bv2 =
  size_of bv1 = size_of bv2 &&
  Z.equal (value_of bv1) (value_of bv2)

let diff bv1 bv2 = not (equal bv1 bv2)

let zero = create Z.zero 1
let one  = create Z.one 1

let zeros size = create Z.zero size
let ones  size = create Z.one size

let fill ?lo ?hi size =
  let lo = match lo with None -> 0 | Some l -> l in
  let hi = match hi with None -> size-1 | Some h -> h in
  if lo < 0 || hi >= size || hi < lo then invalid_arg "Invalid bitvector size";
  create
    (Z.shift_left (Z.sub (Z.shift_left Z.one (hi - lo + 1)) Z.one) lo)
    size

let max_ubv n =
  if n <= 0 then invalid_arg "Invalid bitvector size";
  create (Z.sub (Z.shift_left Z.one n) Z.one) n

let max_sbv n =
  if n <= 0 then invalid_arg "Invalid bitvector size";
  create (Z.sub (Z.shift_left Z.one (n-1)) Z.one) n

let is_zero bv = equal bv zero
let is_one  bv = equal bv one

let is_zeros bv = equal bv (zeros (size_of bv))
let is_ones  bv = equal bv (ones  (size_of bv))
let is_fill  bv = equal bv (fill  (size_of bv))

let is_max_ubv bv = equal bv (max_ubv (size_of bv))
let is_max_sbv bv = equal bv (max_sbv (size_of bv))


(* Utils *)

let pp ppf bv =
  Format.fprintf ppf "{%s; %i}" (Z.to_string (value_of bv)) (size_of bv)

let print bv =
  Printf.sprintf "{%s; %i}" (Z.to_string (value_of bv)) (size_of bv)

let binop_error bv1 bv2 msg =
  Printf.sprintf "%s %s %s" msg (print bv1) (print bv2)

let bvint_error bv i msg =
  Printf.sprintf "%s %s %i" msg (print bv) i

let operands_size_conflict msg = failwith (Printf.sprintf "Operands size conflict in '%s'" msg)
let bad_bound msg = failwith (Printf.sprintf "Bad bound in '%s'" msg)

let unsigned_compare (f: Z.t -> Z.t -> bool) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2
  then operands_size_conflict (binop_error bv1 bv2 msg)
  else f (value_of bv1) (value_of bv2)

let signed_compare (f: Z.t -> Z.t -> bool) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2
  then operands_size_conflict (binop_error bv1 bv2 msg)
  else f (signed_of bv1) (signed_of bv2)

let unsigned_apply (f: Z.t -> Z.t -> Z.t) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2
  then operands_size_conflict (binop_error bv1 bv2 msg)
  else update bv1 (f (value_of bv1) (value_of bv2))

let signed_apply (f: Z.t -> Z.t -> Z.t) bv1 bv2 msg =
  if size_of bv1 <> size_of bv2
  then operands_size_conflict (binop_error bv1 bv2 msg)
  else update bv1 (f (signed_of bv1) (signed_of bv2))


(* Comparison *)

let ule bv1 bv2 = unsigned_compare Z.leq bv1 bv2 "ule"
let uge bv1 bv2 = unsigned_compare Z.geq bv1 bv2 "uge"
let ult bv1 bv2 = unsigned_compare Z.lt  bv1 bv2 "ult"
let ugt bv1 bv2 = unsigned_compare Z.gt  bv1 bv2 "ugt"

let sle bv1 bv2 = signed_compare Z.leq bv1 bv2 "sle"
let sge bv1 bv2 = signed_compare Z.geq bv1 bv2 "sge"
let slt bv1 bv2 = signed_compare Z.lt  bv1 bv2 "slt"
let sgt bv1 bv2 = signed_compare Z.gt  bv1 bv2 "sgt"


(* Arithmetic *)

let succ bv = create (Z.succ (value_of bv)) (size_of bv)
let pred bv = create (Z.pred (value_of bv)) (size_of bv)

let add bv1 bv2 = unsigned_apply Z.add bv1 bv2 "add"
let sub bv1 bv2 = unsigned_apply Z.sub bv1 bv2 "sub"
let mul bv1 bv2 = unsigned_apply Z.mul bv1 bv2 "mul"

let udiv bv1 bv2 = unsigned_apply Z.div bv1 bv2 "udiv"
let umod bv1 bv2 = unsigned_apply Z.rem bv1 bv2 "umod"
let urem bv1 bv2 = unsigned_apply Z.rem bv1 bv2 "urem"

let pow bv1 bv2 = unsigned_apply (fun a b -> Z.pow a (Z.to_int b)) bv1 bv2 "pow"

let umax bv1 bv2 = if uge bv1 bv2 then bv1 else bv2
let umin bv1 bv2 = if ule bv1 bv2 then bv1 else bv2

let sdiv bv1 bv2 = signed_apply Z.div  bv1 bv2 "sdiv"
let smod bv1 bv2 = signed_apply Z.erem bv1 bv2 "smod"
let srem bv1 bv2 = signed_apply Z.rem  bv1 bv2 "srem"

let neg bv = update bv (Z.neg (signed_of bv))

let smax bv1 bv2 = if sge bv1 bv2 then bv1 else bv2
let smin bv1 bv2 = if sle bv1 bv2 then bv1 else bv2

let is_neg bv = Z.lt (signed_of bv) Z.zero


(* Logical *)

let logand bv1 bv2 = unsigned_apply Z.logand bv1 bv2 "logand"
let logor  bv1 bv2 = unsigned_apply Z.logor  bv1 bv2 "logor"
let logxor bv1 bv2 = unsigned_apply Z.logxor bv1 bv2 "logxor"
let lognot bv = update bv (Z.lognot (value_of bv))

let shift_left  bv i = update bv (Z.shift_left  (value_of bv) i)
let shift_right bv i = update bv (Z.shift_right (value_of bv) i)
let shift_right_signed bv i = update bv (Z.shift_right_trunc (signed_of bv) i)

let rotate_left bv i =
  update bv
    (Z.logor
       (Z.shift_left  (value_of bv) i)
       (Z.shift_right (value_of bv) (size_of bv - i)))

let rotate_right bv i =
  update bv
    (Z.logor
       (Z.shift_right (value_of bv) i)
       (Z.shift_left  (value_of bv) (size_of bv - i)))

let reduce bv i =
  if size_of bv < i
  then bad_bound (bvint_error bv i "reduce")
  else resize bv i

let extend bv i =
  if size_of bv > i
  then bad_bound (bvint_error bv i "extend")
  else resize bv i

let extend_signed bv i =
  if size_of bv > i
  then bad_bound (bvint_error bv i "extend_signed")
  else create (signed_of bv) i

let extend_unsafe bv i = resize bv i

let bit_mask i = Z.shift_left Z.one i
let bit_mask_not i = Z.logxor Z.minus_one (bit_mask i)

let get_bit bv i = Z.testbit (value_of bv) i

let set_bit bv i = update bv (Z.logor (bit_mask i) (value_of bv))

let clear_bit bv i = update bv (Z.logand (bit_mask_not i) (value_of bv))

let flip_bit bv i = if get_bit bv i then clear_bit bv i else set_bit bv i

let append bv1 bv2 =
  create
    (Z.logor
       (Z.shift_left (value_of bv1) (size_of bv2))
       (value_of bv2))
    (size_of bv1 + size_of bv2)

let concat = function
  | [] -> failwith "concat"
  | bv :: lst -> List.fold_left append bv lst

let extract bv {Interval.lo; Interval.hi} =
  if (lo < 0) || (hi >= size_of bv) || (hi < lo)
  then bad_bound (Printf.sprintf "restrict %s [%i..%i]" (print bv) lo hi)
  else
    let size = hi - lo + 1 in
    create (Z.extract (value_of bv) lo size) size


(* Conversion *)

let of_string str =
  let len = String.length str in
  if len < 3 then failwith "Bitvector.of_string : too short string" else
    let size =
      match str.[0], str.[1], str.[2] with
      | '0', 'x', _ -> (len - 2) * 4
      | '0', 'b', _ -> len - 2
      | '+', '0', 'x'
      | '-', '0', 'x' -> (len - 3) * 4
      | '+', '0', 'b'
      | '-', '0', 'b' -> len - 3
      | _ -> failwith "Bitvector.of_string : should start with [+-]?0[xb]"
    in
    try create (Z.of_string str) size
    with Failure _ -> failwith ("Bitvector.of_string : " ^ str)

let of_hexstring = of_string
let to_hexstring bv : string = Z.format "0x%x" (value_of bv)

let of_bool b = if b then one else zero
let to_bool bv = not (is_zero bv)

let of_int32 i32 = create (Z.of_int32 i32) 32
let to_int32 bv  = Z.to_int32 (signed_of bv)

let of_int64 i64 = create (Z.of_int64 i64) 64
let to_int64 bv  = Z.to_int64 (signed_of bv)

let pp_hex ppf bv = Format.fprintf ppf "{%s; %i}" (to_hexstring bv) (size_of bv)


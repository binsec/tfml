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

(** Generic signatures used throughout BINSEC *)

module type Any = sig
  type t
end

module type Printable = sig
  type t
  val pp : Format.formatter -> t -> unit
end

module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
end

module type Eq = sig
  type t
  val equal : t -> t -> bool
end

module type Collection = sig
  include COMPARABLE
  module Map: sig
    include Map.S with type key = t
    val pop : 'a t -> (key * 'a) * 'a t
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
  end
  module Set: sig
    include Set.S with type elt = t
    val pop : t -> elt * t
  end
end

module type Comparisons = sig
  type t
  val equal : t -> t -> bool
  val diff  : t -> t -> bool

  val ule : t -> t -> bool
  val uge : t -> t -> bool
  val ult : t -> t -> bool
  val ugt : t -> t -> bool

  val sle : t -> t -> bool
  val sge : t -> t -> bool
  val slt : t -> t -> bool
  val sgt : t -> t -> bool
end


module type Arithmetic = sig
  type t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val neg  : t -> t

  (* Unsigned operations *)
  val udiv : t -> t -> t
  val umod : t -> t -> t
  val urem : t -> t -> t
  val pow  : t -> t -> t

  (* Signed operations *)
  val sdiv : t -> t -> t
  val smod : t -> t -> t
  val srem : t -> t -> t
end


module type Logical = sig
  type t
  val logand : t -> t -> t
  val logor  : t -> t -> t
  val lognot : t -> t
end

module type EXTENDED_LOGICAL = sig
  include Logical
  val logxor : t -> t -> t
end

module type SHIFT_ROT = sig
  type t
  val shift_left  : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_signed : t -> int -> t

  val rotate_left  : t -> int -> t
  val rotate_right : t -> int -> t
end

module type Bitwise = sig
  include EXTENDED_LOGICAL
  include SHIFT_ROT with type t := t
end

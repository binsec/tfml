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

type 'a t = { lo : 'a; hi : 'a }

val belongs    : ('a -> 'a -> int) -> 'a -> 'a t -> bool
val intersects : ('a -> 'a -> int) -> 'a t -> 'a t -> bool

module type S =
sig
  type point
  type interval
  type t

  val empty : t
  val singleton : interval -> t
  val add    : interval -> t -> t
  val remove : interval -> t -> t

  val is_empty : t -> bool
  val mem : interval -> t -> bool

  val belongs    : point    -> t -> interval list
  val intersects : interval -> t -> interval list
end

module Make (Ord : Sigs.COMPARABLE) : S
  with type point = Ord.t
   and type interval = Ord.t t

module Int : S
  with type point = int
   and type interval = int t

module Float : S
  with type point = float
   and type interval = float t

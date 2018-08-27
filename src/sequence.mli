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

type 'a t

val empty : 'a t
val length : 'a t -> int
val append : 'a t -> 'a t -> 'a t

val push_front : 'a -> 'a t -> 'a t
val push_back  : 'a -> 'a t -> 'a t

val peek_front : 'a t -> 'a option
val peek_back  : 'a t -> 'a option

val pop_front : 'a t -> 'a t option
val pop_back  : 'a t -> 'a t option

val map_forward  : ('a -> 'b) -> 'a t -> 'b t
val map_backward : ('a -> 'b) -> 'a t -> 'b t

val iter_forward  : ('a -> unit) -> 'a t -> unit
val iter_backward : ('a -> unit) -> 'a t -> unit

val fold_forward  : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_backward : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

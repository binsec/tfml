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

val uniquify : formula -> formula

val specialize : (decl -> bool) -> formula -> formula
val generalize : (decl -> bool) -> formula -> formula

val constant_propagation : ?keep:VarSet.t -> formula -> formula
val prune_and_inline     : ?keep:VarSet.t -> formula -> formula
val static_single_assignment : formula -> formula

val taint : VarSet.t -> formula -> formula

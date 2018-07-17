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

module IntHashtbl =
  Hashtbl.Make
    (struct
      type t = int
      let equal (n1: int) (n2: int) = n1 = n2
      let hash n = n
    end)

module IntMap = Ptmap

module IntSet = Ptset

module StringHashtbl =
  Hashtbl.Make
    (struct
      type t = string
      let equal s1 s2 = String.compare s1 s2 = 0
      let hash s = Hashtbl.hash s
    end)

module StringMap =
  Map.Make
    (struct
      type t = string
      let compare s1 s2 = String.compare s1 s2
    end)

module StringSet =
  Set.Make
    (struct
      type t = string
      let compare s1 s2 = String.compare s1 s2
    end)

let rec list_init i n f =
  if i >= n then []
  else
    let r = f i in
    r :: list_init (i+1) n f

let list_init n f = list_init 0 n f

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

let genr_bool_switch () =
  let bool = ref false in
  (fun b -> bool := b),
  (fun () -> !bool)

let genr_string_switch () =
  let string = ref "" in
  (fun b -> string := b),
  (fun () -> !string)

let add_file, get_files, clear_files  =
  let (files : string list ref) = ref ([] : string list) in
  (fun (f : string) -> files := f :: !files),
  (fun () -> List.rev !files),
  (fun () ->  files := [])

let set_logic, get_logic = genr_string_switch ()

let set_taint, get_taint = genr_bool_switch ()

let set_reprint, get_reprint = genr_bool_switch ()


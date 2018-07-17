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
open Formula_utils
open Formula_transformation

let umsg = Format.sprintf "Usage: %s [options] file+" Sys.argv.(0)

let rec argspec =
  [
    "--help", Arg.Unit print_usage,
    " print this option list";
    "-help", Arg.Unit print_usage,
    " print this option list";
    "-logic", Arg.String (fun string -> Config.set_logic string),
    " set the logic";
    "-taint", Arg.Unit (fun () -> Config.set_taint true),
    " enable formula tainting";
    "-pp", Arg.Unit (fun () -> Config.set_reprint true),
    " print the SMT-LIB AST read on stdout";
  ]

and print_usage () =
  Arg.usage (Arg.align argspec) umsg;
  exit 0

let lex_file fname =
  try
    let chan =
      match fname with
      | "-" -> stdin
      | file -> open_in file
    in
    let lexbuf = Lexing.from_channel chan in
    lexbuf.Lexing.lex_curr_p <- {
      Lexing.pos_fname = fname;
      Lexing.pos_lnum = 1;
      Lexing.pos_bol  = 0;
      Lexing.pos_cnum = 0;
    };
    (lexbuf, fun () -> close_in chan)
  with Not_found -> exit 2

let parse_file f =
  let lexbuf, close = lex_file f in
  let scr = Smtlib_parser.script Smtlib_lexer.token lexbuf in
  close ();
  scr

let apply_if bool f fm = if bool then f fm else fm

let taint fm =
  let vars = ref VarSet.empty in
  Formula_transformation.specialize
    (fun { decl_desc = Decl (v,_); _ } ->
       vars := VarSet.add (any_var v) !vars; true)
    fm
  |> Formula_transformation.uniquify
  |> Formula_transformation.static_single_assignment
  |> taint !vars
  |> Formula_transformation.prune_and_inline ~keep:VarSet.empty

let print_exn file exn =
  Printf.sprintf "%s %s" file
    (match exn with
     | Sys_error s -> s
     | exn -> Printexc.to_string exn)

let run file =
  try
    if Config.get_reprint () then
      parse_file file
      |> Smtlib_pp.pp Format.std_formatter
    else
      parse_file file
      |> Smtlib_to_formula.script
      |> Formula_transformation.prune_and_inline ~keep:VarSet.empty
      |> Formula_transformation.constant_propagation ~keep:VarSet.empty
      |> Formula_transformation.uniquify
      |> Formula_transformation.static_single_assignment
      |> Formula_transformation.prune_and_inline ~keep:VarSet.empty
      |> apply_if (Config.get_taint ()) taint
      |> Formula_transformation.prune_and_inline ~keep:VarSet.empty
      (*|> (fun fml -> Gc.print_stat stderr; fml)*)
      |> Formula.set_logic (Config.get_logic ())
      |> Formula_to_smtlib.formula
      |> Smtlib_pp.pp Format.std_formatter
  with exn -> print_endline (print_exn file exn)

let () =
  Arg.parse (Arg.align argspec) Config.add_file umsg;
  match Config.get_files () with
  | [] -> print_usage ()
  | files -> List.iter run files;
    exit 0


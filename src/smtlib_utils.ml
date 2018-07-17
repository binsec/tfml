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

open Smtlib

let string_of_symbol { symbol_desc; _ } =
  match symbol_desc with
  | SimpleSymbol s
  | QuotedSymbol s -> s


let symbol_of_identifier { id_desc; _ } =
  match id_desc with
  | IdSymbol sy
  | IdUnderscore (sy,_) -> sy

let string_of_identifier id =
  id |> symbol_of_identifier |> string_of_symbol


let identifier_of_sort { sort_desc; _ } =
  match sort_desc with
  | SortIdentifier id
  | SortFun (id,_) -> id

let symbol_of_sort st =
  st |> identifier_of_sort |> symbol_of_identifier

let string_of_sort st =
  st |> identifier_of_sort |> string_of_identifier

let symbols_of_sort sort =
  let rec aux symbols { sort_desc; _ } =
    match sort_desc with
    | SortIdentifier id -> (symbol_of_identifier id) :: symbols
    | SortFun (id, sorts) ->
      List.fold_left aux ((symbol_of_identifier id) :: symbols) sorts
  in List.rev (aux [] sort)


let symbol_of_sorted_var { sorted_var_desc; _ } =
  match sorted_var_desc with
  | SortedVar (sy, _) -> sy

let string_of_sorted_var sv =
  sv |> symbol_of_sorted_var |> string_of_symbol

let sort_of_sorted_var { sorted_var_desc; _ } =
  match sorted_var_desc with
  | SortedVar (_, so) -> so


let symbol_of_var_binding { var_binding_desc; _ } =
  match var_binding_desc with
  | VarBinding (sy, _) -> sy

let string_of_var_binding vb =
  vb |> symbol_of_var_binding |> string_of_symbol

let term_of_var_binding { var_binding_desc; _ } =
  match var_binding_desc with
  | VarBinding (_, tm) -> tm


let identifier_of_qual_identifier { qual_identifier_desc; _ } =
  match qual_identifier_desc with
  | QualIdentifierAs (id, _)
  | QualIdentifierIdentifier id -> id

let symbol_of_qual_identifier qid =
  qid |> identifier_of_qual_identifier |> symbol_of_identifier

let string_of_qual_identifier qid =
  qid |> identifier_of_qual_identifier |> string_of_identifier


let get_logic (s : Smtlib.script) =
  let rec aux (cmds : Smtlib.commands) =
    match cmds with
    | [] -> ""
    | cmd :: cmds ->
      match cmd.command_desc with
      | CmdSetLogic symb ->
        begin
          match symb.symbol_desc with
          | SimpleSymbol logic_name -> logic_name
          | QuotedSymbol _ -> assert false
        end
      | CmdAssert _
      | CmdCheckSat
      | CmdCheckSatAssuming _
      | CmdComment _
      | CmdDeclareConst (_,_)
      | CmdDeclareFun (_,_,_,_)
      | CmdDeclareSort (_,_)
      | CmdDefineFun _
      | CmdDefineFunRec _
      | CmdDefineSort (_,_,_)
      | CmdEcho _
      | CmdExit
      | CmdGetAssertions
      | CmdGetAssignment
      | CmdGetInfo _
      | CmdGetModel
      | CmdGetOption _
      | CmdGetProof
      | CmdGetUnsatAssumptions
      | CmdGetUnsatCore
      | CmdGetValue _
      | CmdMetaInfo _
      | CmdPop _
      | CmdPush _
      | CmdReset
      | CmdResetAssertions
      | CmdSetInfo _
      | CmdSetOption _ -> aux cmds
  in aux s.script_commands


let rec is_constant_term (t : Smtlib.term) =
  match t.term_desc with
  | TermSpecConstant cst -> Some cst
  | TermAnnotatedTerm (t, _) -> is_constant_term t
  | TermLetTerm _
  | TermQualIdentifier _
  | TermQualIdentifierTerms _
  | TermForallTerm _
  | TermExistsTerm _ -> None


let rec is_variable_term (t : Smtlib.term) =
  match t.term_desc with
  | TermQualIdentifier qi -> Some qi
  | TermAnnotatedTerm (t, _) -> is_variable_term t
  | TermSpecConstant _
  | TermLetTerm _
  | TermQualIdentifierTerms _
  | TermForallTerm _
  | TermExistsTerm _ -> None


let mk_symbol s =  {
  symbol_desc = SimpleSymbol s;
  symbol_loc = Locations.dummy_loc;
}

let mk_localized_symbol s symbol_loc = {
  symbol_desc = SimpleSymbol s;
  symbol_loc;
}

let mk_idx_num i =
  IdxNum (string_of_int i)

let mk_id_symbol symbol =
  let id_desc = Smtlib.IdSymbol symbol in
  let id_loc  = Locations.dummy_loc in
  { id_desc; id_loc }

let mk_id_underscore symbol indexes =
  let id_desc = Smtlib.IdUnderscore (symbol,indexes) in
  let id_loc  = Locations.dummy_loc in
  { id_desc; id_loc }

let mk_qual_identifier_identifier identifier =
  let qual_identifier_desc = Smtlib.QualIdentifierIdentifier identifier in
  let qual_identifier_loc  = Locations.dummy_loc in
  { qual_identifier_desc; qual_identifier_loc }

let mk_sorted_var symbol sort =
  let sorted_var_desc = Smtlib.SortedVar (symbol,sort) in
  let sorted_var_loc  = Locations.dummy_loc in
  { sorted_var_desc; sorted_var_loc }

let mk_var_binding symbol term =
  let var_binding_desc = Smtlib.VarBinding (symbol,term) in
  let var_binding_loc  = Locations.dummy_loc in
  { var_binding_desc; var_binding_loc }

let mk_sort_identifier symbol =
  let sort_desc = Smtlib.SortIdentifier (mk_id_symbol symbol) in
  let sort_loc  = Locations.dummy_loc in
  { sort_desc; sort_loc }

let mk_sort_identifier_underscore symbol indexes =
  let sort_desc = Smtlib.SortIdentifier (mk_id_underscore symbol indexes) in
  let sort_loc  = Locations.dummy_loc in
  { sort_desc; sort_loc }

let mk_sort_fun symbol sorts =
  let sort_desc = Smtlib.SortFun (mk_id_symbol symbol, sorts) in
  let sort_loc  = Locations.dummy_loc in
  { sort_desc; sort_loc }

let mk_term_spec_constant constant =
  let term_desc = Smtlib.TermSpecConstant constant in
  let term_loc  = Locations.dummy_loc in
  { term_desc; term_loc }

let mk_term_qual_identifier qual_identifier =
  let term_desc = Smtlib.TermQualIdentifier qual_identifier in
  let term_loc  = Locations.dummy_loc in
  { term_desc; term_loc }

let mk_term_qual_identifier_terms qual_identifier terms =
  let term_desc = Smtlib.TermQualIdentifierTerms (qual_identifier, terms) in
  let term_loc  = Locations.dummy_loc in
  { term_desc; term_loc }

let mk_term_let_term var_bindings term =
  let term_desc = Smtlib.TermLetTerm (var_bindings,term) in
  let term_loc  = Locations.dummy_loc in
  { term_desc; term_loc }

let mk_term_forall_term sorted_vars term =
  let term_desc = Smtlib.TermForallTerm (sorted_vars,term) in
  let term_loc  = Locations.dummy_loc in
  { term_desc; term_loc }

let mk_term_exists_term sorted_vars term =
  let term_desc = Smtlib.TermExistsTerm (sorted_vars,term) in
  let term_loc  = Locations.dummy_loc in
  { term_desc; term_loc }

let mk_fun_def symbol sort sorted_vars term =
  let fun_def_desc = Smtlib.FunDef (symbol, None, sorted_vars, sort, term) in
  let fun_def_loc  = Locations.dummy_loc in
  { fun_def_desc; fun_def_loc }

let mk_cmd_declare_fun symbol sorts sort =
  let command_desc = Smtlib.CmdDeclareFun (symbol, None, sorts, sort) in
  let command_loc  = Locations.dummy_loc in
  { command_desc; command_loc }

let mk_cmd_define_fun fun_def =
  let command_desc = Smtlib.CmdDefineFun fun_def in
  let command_loc  = Locations.dummy_loc in
  { command_desc; command_loc }

let mk_command command_desc : Smtlib.command =
  { command_desc; command_loc = Locations.dummy_loc; }


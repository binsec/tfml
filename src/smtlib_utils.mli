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

val get_logic : Smtlib.script -> string
(** Extracts the logic name from a SMT script *)

val is_constant_term : Smtlib.term -> Smtlib.constant option
(** [is_constant t] checks if the term t is a constant or not.
 *  A real constant might be hidden under an annotated term. *)

val is_variable_term : Smtlib.term -> Smtlib.qual_identifier option
(** [is_variable t] checks if the term t is possibly a variable or not. *)


(* Destructor *)

val string_of_symbol : Smtlib.symbol -> string

val string_of_identifier : Smtlib.identifier -> string
val symbol_of_identifier : Smtlib.identifier -> Smtlib.symbol

val string_of_sort     : Smtlib.sort -> string
val symbol_of_sort     : Smtlib.sort -> Smtlib.symbol
val identifier_of_sort : Smtlib.sort -> Smtlib.identifier
val symbols_of_sort    : Smtlib.sort -> Smtlib.symbol list

val string_of_sorted_var : Smtlib.sorted_var -> string
val symbol_of_sorted_var : Smtlib.sorted_var -> Smtlib.symbol
val sort_of_sorted_var   : Smtlib.sorted_var -> Smtlib.sort

val string_of_var_binding : Smtlib.var_binding -> string
val symbol_of_var_binding : Smtlib.var_binding -> Smtlib.symbol
val term_of_var_binding   : Smtlib.var_binding -> Smtlib.term

val string_of_qual_identifier     : Smtlib.qual_identifier -> string
val symbol_of_qual_identifier     : Smtlib.qual_identifier -> Smtlib.symbol
val identifier_of_qual_identifier : Smtlib.qual_identifier -> Smtlib.identifier


(* Constructor *)
val mk_symbol : string -> Smtlib.symbol

val mk_localized_symbol : string -> Locations.t -> Smtlib.symbol

val mk_idx_num : int -> Smtlib.index

val mk_id_symbol : Smtlib.symbol -> Smtlib.identifier

val mk_id_underscore : Smtlib.symbol -> Smtlib.indexes -> Smtlib.identifier

val mk_qual_identifier_identifier : Smtlib.identifier -> Smtlib.qual_identifier

val mk_sorted_var : Smtlib.symbol -> Smtlib.sort -> Smtlib.sorted_var

val mk_var_binding : Smtlib.symbol -> Smtlib.term -> Smtlib.var_binding

val mk_sort_identifier : Smtlib.symbol -> Smtlib.sort

val mk_sort_identifier_underscore : Smtlib.symbol -> Smtlib.indexes -> Smtlib.sort

val mk_sort_fun : Smtlib.symbol -> Smtlib.sorts -> Smtlib.sort

val mk_term_spec_constant : Smtlib.constant -> Smtlib.term

val mk_term_qual_identifier : Smtlib.qual_identifier -> Smtlib.term

val mk_term_qual_identifier_terms : Smtlib.qual_identifier -> Smtlib.terms -> Smtlib.term

val mk_term_let_term : Smtlib.var_bindings -> Smtlib.term -> Smtlib.term

val mk_term_forall_term : Smtlib.sorted_vars -> Smtlib.term -> Smtlib.term

val mk_term_exists_term : Smtlib.sorted_vars -> Smtlib.term -> Smtlib.term

val mk_fun_def : Smtlib.symbol -> Smtlib.sort -> Smtlib.sorted_vars -> Smtlib.term -> Smtlib.fun_def

val mk_cmd_declare_fun : Smtlib.symbol -> Smtlib.sorts -> Smtlib.sort -> Smtlib.command

val mk_cmd_define_fun : Smtlib.fun_def -> Smtlib.command

val mk_command: Smtlib.command_desc -> Smtlib.command

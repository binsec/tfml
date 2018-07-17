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

val is_bool    : bl term -> bool option
val is_bitvec  : bv term -> Bitvector.t option
val is_integer : ir term -> Z.t option
val is_real    : rl term -> Q.t option

val is_var : 'a term -> 'a var option
val is_fun : 'a term -> ('a var * any_term list) option

val any_var_hash : any_var -> int
val any_var_name : any_var -> string
val any_var_sort : any_var -> any_sort

val any_term_hash : any_term -> int
val any_term_sort : any_term -> any_sort

val decl_var : decl -> any_var
val def_var  : def  -> any_var

val decl_name : decl -> string
val def_name  : def  -> string

val string_of_unop : (_,_) unop -> string
val string_of_bnop : (_,_,_) bnop -> string

type (_,_) eq =
  | Eq : 'a sort * 'a sort -> ('a,'a) eq
  | Nq : 'a sort * 'b sort -> ('a,'b) eq

val equal_sort : 'a sort -> 'b sort -> ('a,'b) eq

module BindEnv :
sig
  type t

  val create : int -> t

  val bind : t -> bind -> unit
  val def  : t -> def  -> unit
  val decl : t -> decl -> unit

  val unbind : t -> bind -> unit
  val undef  : t -> def  -> unit
  val undecl : t -> decl -> unit

  type 'a status =
    | Free
    | Declared of 'a var * any_sort list
    | Defined  of 'a var * any_var list * 'a term

  val lookup : t -> 'a var -> 'a status

  val is_binded  : t -> 'a term -> ('a var * any_var list * 'a term) option

  val is_bool    : t -> bl term -> bool option
  val is_bitvec  : t -> bv term -> Bitvector.t option
  val is_integer : t -> ir term -> Z.t option
  val is_real    : t -> rl term -> Q.t option
  val is_var     : t -> 'a term -> 'a var option
end

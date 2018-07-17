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

(* Uniquify *)

module Uniquify =
struct

  type env = {
    count : (int*int) VarHashtbl.t
  }

  let create n = {
    count = VarHashtbl.create n;
  }

  let find_var_count env v =
    try VarHashtbl.find env.count v
    with Not_found -> (0,0)

  let incr_var env v =
    let (i,j) = find_var_count env v in
    VarHashtbl.add env.count v (i+1,j+1)

  let decr_var env v =
    let (i,j) = find_var_count env v in
    VarHashtbl.add env.count v (i,j-1)

  let get_var env v =
    let (i,j) = find_var_count env v in
    if i = 1 then None else Some (i-j-1)

  let rec count_term : type a. env -> a term -> unit =
    fun env { term_desc; _ } ->
      match term_desc with
      | Bool _ -> ()
      | BitVec _ -> ()
      | Integer _ -> ()
      | Real _ -> ()
      | Fun (_,ls) ->
        List.iter (fun (AnyTerm tm) -> count_term env tm) ls
      | Bind (bn,tm) ->
        count_bind env bn;
        count_term env tm
      | Unop (_,tm) ->
        count_term env tm
      | Bnop (_,tm1,tm2) ->
        count_term env tm1;
        count_term env tm2
      | Ite (bl,tm1,tm2) ->
        count_term env bl;
        count_term env tm1;
        count_term env tm2
      | Select (ax,tm) ->
        count_term env ax;
        count_term env tm
      | Store (ax,tm1,tm2) ->
        count_term env ax;
        count_term env tm1;
        count_term env tm2

  and count_bind env { bind_desc; _ } =
    match bind_desc with
    | Let ls ->
      List.iter
        (fun { def_desc = Def (v,_,tm); _ } ->
           incr_var env (any_var v);
           count_term env tm)
        ls
    | Exists ls
    | Forall ls ->
      List.iter
        (fun { decl_desc = Decl (v,_); _ } ->
           incr_var env (any_var v))
        ls

  let count_entry env { entry_desc; _ } =
    match entry_desc with
    | Declare { decl_desc = Decl (v,_); _ } ->
      incr_var env (any_var v)
    | Define { def_desc = Def (v,_,tm); _ } ->
      incr_var env (any_var v); count_term env tm
    | Assert tm -> count_term env tm
    | Comment _ -> ()

  let rename env v =
    match get_var env (any_var v) with
    | None -> v
    | Some i -> var (v.var_name ^ (string_of_int i)) v.var_sort

  let rec visit_term : type a. env -> a term -> a term =
    fun env ({ term_desc; _ } as tm) ->
      match term_desc with
      | Bool _ -> tm
      | BitVec _ -> tm
      | Integer _ -> tm
      | Real _ -> tm
      | Fun (v,ls) ->
        let ls = List.map (fun (AnyTerm tm) -> any_term (visit_term env tm)) ls in
        mk_fun (rename env v) ls
      | Bind (bn,tm) ->
        let bn = visit_bind env bn in
        let tm = visit_term env tm in
        mk_bind bn tm
      | Unop (u,tm) ->
        let tm = visit_term env tm in
        mk_unop u tm
      | Bnop (b,tm1,tm2) ->
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_bnop b tm1 tm2
      | Ite (bl,tm1,tm2) ->
        let bl = visit_term env bl in
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_ite bl tm1 tm2
      | Select (ax,tm) ->
        let ax = visit_term env ax in
        let tm = visit_term env tm in
        mk_select ax tm
      | Store (ax,tm1,tm2) ->
        let ax = visit_term env ax in
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_store ax tm1 tm2

  and visit_bind env { bind_desc; _ } =
    match bind_desc with
    | Let ls ->
      mk_let
        (List.map
           (fun { def_desc = Def (v,ls,tm); _ } ->
              let tm = visit_term env tm in
              decr_var env (any_var v); mk_def (rename env v) ls tm)
           ls)
    | Exists ls ->
      mk_exists
        (List.map
           (fun { decl_desc = Decl (v,ls); _ } ->
              decr_var env (any_var v); mk_decl (rename env v) ls)
           ls)
    | Forall ls ->
      mk_forall
        (List.map
           (fun { decl_desc = Decl (v,ls); _ } ->
              decr_var env (any_var v); mk_decl (rename env v) ls)
           ls)

  let visit_entry env ({ entry_desc; _ } as en) =
    match entry_desc with
    | Declare _ -> en
    | Define { def_desc = Def (v,ls,tm); _ } ->
      mk_define (mk_def v ls (visit_term env tm))
    | Assert tm ->
      mk_assert (visit_term env tm)
    | Comment _ -> en
end

let uniquify fm =
  let env = Uniquify.create (length fm / 4) in
  iter_forward (Uniquify.count_entry env) fm;
  map_forward (Uniquify.visit_entry env) fm


(* Specialize and generalize *)

module Specialize =
struct

  let rec visit_term : type a. (decl -> bool) -> a term -> a term =
    fun f ({ term_desc; _ } as tm) ->
      match term_desc with
      | Bool _ -> tm
      | BitVec _ -> tm
      | Integer _ -> tm
      | Real _ -> tm
      | Fun (v,ls) -> mk_fun v (List.map (visit_any_term f) ls)
      | Bind (bn,tm) -> visit_bind f bn tm
      | Unop (u,tm) ->
        let tm = visit_term f tm in
        mk_unop u tm
      | Bnop (b,tm1,tm2) ->
        let tm1 = visit_term f tm1 in
        let tm2 = visit_term f tm2 in
        mk_bnop b tm1 tm2
      | Ite (bl,tm1,tm2) ->
        let bl = visit_term f bl in
        let tm1 = visit_term f tm1 in
        let tm2 = visit_term f tm2 in
        mk_ite bl tm1 tm2
      | Select (ax,tm) ->
        let ax = visit_term f ax in
        let tm = visit_term f tm in
        mk_select ax tm
      | Store (ax,tm1,tm2) ->
        let ax = visit_term f ax in
        let tm1 = visit_term f tm1 in
        let tm2 = visit_term f tm2 in
        mk_store ax tm1 tm2

  and visit_any_term env (AnyTerm tm) =
    any_term (visit_term env tm)

  and visit_bind : type a. (decl -> bool) -> bind -> a term -> a term =
    fun f ({ bind_desc; _ } as bn) tm ->
      match bind_desc with
      | Let l ->
        let l = List.map (visit_def f) l in
        mk_bind (mk_let l) (visit_term f tm)
      | Exists _ -> mk_bind bn (visit_term f tm)
      | Forall l ->
        let lex,lfa = List.partition f l in
        mk_bind (mk_exists lex) (mk_bind (mk_forall lfa) (visit_term f tm))

  and visit_def f { def_desc = Def (v,ls,tm); _ } =
    mk_def v ls (visit_term f tm)

  let visit_entry f ({ entry_desc; _ } as en) =
    match entry_desc with
    | Declare _
    | Define _
    | Comment _ -> en
    | Assert bl -> mk_assert (visit_term f bl)
end

let specialize f fm = map_forward (Specialize.visit_entry f) fm

module Generalize =
struct

  let rec visit_term : type a. (decl -> bool) -> a term -> a term =
    fun f ({ term_desc; _ } as tm) ->
      match term_desc with
      | Bool _ -> tm
      | BitVec _ -> tm
      | Integer _ -> tm
      | Real _ -> tm
      | Fun (v,ls) -> mk_fun v (List.map (visit_any_term f) ls)
      | Bind (bn,tm) -> visit_bind f bn tm
      | Unop (u,tm) ->
        let tm = visit_term f tm in
        mk_unop u tm
      | Bnop (b,tm1,tm2) ->
        let tm1 = visit_term f tm1 in
        let tm2 = visit_term f tm2 in
        mk_bnop b tm1 tm2
      | Ite (bl,tm1,tm2) ->
        let bl = visit_term f bl in
        let tm1 = visit_term f tm1 in
        let tm2 = visit_term f tm2 in
        mk_ite bl tm1 tm2
      | Select (ax,tm) ->
        let ax = visit_term f ax in
        let tm = visit_term f tm in
        mk_select ax tm
      | Store (ax,tm1,tm2) ->
        let ax = visit_term f ax in
        let tm1 = visit_term f tm1 in
        let tm2 = visit_term f tm2 in
        mk_store ax tm1 tm2

  and visit_any_term f (AnyTerm tm) =
    any_term (visit_term f tm)

  and visit_bind : type a. (decl -> bool) -> bind -> a term -> a term =
    fun f ({ bind_desc; _ } as bn) tm ->
      match bind_desc with
      | Let l ->
        let l = List.map (visit_def f) l in
        mk_bind (mk_let l) (visit_term f tm)
      | Forall _ -> mk_bind bn (visit_term f tm)
      | Exists l ->
        let lfa,lex = List.partition f l in
        mk_bind (mk_exists lex) (mk_bind (mk_forall lfa) (visit_term f tm))

  and visit_def f { def_desc = Def (v,ls,tm); _ } =
    mk_def v ls (visit_term f tm)

  let visit_entry f ({ entry_desc; _ } as en) =
    match entry_desc with
    | Declare _
    | Define _
    | Comment _ -> en
    | Assert bl -> mk_assert (visit_term f bl)
end

let generalize f fm = map_forward (Generalize.visit_entry f) fm


(* Assert table  *)

module Assertbl :
sig
  type t
  val create : unit -> t
  val get : t -> bl term -> bool
  val set : t -> bl term -> unit
end = struct
  type t = bool TermHashtbl.t

  let create () =
    let t = TermHashtbl.create 1 in
    TermHashtbl.add t (any_term mk_bl_true) true;
    t

  let get t bl =
    try TermHashtbl.find t (any_term bl)
    with Not_found -> false

  let set t bl =
    TermHashtbl.replace t (any_term bl) true
end


(* Constant propagation *)

module ConstantPropagation =
struct

  type env = {
    assertbl : Assertbl.t;
    bindenv  : BindEnv.t;
  }

  let create n = {
    assertbl = Assertbl.create ();
    bindenv  = BindEnv.create n;
  }

  let get_assertbl env bl = Assertbl.get env.assertbl bl
  let set_assertbl env bl = Assertbl.set env.assertbl bl

  let keep env { def_desc = Def (_,_,tm); _ } =
    BindEnv.is_var env.bindenv tm = None &&
    (match tm.term_sort.sort_desc with
     | BlSort -> BindEnv.is_bool env.bindenv tm = None
     | BvSort _ -> BindEnv.is_bitvec env.bindenv tm = None
     | IrSort -> BindEnv.is_integer env.bindenv tm = None
     | RlSort -> BindEnv.is_real env.bindenv tm = None
     | AxSort _ -> true)

  let filter_bind env bn =
    match bn.bind_desc with
    | Let dfs -> mk_let (List.filter (keep env) dfs)
    | Exists _ | Forall _ -> bn

  let bind_assert env tm =
    (match tm.term_desc with
     | Bnop (Equal,tm1,tm2) ->
       (match BindEnv.is_var env.bindenv tm1,
              BindEnv.is_var env.bindenv tm2 with
       | None, None | Some _, Some _ -> ()
       | Some v, None -> BindEnv.def env.bindenv (mk_def v [] tm2)
       | None, Some v -> BindEnv.def env.bindenv (mk_def v [] tm1))
     | _ -> ())


  let visit_list : 'env 'a 'b.
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
    fun f env ls -> List.map (f env) ls

  let rec visit_term : type a. env -> a term -> a term =
    fun env tm ->
      match tm.term_desc with
      | Bool _ -> tm
      | BitVec _ -> tm
      | Integer _ -> tm
      | Real _  -> tm

      | Fun (v,ls) ->
        let ls = visit_list visit_any_term env ls in
        let tm = mk_fun v ls in
        (match BindEnv.is_var env.bindenv tm with
         | Some v -> mk_fun v ls
         | None ->
           (match v.var_sort.sort_desc with
            | BlSort ->
              (match BindEnv.is_bool env.bindenv tm with
               | Some bl -> mk_bool bl
               | None -> tm)
            | BvSort _ ->
              (match BindEnv.is_bitvec env.bindenv tm with
               | Some bv -> mk_bitvec bv
               | None -> tm)
            | IrSort ->
              (match BindEnv.is_integer env.bindenv tm with
               | Some ir -> mk_integer ir
               | None -> tm)
            | RlSort ->
              (match BindEnv.is_real env.bindenv tm with
               | Some rl -> mk_real rl
               | None -> tm)
            | AxSort _ -> tm))

      | Bind (bn,tm) ->
        let bn = visit_bind env bn in
        BindEnv.bind env.bindenv bn;
        let tm = visit_term env tm in
        let bn' = filter_bind env bn in
        BindEnv.unbind env.bindenv bn;
        mk_bind bn' tm

      | Unop (u,tm) ->
        let tm = visit_term env tm in
        mk_unop u tm

      | Bnop (b,tm1,tm2) ->
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_bnop b tm1 tm2

      | Ite (bl,tm1,tm2) ->
        let bl = visit_term env bl in
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_ite bl tm1 tm2

      | Select (ax,tm) ->
        let ax = visit_term env ax in
        let tm = visit_term env tm in
        mk_select ax tm

      | Store (ax,tm1,tm2) ->
        let ax = visit_term env ax in
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_store ax tm1 tm2

  and visit_any_term env (AnyTerm tm) =
    any_term (visit_term env tm)

  and visit_bind env bn =
    match bn.bind_desc with
    | Let ls ->
      let ls = visit_list visit_def env ls in
      mk_let ls
    | Exists _ | Forall _ -> bn

  and visit_def env { def_desc = Def (v,ls,tm); _ } =
    let dcs = List.map (fun (AnyVar v) -> mk_decl v []) ls in
    List.iter (BindEnv.decl env.bindenv) dcs;
    let tm = visit_term env tm in
    List.iter (BindEnv.undecl env.bindenv) dcs;
    mk_def v ls tm

  let visit_entry env en =
    match en.entry_desc with
    | Declare dc ->
      BindEnv.decl env.bindenv dc;
      en
    | Define df ->
      let df = visit_def env df in
      BindEnv.def env.bindenv df;
      mk_define df
    | Assert bl ->
      let bl = visit_term env bl in
      bind_assert env bl;
      mk_assert bl
    | Comment _ -> en

end

let constant_propagation ?(keep=VarSet.empty) fm =
  let env = ConstantPropagation.create (length fm / 4) in
  fold_forward
    (fun entry fm ->
       let entry = ConstantPropagation.visit_entry env entry in
       match entry.entry_desc with
       | Declare _ | Comment _ -> push_front entry fm
       | Define ({ def_desc = Def (v,_,_); _ } as df) ->
         if (ConstantPropagation.keep env df || VarSet.mem (any_var v) keep)
         then push_front entry fm
         else fm
       | Assert bl ->
         if ConstantPropagation.get_assertbl env bl then fm
         else (ConstantPropagation.set_assertbl env bl; push_front entry fm))
    fm empty


(* Prune and inline *)

module PruneAndInline =
struct
  type env = {
    keep : VarSet.t;
    count : int VarHashtbl.t;
    bindenv  : BindEnv.t;
  }

  let create keep n = {
    keep;
    count   = VarHashtbl.create n;
    bindenv = BindEnv.create n;
  }

  let count_var env v =
    try VarHashtbl.find env.count (any_var v)
    with Not_found -> 0

  let incr_var env v =
    VarHashtbl.replace env.count (any_var v) (count_var env v + 1)

  let find_var env v =
    let open BindEnv in
    match lookup env.bindenv v with
    | Free | Declared _ -> None
    | Defined (_,_,tm) -> Some tm

  let keep_var env v =
    count_var env v > 0 || VarSet.mem (any_var v) env.keep

  let keep_decl env { decl_desc = Decl (v,_); _ } = keep_var env v

  let keep_def env { def_desc = Def (v,_,tm); _ } =
    count_var env v > 1 || VarSet.mem (any_var v) env.keep ||
    (match v.var_sort.sort_desc with
     | BlSort -> false
     | BvSort _ -> false
     | IrSort -> false
     | RlSort -> false
     | AxSort _ -> count_var env v = 1 && is_var tm = None)

  let filter_bind env bn =
    match bn.bind_desc with
    | Let dfs -> mk_let (List.filter (keep_def env) dfs)
    | Exists dcs -> mk_exists (List.filter (keep_decl env) dcs)
    | Forall dcs -> mk_forall (List.filter (keep_decl env) dcs)


  let count_list : 'env 'a.
    ('env -> 'a -> unit) -> 'env -> 'a list -> unit =
    fun f env ls -> List.iter (f env) ls

  let rec count_term : type a. env -> a term -> unit =
    fun env tm ->
      match tm.term_desc with
      | Bool _ -> ()
      | BitVec _ -> ()
      | Integer _ -> ()
      | Real _ -> ()
      | Fun (v,ls) ->
        incr_var env v;
        count_list count_any_term env ls
      | Bind (bn,tm) ->
        count_term env tm;
        count_bind env bn
      | Unop (_,tm) ->
        count_term env tm
      | Bnop (_,tm1,tm2) ->
        count_term env tm1;
        count_term env tm2
      | Ite (bl,tm1,tm2) ->
        count_term env bl;
        count_term env tm1;
        count_term env tm2
      | Select (ax,tm) ->
        count_term env ax;
        count_term env tm
      | Store (ax,tm1,tm2) ->
        count_term env ax;
        count_term env tm1;
        count_term env tm2

  and count_any_term env (AnyTerm tm) =
    count_term env tm

  and count_bind env bn =
    match bn.bind_desc with
    | Let ls -> count_list count_def env ls
    | Exists _ | Forall _ -> ()

  and count_def env { def_desc = Def (v,ls,tm); _ } =
    let dcs = List.map (fun (AnyVar v) -> mk_decl v []) ls in
    List.iter (BindEnv.decl env.bindenv) dcs;
    if (keep_var env v) then count_term env tm;
    List.iter (BindEnv.undecl env.bindenv) dcs

  let count_entry env en =
    match en.entry_desc with
    | Declare dc -> BindEnv.decl env.bindenv dc
    | Define df -> BindEnv.def env.bindenv df; count_def env df
    | Assert bl -> count_term env bl
    | Comment _ -> ()


  let visit_list : 'env 'a.
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
    fun f env ls -> List.map (f env) ls

  let rec visit_term : type a. env -> a term -> a term =
    fun env tm ->
      match tm.term_desc with
      | Bool _ -> tm
      | BitVec _ -> tm
      | Integer _ -> tm
      | Real _ -> tm

      | Fun (v,ls) ->
        let ls = visit_list visit_any_term env ls in
        if VarSet.mem (any_var v) env.keep then mk_fun v ls
        else
        if count_var env v = 1 then
          match find_var env v with
          | None -> mk_fun v ls
          | Some tm ->
            match is_var tm with
            | None ->
              (match v.var_sort.sort_desc with
               | BlSort -> visit_term env tm
               | BvSort _ -> visit_term env tm
               | IrSort -> visit_term env tm
               | RlSort -> visit_term env tm
               | AxSort _ -> mk_fun v ls)
            | Some v' ->
              if v = v' then tm
              else visit_term env (mk_fun v' ls)
        else mk_fun v ls

      | Bind (bn,tm) ->
        let bn = visit_bind env bn in
        BindEnv.bind env.bindenv bn;
        let tm = visit_term env tm in
        let bn' = filter_bind env bn in
        BindEnv.unbind env.bindenv bn;
        mk_bind bn' tm

      | Unop (u,tm) ->
        let tm = visit_term env tm in
        mk_unop u tm
      | Bnop (b,tm1,tm2) ->
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_bnop b tm1 tm2
      | Ite (bl,tm1,tm2) ->
        let bl = visit_term env bl in
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_ite bl tm1 tm2
      | Select (ax,tm) ->
        let ax = visit_term env ax in
        let tm = visit_term env tm in
        mk_select ax tm
      | Store (ax,tm1,tm2) ->
        let ax = visit_term env ax in
        let tm1 = visit_term env tm1 in
        let tm2 = visit_term env tm2 in
        mk_store ax tm1 tm2

  and visit_any_term env (AnyTerm tm) =
    any_term (visit_term env tm)

  and visit_bind env bn =
    match bn.bind_desc with
    | Let ls ->
      let ls = visit_list visit_def env ls in
      mk_let ls
    | Exists _ | Forall _ -> bn

  and visit_def env ({ def_desc = Def (v,ls,tm); _ } as df) =
    let dcs = List.map (fun (AnyVar v) -> mk_decl v []) ls in
    List.iter (BindEnv.decl env.bindenv) dcs;
    let tm = if keep_def env df then visit_term env tm else tm in
    List.iter (BindEnv.undecl env.bindenv) dcs;
    mk_def v ls tm

  let visit_entry env en =
    match en.entry_desc with
    | Declare dc -> mk_declare dc
    | Define df -> mk_define (visit_def env df)
    | Assert bl -> mk_assert (visit_term env bl)
    | Comment s -> mk_comment s

end

let prune_and_inline ?(keep=VarSet.empty) fm =
  let env = PruneAndInline.create keep (length fm / 4) in
  iter_backward (PruneAndInline.count_entry env) fm;
  fold_backward
    (fun entry fm ->
       let entry = PruneAndInline.visit_entry env entry in
       match entry.entry_desc with
       | Declare dc ->
         if PruneAndInline.keep_decl env dc
         then push_back entry fm
         else fm
       | Define df ->
         if PruneAndInline.keep_def env df
         then push_back entry fm
         else fm
       | Assert _ | Comment _ -> push_back entry fm)
    fm empty


(* Static single assignment *)

module StaticSingleAssignment =
struct

  type env = {
    params : VarSet.t;
    name_count : int Utils.StringHashtbl.t;
    term_alias : string TermHashtbl.t;
    var_params : (VarSet.t) VarHashtbl.t;
  }

  let create n = {
    params = VarSet.empty;
    name_count = Utils.StringHashtbl.create n;
    term_alias = TermHashtbl.create n;
    var_params = VarHashtbl.create n;
  }

  let rec fresh t name =
    let i =
      try Utils.StringHashtbl.find t.name_count name
      with Not_found -> 0
    in
    Utils.StringHashtbl.replace t.name_count name (i+1);
    let fr = Printf.sprintf "%s_%i" name i in
    if Utils.StringHashtbl.mem t.name_count fr
    then fresh t name
    else (Utils.StringHashtbl.add t.name_count fr 0; fr)

  let fresh t name tm = var (fresh t name) tm.term_sort

  let add_term_alias t v tm =
    TermHashtbl.add t.term_alias (any_term tm) v.var_name

  let find_term_alias t tm =
    try
      let name = TermHashtbl.find t.term_alias (any_term tm) in
      Some (var name tm.term_sort)
    with Not_found -> None

  let add_var_params t v params =
    VarHashtbl.add t.var_params (any_var v) params

  let find_var_params t v =
    try VarHashtbl.find t.var_params (any_var v)
    with Not_found -> VarSet.empty

  let finalize_var_params env set seq v =
    if VarSet.mem (any_var v) env.params
    then VarSet.add (any_var v) set, seq, mk_fun v []
    else
      let params = find_var_params env v in
      let ls =
        List.map
          (fun (AnyVar v) -> any_term (mk_fun v []))
          (VarSet.elements params)
      in VarSet.union set params, seq, mk_fun v ls

  let finalize bool name env set seq tm =
    if bool then set, seq, tm
    else
      match find_term_alias env tm with
      | Some v -> finalize_var_params env set seq v
      | None ->
        let v = fresh env name tm in
        let ls = VarSet.elements set in
        add_term_alias env v tm;
        add_var_params env v set;
        set, push_front_define (mk_def v ls tm) seq,
        mk_fun v (List.map (fun (AnyVar v) -> any_term (mk_fun v [])) ls)

  let rec visit_term :
    type a. bool -> string -> env -> formula -> a term -> VarSet.t * formula * a term =
    fun bool string env seq ({ term_desc; _ } as tm) ->
      match term_desc with
      | Bool _ -> VarSet.empty, seq, tm
      | BitVec _ -> VarSet.empty, seq, tm
      | Integer _ -> VarSet.empty, seq, tm
      | Real _ -> VarSet.empty, seq, tm

      | Fun (v,ls) ->
        (match ls with
         | [] -> finalize_var_params env VarSet.empty seq v
         | _  ->
           let set,seq,lst =
             List.fold_left
               (fun (set,seq,lst) tm ->
                  let st,sq,tm = visit_any_term false string env seq tm in
                  VarSet.union st set, sq, tm :: lst)
               (VarSet.empty,seq,[]) ls
           in set, seq, mk_fun v (List.rev lst))

      | Bind (bn,tm) ->
        let env,seq,bn = visit_bind env seq bn in
        let set,seq,tm  = visit_term false string env seq tm in
        set, seq, mk_bind bn tm

      | Unop (u,tm) ->
        let set,seq,tm  = visit_term false string env seq tm in
        finalize bool (string ^ "_" ^ string_of_unop u) env set seq (mk_unop u tm)
      | Bnop (b,tm1,tm2) ->
        let set1,seq,tm1 = visit_term false string env seq tm1 in
        let set2,seq,tm2 = visit_term false string env seq tm2 in
        let set = VarSet.union set1 set2 in
        finalize bool (string ^ "_" ^ string_of_bnop b) env set seq (mk_bnop b tm1 tm2)
      | Ite (bl,tm1,tm2) ->
        let set0,seq,bl  = visit_term false string env seq bl in
        let set1,seq,tm1 = visit_term false string env seq tm1 in
        let set2,seq,tm2 = visit_term false string env seq tm2 in
        let set = VarSet.union set0 (VarSet.union set1 set2) in
        finalize bool (string ^ "_ite") env set seq (mk_ite bl tm1 tm2)
      | Select (ax,tm) ->
        let set1,seq,ax = visit_term false string env seq ax in
        let set2,seq,tm = visit_term false string env seq tm in
        let set = VarSet.union set1 set2 in
        finalize bool (string ^ "_select") env set seq (mk_select ax tm)
      | Store (ax,tm1,tm2) ->
        let set0,seq,ax  = visit_term false string env seq ax in
        let set1,seq,tm1 = visit_term false string env seq tm1 in
        let set2,seq,tm2 = visit_term false string env seq tm2 in
        let set = VarSet.union set0 (VarSet.union set1 set2) in
        finalize bool (string ^ "_store") env set seq (mk_store ax tm1 tm2)

  and visit_any_term bool string env seq (AnyTerm tm) =
    let set,seq,tm = visit_term bool string env seq tm in
    set, seq, any_term tm

  and visit_bind env seq ({ bind_desc; _ } as bn) =
    match bind_desc with
    | Let ls ->
      let seq =
        List.fold_left
          (fun seq { def_desc = Def (v,l,tm); _ } ->
             let set,seq,tm = visit_term true v.var_name env seq tm in
             assert (l = []); add_var_params env v set;
             push_front_define (mk_def v (VarSet.elements set) tm) seq)
          seq ls
      in env, seq, mk_let []

    | Exists ls ->
      let lst = List.map any_var_sort (VarSet.elements env.params) in
      let seq =
        List.fold_left
          (fun seq { decl_desc = Decl (v,l); _ } ->
             assert (l = []); add_var_params env v env.params;
             push_front_declare (mk_decl v lst) seq)
          seq ls
      in  env, seq, mk_exists []

    | Forall ls ->
      let params =
        List.fold_left
          (fun p { decl_desc = Decl (v,l); _ } ->
             assert (l = []); VarSet.add (any_var v) p)
          env.params ls
      in { env with params }, seq, bn

  let visit_entry env seq { entry_desc; _ } =
    match entry_desc with
    | Declare dc -> push_front_declare dc seq
    | Define { def_desc = Def (v,ls,tm); _ } ->
      let params = List.fold_left (fun p v -> VarSet.add v p) env.params ls in
      let _,seq,tm = visit_term true v.var_name { env with params } seq tm in
      push_front_define (mk_def v ls tm) seq
    | Assert bl ->
      let _,seq,bl = visit_term false "assert" env seq bl in
      push_front_assert bl seq
    | Comment c -> push_front_comment c seq

  let reserve_name t name = Utils.StringHashtbl.add t.name_count name 0

  let reserve_entry env em =
    match em.entry_desc with
    | Assert _ | Comment _ -> ()
    | Declare dc -> reserve_name env (decl_name dc)
    | Define  df -> reserve_name env (def_name df)

end

let static_single_assignment fm =
  let env = StaticSingleAssignment.create (length fm / 4) in
  iter_forward (StaticSingleAssignment.reserve_entry env) fm;
  fold_forward
    (fun en seq -> StaticSingleAssignment.visit_entry env seq en)
    fm empty


(* Taint *)

module Taint =
struct

  type env = {
    tainted : VarSet.t;
    bindenv : BindEnv.t;
    reprtbl : any_term TermHashtbl.t;
    taintbl : any_term TermHashtbl.t;
  }

  let create tainted n = {
    tainted;
    bindenv = BindEnv.create n;
    reprtbl = TermHashtbl.create n;
    taintbl = TermHashtbl.create n;
  }

  let taintify v = mk_bl_var (v.var_name ^ "_taint")

  let arrayify (v: (_,_) ax var) =
    let AxSort (idx,_) = v.var_sort.sort_desc in
    mk_ax_var (v.var_name ^ "_array") idx mk_bl_sort

  let taint_unop : ('a,'b) unop -> ('a term * bl term) -> bl term =
    fun  _ (_,bl) -> bl

  let taint_bnop :
    type a b c. c sort -> (a,b,c) bnop -> (a term * bl term) -> (b term * bl term) -> bl term =
    fun s b (tm1,bl1) (tm2,bl2) ->
      match tm1.term_sort.sort_desc, tm2.term_sort.sort_desc, s.sort_desc with
      | BlSort, BlSort, BlSort ->
        (match b with
         | BlImply ->
           mk_bl_or
             (mk_bl_or
                (mk_bl_and bl1 (mk_equal tm1 mk_bl_false))
                (mk_bl_and bl2 (mk_equal tm2 mk_bl_true)))
             (mk_bl_and bl1 bl2)
         | BlAnd ->
           mk_bl_or
             (mk_bl_or
                (mk_bl_and bl1 (mk_equal tm1 mk_bl_false))
                (mk_bl_and bl2 (mk_equal tm2 mk_bl_false)))
             (mk_bl_and bl1 bl2)
         | BlOr ->
           mk_bl_or
             (mk_bl_or
                (mk_bl_and bl1 (mk_equal tm1 mk_bl_true))
                (mk_bl_and bl2 (mk_equal tm2 mk_bl_true)))
             (mk_bl_and bl1 bl2)
         | Equal | Distinct
         | BlXor -> mk_bl_and bl1 bl2)

      | BvSort _, BvSort _, BlSort ->
        (match b with
         | Equal | Distinct
         | BvUlt | BvUle | BvUgt | BvUge
         | BvSlt | BvSle | BvSgt | BvSge -> mk_bl_and bl1 bl2)

      | BvSort _, BvSort _, BvSort sz  ->
        (match b with
         | BvAnd | BvNand | BvMul ->
           mk_bl_or
             (mk_bl_or
                (mk_bl_and bl1 (mk_equal tm1 (mk_bv_zeros sz)))
                (mk_bl_and bl2 (mk_equal tm2 (mk_bv_zeros sz))))
             (mk_bl_and bl1 bl2)
         | BvOr | BvNor ->
           mk_bl_or
             (mk_bl_or
                (mk_bl_and bl1 (mk_equal tm1 (mk_bv_fill sz)))
                (mk_bl_and bl2 (mk_equal tm2 (mk_bv_fill sz))))
             (mk_bl_and bl1 bl2)
         | BvConcat
         | BvXor  | BvXnor | BvCmp | BvAdd | BvSub
         | BvUdiv | BvSdiv
         | BvUrem | BvSrem | BvSmod
         | BvShl  | BvAshr | BvLshr -> mk_bl_and bl1 bl2)

      | IrSort, IrSort, IrSort ->
        (match b with
         | IrMul ->
           mk_bl_or
             (mk_bl_or
                (mk_bl_and bl1 (mk_equal tm1 mk_ir_zero))
                (mk_bl_and bl2 (mk_equal tm2 mk_ir_zero)))
             (mk_bl_and bl1 bl2)
         | IrAdd | IrSub | IrDiv | IrMod -> mk_bl_and bl1 bl2)

      | RlSort, RlSort, RlSort ->
        (match b with
         | RlMul ->
           mk_bl_or
             (mk_bl_or
                (mk_bl_and bl1 (mk_equal tm1 mk_rl_zero))
                (mk_bl_and bl2 (mk_equal tm2 mk_rl_zero)))
             (mk_bl_and bl1 bl2)
         | RlAdd | RlSub | RlDiv -> mk_bl_and bl1 bl2)

      | _ -> mk_bl_and bl1 bl2

  let add_taint_array : type a b. env -> (a,b) ax term -> (a,bl) ax term -> unit =
    fun env ax bx -> TermHashtbl.add env.taintbl (any_term ax) (any_term bx)

  let find_taint_array : type a b. env -> (a,b) ax term -> (a,bl) ax term =
    fun env ax ->
      let AnyTerm bx = TermHashtbl.find env.taintbl (any_term ax) in
      let AxSort (idx,_) = ax.term_sort.sort_desc in
      match equal_sort (mk_ax_sort idx mk_bl_sort) bx.term_sort with
      | Eq _ -> bx
      | Nq _ -> assert false

  let add_repr_array : type a. env -> (a,bl) ax term -> (a,bl) ax term -> unit =
    fun env ax bx -> TermHashtbl.add env.reprtbl (any_term ax) (any_term bx)

  let find_repr_array : type a. env -> (a,bl) ax term -> (a,bl) ax term =
    fun env ax ->
      let AnyTerm bx =
        try TermHashtbl.find env.reprtbl (any_term ax)
        with Not_found -> any_term ax
      in
      match equal_sort ax.term_sort bx.term_sort with
      | Eq _ -> bx
      | Nq _ -> assert false

  let rec visit_term : type a. env -> a term -> bl term =
    fun env ({ term_desc; term_sort; _ } as tm) ->
      match term_desc with
      | Bool _ -> mk_bl_true
      | BitVec _ -> mk_bl_true
      | Integer _ -> mk_bl_true
      | Real _ -> mk_bl_true

      | Fun (v,ls) ->
        (match term_sort.sort_desc with
         | AxSort _ ->
           let v_a = arrayify v in
           let tm_a = mk_fun v_a ls in
           add_taint_array env tm tm_a;
           add_repr_array env tm_a (find_repr_array env (mk_fun v_a []))
         | _ -> ());
        (match BindEnv.lookup env.bindenv v with
         | BindEnv.Free -> mk_bl_false
         | BindEnv.Declared _ ->
           mk_fun (taintify v) []
         | BindEnv.Defined _ ->
           let ls_t = List.map (fun (AnyTerm tm) -> visit_term env tm) ls in
           let v_t = mk_fun (taintify v) (ls @ (List.map any_term ls_t)) in
           mk_bl_and v_t (List.fold_right mk_bl_and ls_t mk_bl_true))

      | Bind (_,_) -> mk_bl_false

      | Unop (u,tm) ->
        let tm_t = visit_term env tm in
        taint_unop u (tm,tm_t)

      | Bnop (b,tm1,tm2) ->
        let tm1_t = visit_term env tm1 in
        let tm2_t = visit_term env tm2 in
        taint_bnop tm.term_sort b (tm1,tm1_t) (tm2,tm2_t)

      | Ite (bl,tm1,tm2) ->
        let bl_t  = visit_term env bl in
        let tm1_t = visit_term env tm1 in
        let tm2_t = visit_term env tm2 in
        (match term_sort.sort_desc with
         | AxSort _ ->
           let tm1_a = find_taint_array env tm1 in
           let tm2_a = find_taint_array env tm2 in
           let tm_a  = mk_ite bl tm1_a tm2_a in
           add_taint_array env tm tm_a;
           add_repr_array env tm_a
             (mk_ite bl (find_repr_array env tm1_a) (find_repr_array env tm2_a))
         | _ -> ());
        mk_bl_or
          (mk_bl_and bl_t (mk_ite bl tm1_t tm2_t))
          (mk_bl_and (mk_bl_and tm1_t tm2_t) (mk_equal tm1 tm2))

      | Select (ax,tm) ->
        let ax_t = visit_term env ax in
        let tm_t = visit_term env tm in
        let ax_a = find_taint_array env ax in
        let ax_r = find_repr_array env ax_a in
        (match term_sort.sort_desc with
         | AxSort _ -> assert false (*TODO: arrays of arrays *)
         | _ -> ());
        mk_bl_or
          (mk_bl_and
             (mk_equal (mk_select ax_r tm) ax_t)
             (mk_bl_and tm_t (mk_select ax_a tm)))
          (mk_bl_and ax_t tm_t)

      | Store (ax,tm1,tm2) ->
        let ax_t  = visit_term env ax in
        let tm1_t = visit_term env tm1 in
        let tm2_t = visit_term env tm2 in
        let ax_a  = find_taint_array env ax in
        let ax_a' = mk_store ax_a tm1 tm2_t in
        add_taint_array env tm ax_a';
        add_repr_array env ax_a' (find_repr_array env ax_a);
        mk_bl_and ax_t tm1_t

  let visit_declare env fm ({ decl_desc = Decl (v,ls); _ } as dc) =
    BindEnv.decl env.bindenv dc;
    push_front_define
      (mk_def (taintify v) [] (mk_bool (not (VarSet.mem (any_var v) env.tainted))))
      (match v.var_sort.sort_desc with
       | AxSort _ -> push_front_declare (mk_decl (arrayify v) ls) fm
       |  _ -> fm)

  let visit_define env fm ({ def_desc = Def (v,ls,tm); _ } as df) =
    BindEnv.def env.bindenv df;
    let ls_t = List.map (fun (AnyVar v) -> any_var (taintify v)) ls in
    let tm_t = visit_term env tm in
    push_front_define
      (mk_def (taintify v) (ls @ ls_t) tm_t)
      (match v.var_sort.sort_desc with
       | AxSort _ ->
         let v_a = arrayify v in
         let tm_a = find_taint_array env tm in
         add_repr_array env (mk_fun v_a []) (find_repr_array env tm_a);
         push_front_define (mk_def v_a ls tm_a) fm
       | _ -> fm);

end

let taint vars fm =
  let env = Taint.create vars (length fm / 4) in
  fold_forward
    (fun ({ entry_desc; _ } as en) fm ->
       let fm = push_front en fm in
       match entry_desc with
       | Declare dc -> Taint.visit_declare env fm dc
       | Define df -> Taint.visit_define env fm df
       | Assert bl -> push_front_assert (Taint.visit_term env bl) fm
       | Comment _ -> fm)
    fm empty

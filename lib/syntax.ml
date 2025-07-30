module StringSet = Set.Make (String)
open Z3

(**
  Definition of the expr terms with
  the functions associated.
*)
module Expr = struct
  type t = Var of string | Int of int | Add of t * t | Sub of t * t | Read

  let rec to_string = function
    | Var x -> x
    | Int i -> string_of_int i
    | Add (e1, e2) -> to_string e1 ^ " + " ^ to_string e2
    | Sub (e1, e2) -> to_string e1 ^ " - " ^ to_string e2
    | Read -> "read()"
  ;;

  let rec to_expr ctx = function
    | Var x -> Arithmetic.Integer.mk_const_s ctx x
    | Int i -> Arithmetic.Integer.mk_numeral_i ctx i
    | Add (a, b) -> Arithmetic.mk_add ctx [ to_expr ctx a; to_expr ctx b ]
    | Sub (a, b) -> Arithmetic.mk_sub ctx [ to_expr ctx a; to_expr ctx b ]
    | Read -> failwith "Read shouldn't appear in a formula to check sat!"
  ;;

  let variables phi =
    let rec aux t = function
      | Var x -> StringSet.add x t
      | Int _ -> t
      | Add (e1, e2) | Sub (e1, e2) -> aux (aux t e1) e2
      | Read -> t
    in
    aux StringSet.empty phi
  ;;

  let rec var_in x = function
    | Var x_ -> x = x_
    | Int _ -> false
    | Add (e1, e2) | Sub (e1, e2) -> var_in x e1 || var_in x e2
    | Read -> false
  ;;

  let rec equal a b =
    match (a, b) with
    | Var xa, Var xb -> xa = xb || (xa.[0] = '@' && xb.[0] = '@')
    | Int i, Int j -> i = j
    | Add (ea1, ea2), Add (eb1, eb2) | Sub (ea1, ea2), Sub (eb1, eb2) ->
        equal ea1 eb1 && equal ea2 eb2
    | Read, Read -> true
    | _ -> false
  ;;
end

module Cond = struct
  type t =
    | Cte of bool
    | Not of t
    | And of t * t
    | Or of t * t
    | Eq of Expr.t * Expr.t
    | Le of Expr.t * Expr.t
    | Lt of Expr.t * Expr.t
    | Ge of Expr.t * Expr.t
    | Gt of Expr.t * Expr.t

  let rec to_string = function
    | Cte true -> "T"
    | Cte false -> "F"
    | Not b -> "¬(" ^ to_string b ^ ")"
    | And (b1, b2) -> to_string b1 ^ " ^ " ^ to_string b2
    | Or (b1, b2) -> "(" ^ to_string b1 ^ " v " ^ to_string b2 ^ ")"
    | Eq (e1, e2) -> Expr.to_string e1 ^ " == " ^ Expr.to_string e2
    | Le (e1, e2) -> Expr.to_string e1 ^ " <= " ^ Expr.to_string e2
    | Lt (e1, e2) -> Expr.to_string e1 ^ " < " ^ Expr.to_string e2
    | Ge (e1, e2) -> Expr.to_string e1 ^ " >= " ^ Expr.to_string e2
    | Gt (e1, e2) -> Expr.to_string e1 ^ " > " ^ Expr.to_string e2
  ;;

  let rec to_expr ctx = function
    | Cte b -> Boolean.mk_val ctx b
    | Not b -> Boolean.mk_not ctx (to_expr ctx b)
    | And (b1, b2) -> Boolean.mk_and ctx [ to_expr ctx b1; to_expr ctx b2 ]
    | Or (b1, b2) -> Boolean.mk_or ctx [ to_expr ctx b1; to_expr ctx b2 ]
    | Eq (e1, e2) ->
        Boolean.mk_eq ctx (Expr.to_expr ctx e1) (Expr.to_expr ctx e2)
    | Le (e1, e2) ->
        Arithmetic.mk_le ctx (Expr.to_expr ctx e1) (Expr.to_expr ctx e2)
    | Lt (e1, e2) ->
        Arithmetic.mk_lt ctx (Expr.to_expr ctx e1) (Expr.to_expr ctx e2)
    | Ge (e1, e2) ->
        Arithmetic.mk_ge ctx (Expr.to_expr ctx e1) (Expr.to_expr ctx e2)
    | Gt (e1, e2) ->
        Arithmetic.mk_gt ctx (Expr.to_expr ctx e1) (Expr.to_expr ctx e2)
  ;;

  let variables phi =
    let rec aux t = function
      | Cte _ -> t
      | Not b -> aux t b
      | And (b1, b2) | Or (b1, b2) -> aux (aux t b1) b2
      | Eq (e1, e2) | Le (e1, e2) | Lt (e1, e2) | Ge (e1, e2) | Gt (e1, e2) ->
          StringSet.union t
            (StringSet.union (Expr.variables e1) (Expr.variables e2))
    in
    aux StringSet.empty phi
  ;;

  let rec var_in x = function
    | Cte _ -> false
    | Not b -> var_in x b
    | And (b1, b2) | Or (b1, b2) -> var_in x b1 || var_in x b2
    | Eq (e1, e2) | Le (e1, e2) | Lt (e1, e2) | Ge (e1, e2) | Gt (e1, e2) ->
        Expr.var_in x e1 || Expr.var_in x e2
  ;;

  let rec equal a b =
    match (a, b) with
    | Cte ab, Cte bb -> ab = bb
    | Not ab, Not bb -> equal ab bb
    | And (ab1, ab2), And (bb1, bb2) | Or (ab1, ab2), Or (bb1, bb2) ->
        equal ab1 bb1 && equal ab2 bb2
    | Eq (ea1, ea2), Eq (eb1, eb2)
    | Le (ea1, ea2), Le (eb1, eb2)
    | Lt (ea1, ea2), Lt (eb1, eb2)
    | Ge (ea1, ea2), Ge (eb1, eb2)
    | Gt (ea1, ea2), Gt (eb1, eb2) ->
        Expr.equal ea1 eb1 && Expr.equal ea2 eb2
    | _ -> false
  ;;
end

module Stmt = struct
  type t =
    | Skip
    | Fail
    | Assert of Cond.t
    | Define of string * Expr.t
    | Follow of t * t
    | While of Cond.t * t
    | If of Cond.t * t * t

  let rec to_string = function
    | Skip -> "skip"
    | Fail -> "fail"
    | Assert b -> "assert(" ^ Cond.to_string b ^ ")"
    | Define (x, e) -> x ^ " <- " ^ Expr.to_string e
    | Follow (c1, c2) -> to_string c1 ^ "; " ^ to_string c2
    | While (cond, c) ->
        "while " ^ Cond.to_string cond ^ " do " ^ to_string c ^ " done"
    | If (cond, cthen, celse) ->
        "if " ^ Cond.to_string cond ^ " then " ^ to_string cthen ^ " else "
        ^ to_string celse ^ " endif"
  ;;

  let rec to_short_string = function
    | Skip -> "skip"
    | Fail -> "fail"
    | Assert b -> "assert(" ^ Cond.to_string b ^ ")"
    | Define (x, e) -> x ^ " <- " ^ Expr.to_string e
    | Follow (c1, _) -> to_short_string c1
    | While (cond, _) -> "while " ^ Cond.to_string cond
    | If (cond, _, _) -> "if " ^ Cond.to_string cond
  ;;

  (* 
  let labels p =
    match p with
    | Skip | Fail -> []
    | Define _ -> [ to_short_string p ]
    | While (cond, _) -> [ Cond.to_string cond ]
    | If (cond, _, _) ->
        [ Cond.to_string cond; Cond.to_string @@ Cond.Not cond ]
    | _ -> [ "" ]
  ;; *)

  let rec equal a b =
    match (a, b) with
    | Skip, Skip | Fail, Fail -> true
    | Assert ab, Assert bb -> Cond.equal ab bb
    | Define (ax, ae), Define (bx, be) -> ax = bx && Expr.equal ae be
    | Follow (a1, a2), Follow (b1, b2) -> equal a1 b1 && equal a2 b2
    | While (ab, ap), While (bb, bp) -> Cond.equal ab bb && equal ap bp
    | If (ab, athen, aelse), If (bb, bthen, belse) ->
        Cond.equal ab bb && equal athen bthen && equal aelse belse
    | _ -> false
  ;;
end

module SymbolicStore = struct
  type t = (string, Expr.t) Hashtbl.t

  let to_string (s : t) =
    "["
    ^ Hashtbl.fold
        (fun x e acc -> x ^ ":=" ^ Expr.to_string e ^ ", " ^ acc)
        s "id]"
  ;;

  let to_Cond (s : t) =
    Hashtbl.fold
      (fun x e acc -> Cond.And (Cond.Eq (Expr.Var x, e), acc))
      s (Cond.Cte true)
  ;;
end

module State = struct
  type t = { path : Cond.t; store : SymbolicStore.t; prog : Stmt.t }

  let get_path s = try s.path with _ -> failwith "Path isn't set"
  let get_store s = try s.store with _ -> failwith "Store isn't set"
  let get_prog s = try s.prog with _ -> failwith "Prog isn't set"

  let to_string (sigma : t) =
    "<| " ^ Cond.to_string sigma.path ^ "\n | "
    ^ SymbolicStore.to_string sigma.store
    ^ "\n | " ^ Stmt.to_string sigma.prog ^ "\n |>"
  ;;

  let to_short_string (sigma : t) =
    "<| ϕ | "
    ^ SymbolicStore.to_string sigma.store
    ^ " | "
    ^ Stmt.to_short_string sigma.prog
    ^ " |>"
  ;;

  let rec is_stuck (sigma : t) =
    match sigma.prog with
    | Fail -> true
    | Follow (p, _) ->
        is_stuck { path = sigma.path; store = sigma.store; prog = p }
    | _ -> false
  ;;

  let to_Cond (sigma : t) =
    Cond.And (sigma.path, SymbolicStore.to_Cond sigma.store)
  ;;

  (* let same_program_point ((_, sa, pa) : t) ((_, sb, pb) : t) = *)
  let same_program_point (a : t) (b : t) =
    Hashtbl.fold
      (fun x ea acc ->
        match Hashtbl.find_opt b.store x with
        | None -> false
        | Some eb -> acc && Expr.equal ea eb)
      a.store
    @@ Hashtbl.fold
         (fun x eb acc ->
           match Hashtbl.find_opt a.store x with
           | None -> false
           | Some ea -> acc && Expr.equal ea eb)
         b.store
    @@ Stmt.equal a.prog b.prog
  ;;
end

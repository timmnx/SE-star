(* open Numbers *)
open Z3
open Z3.Solver

module Solver = struct
  let ctx = mk_context []
  let mk_expr (e : Syntax.Expr.t) = Syntax.Expr.to_expr ctx e
  let mk_form (e : Syntax.Cond.t) = Syntax.Cond.to_expr ctx e

  let sat (f : Syntax.Cond.t) =
    let slv = mk_simple_solver ctx in
    let pro = mk_form f in
    add slv [ pro ];
    match check slv [] with
    | SATISFIABLE -> Some true
    | UNSATISFIABLE -> Some false
    | UNKNOWN -> None
  ;;
end

module Optimize = struct
  type minmax = Max | Min

  let string_to_int s =
    int_of_string
      (String.fold_left
         (fun acc -> function
           | '(' | ')' | ' ' -> acc
           | c -> acc ^ Char.escaped c)
         "" s)
  ;;

  let optimize (m : minmax) (x : string) (e : Expr.expr) (ctx : context) =
    let x_expr = Arithmetic.Integer.mk_const_s ctx x in
    let opt = Optimize.mk_opt ctx in
    Optimize.add opt [ e ];
    let func =
      match m with
      | Max -> Optimize.maximize
      | Min -> Optimize.minimize
    in
    let _ = func opt x_expr in
    match Optimize.check opt with
    | SATISFIABLE -> (
        match Optimize.get_model opt with
        | None -> None
        | Some model ->
            let x_val = Option.get (Model.eval model x_expr true) in
            Some (string_to_int @@ Expr.to_string x_val))
    | _ -> begin
        let slv = mk_simple_solver ctx in
        add slv [ e ];
        match check slv [] with
        | SATISFIABLE -> begin
            match get_model slv with
            | None -> None
            | Some model -> begin
                let x_val = Option.get (Model.eval model x_expr true) in
                Some (string_to_int @@ Expr.to_string x_val)
              end
          end
        | _ -> None
      end
  ;;

  let to_print = ref 10

  let optimize_Cond (m : minmax) (x : string) (phi : Syntax.Cond.t) =
    let ctx0 = mk_context [ ("model", "true") ] in
    let _ = Arithmetic.Integer.mk_sort ctx0 in
    let e1 = Syntax.Cond.to_expr ctx0 phi in
    match optimize m x e1 ctx0 with
    | None ->
        if !to_print > 10 then (
          print_endline @@ Syntax.Cond.to_string phi;
          decr to_print);
        Numbers.Zint.Undef
    | Some i -> (
        let ctx1 = mk_context [] in
        let phi_c =
          match m with
          | Max -> Syntax.Cond.Lt (Int i, Var x)
          | Min -> Syntax.Cond.Lt (Var x, Int i)
        in
        let e2 = Syntax.Cond.to_expr ctx1 (And (phi_c, phi)) in
        match (optimize m x e2 ctx1, m) with
        | None, _ -> Numbers.Zint.Of i
        | Some _, Max -> Numbers.Zint.Infinity
        | Some _, Min -> Numbers.Zint.NegInfinity)
  ;;

  let maximize_Cond = optimize_Cond Max
  let minimize_Cond = optimize_Cond Min

  let domain (x : string) (phi: Syntax.Cond.t) : Numbers.Zintervals.t =
    let inf = minimize_Cond x phi in
    let sup = maximize_Cond x phi in
    { Numbers.Zintervals.inf; Numbers.Zintervals.sup }
  ;;

  let vars_domain (vars : Syntax.StringSet.t) (phi : Syntax.Cond.t) :
      (string, Numbers.Zintervals.t) Hashtbl.t =
    let h = Hashtbl.create 0 in
    Syntax.StringSet.iter (fun x -> Hashtbl.add h x @@ domain x phi) vars;
    h
  ;;
end

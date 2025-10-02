open Numbers
open Syntax
open Solver
module OptIntPrioqueue = PrioqueueFoncteur.Make (Zint)

module StateHashtabl = Hashtbl.Make (struct
    type t = State.t

    let equal = State.same_program_point
    let hash (Sigma (_, store, prog) : t) = Hashtbl.hash (store, prog)
  end)

(** https://gist.github.com/JBlond/2fea43a3049b38287e5e9cefc87b2124 *)
let nrm, red, grn, blu, yel, mag, cya =
  ( "\x1B[0;0m"
  , "\x1B[1;31m"
  , "\x1B[1;32m"
  , "\x1B[1;33m"
  , "\x1B[1;34m"
  , "\x1B[1;35m"
  , "\x1B[1;36m" )
;;

(**
  This function is used to introduce a
  fresh symbol when a [read ()] instruction
  is executed
*)
let read =
  let i = ref 0 in
  fun () ->
    incr i;
    !i
;;

let rec eval_Expr (e : Expr.t) (s : SymbolicStore.t) =
  let open Expr in
  match e with
  | Var x when Hashtbl.mem s x -> Hashtbl.find s x
  | Var _ | Int _ -> e
  | Add (e1, e2) ->
    (match eval_Expr e1 s, eval_Expr e2 s with
     | Int i1, Int i2 -> Int (i1 + i2)
     | e1', e2' -> Add (e1', e2'))
  | Sub (e1, e2) ->
    (match eval_Expr e1 s, eval_Expr e2 s with
     | Int i1, Int i2 -> Int (i1 - i2)
     | e1', e2' -> Sub (e1', e2'))
  | Read ->
    let x = "@" ^ string_of_int (read ()) in
    Var x
;;

let rec eval_Cond cond s =
  let open Cond in
  match cond with
  | Cte _ -> cond
  | Not b ->
    (match eval_Cond b s with
     | Cte b' -> Cte (not b')
     | Not b' -> b'
     | b' -> Not b')
  | Or (b1, b2) ->
    (match eval_Cond b1 s, eval_Cond b2 s with
     | _, Cte true | Cte true, _ -> Cte true
     | b', Cte false | Cte false, b' -> b'
     | b1', b2' -> Or (b1', b2'))
  | And (b1, b2) ->
    (match eval_Cond b1 s, eval_Cond b2 s with
     | _, Cte false | Cte false, _ -> Cte false
     | b', Cte true | Cte true, b' -> b'
     | b1', b2' -> And (b1', b2'))
  | Eq (e1, e2) ->
    (match eval_Expr e1 s, eval_Expr e2 s with
     | Int i, Int j -> Cte (i = j)
     | e1', e2' -> Eq (e1', e2'))
  | Le (e1, e2) ->
    (match eval_Expr e1 s, eval_Expr e2 s with
     | Int i, Int j -> Cte (i <= j)
     | e1', e2' -> Le (e1', e2'))
  | Lt (e1, e2) ->
    (match eval_Expr e1 s, eval_Expr e2 s with
     | Int i, Int j -> Cte (i < j)
     | e1', e2' -> Lt (e1', e2'))
  | Ge (e1, e2) ->
    (match eval_Expr e1 s, eval_Expr e2 s with
     | Int i, Int j -> Cte (i >= j)
     | e1', e2' -> Ge (e1', e2'))
  | Gt (e1, e2) ->
    (match eval_Expr e1 s, eval_Expr e2 s with
     | Int i, Int j -> Cte (i > j)
     | e1', e2' -> Gt (e1', e2'))
;;

(**
  @param State.t the state to expand

  @return
  - [(State.t * string) list] the list of all successors of entry state with the string that is the label of the transition
  - [bool] whether the evaluoator should continue to expand or not (continue until a branching node)
*)
let rec expand (Sigma (path, store, prog) : State.t) : (State.t * string) list * bool =
  let open Stmt in
  match prog with
  | Skip | Fail -> [], false
  | Follow (Skip, p) -> [ Sigma (path, store, p), "" ], true
  | Assert cond -> [ Sigma (path, store, If (cond, Skip, Fail)), "" ], false
  | Define (x, e) ->
    let s' = Hashtbl.copy store in
    Hashtbl.replace s' x (eval_Expr e store);
    [ Sigma (path, s', Skip), Stmt.to_short_string prog ], true
  | Follow (p1, p2) ->
    let follow, b = expand (Sigma (path, store, p1)) in
    ( List.map
        (fun (State.Sigma (path', store', prog'), l) ->
           State.Sigma (path', store', Follow (prog', p2)), l)
        follow
    , b )
  | While (cond, p) ->
    ( [ ( Sigma (eval_Cond (And (path, cond)) store, store, Follow (p, prog))
        , Cond.to_string cond )
      ; ( Sigma (eval_Cond (And (path, Not cond)) store, store, Skip)
        , Cond.to_string @@ Not cond )
      ]
    , false )
  | If (cond, pthen, pelse) ->
    ( [ Sigma (eval_Cond (And (path, cond)) store, store, pthen), Cond.to_string cond
      ; ( Sigma (eval_Cond (And (path, Not cond)) store, store, pelse)
        , Cond.to_string @@ Not cond )
      ]
    , false )
;;

let ask question () =
  let s = ref "" in
  while !s <> "y" && !s <> "n" do
    print_string (question ^ " [y/n] : ");
    s := "n" (* s := read_line () *)
  done;
  print_newline ();
  !s = "y"
;;

let yield prio d id (sigma : State.t) show file =
  let (Sigma (path, _, _)) = sigma in
  if show
  then (
    print_endline ("prio=" ^ Zint.to_string prio ^ " -> d=" ^ string_of_int d ^ ",");
    print_endline (State.to_string sigma));
  if State.is_stuck sigma
  then (
    match Solver.sat path with
    | None ->
      (* print_endline
          ("[" ^ yel ^ "~" ^ nrm ^ "] Potential bug (prio="
         ^ Zint.to_string prio ^ ", d=" ^ string_of_int d ^ "):"); *)
      (* print_state (State (phi, s, p)); *)
      ask "Show ?" ()
    | Some false ->
      (* print_endline
          ("[" ^ grn ^ "O" ^ nrm ^ "] Cleared bug (prio=" ^ Zint.to_string prio
         ^ ", d=" ^ string_of_int d ^ "):"); *)
      (* print_state (State (phi, s, p)); *)
      true
    | Some true ->
      (* print_endline
          ("[" ^ red ^ "X" ^ nrm ^ "] Bug (prio=" ^ Zint.to_string prio ^ ", d="
         ^ string_of_int d ^ "):");
        print_endline @@ State.to_string sigma; *)
      file := !file ^ string_of_int id ^ "[fillcolor=red; width=0.5]\n";
      ask "Continue ?" ())
  else true (* print_endline "pending..." *)
;;

let heuristique
      (Sigma (path, store, _) : State.t)
      (d_tphi : (string, Zintervals.t) Hashtbl.t)
      (vars : StringSet.t)
  =
  let phi' = Cond.And (path, SymbolicStore.to_Cond store) in
  let d_phi = Optimize.vars_domain vars phi' in
  let d =
    StringSet.fold
      (fun x acc ->
         let dx_phi = Hashtbl.find d_phi x in
         let dx_tphi = Hashtbl.find d_tphi x in
         let dx = Zintervals.dist dx_phi dx_tphi in
         (* Zint.add (Optimize.mult dx dx) acc) *)
         Zint.add dx acc)
      vars
      (Zint.Of 0)
  in
  match d with
  | Undef -> Zint.Infinity
  | _ -> d
;;

let heuristique_simple
      (Sigma (_, store, _) : State.t)
      (d_tphi : (string, Zintervals.t) Hashtbl.t)
      (vars : StringSet.t)
  =
  let d =
    StringSet.fold
      (fun x acc ->
         match Hashtbl.find_opt store x with
         | Some (Int i) ->
           Zint.add
             (Zintervals.dist
                { Zintervals.inf = Zint.Of i; Zintervals.sup = Zint.Of i }
                (Hashtbl.find d_tphi x))
             acc
         | _ -> acc)
      vars
      (Zint.Of 0)
  in
  d
;;

let _ = heuristique_simple, heuristique

let run (pq : (int * int * State.t) OptIntPrioqueue.t) (h : State.t -> Zint.t)
  : int * string
  =
  let limit = 1000000 in
  let round = ref 0 in
  let file = ref "graph{\nnode[shape=point]\n" in
  (* let visited : int StateHashtabl.t = StateHashtabl.create 0 in *)
  let add_to_dot father son =
    file := !file ^ string_of_int father ^ " -- " ^ string_of_int son ^ "\n"
  in
  let rec run_iter id d (expanded : (State.t * string) list) (continue : bool) =
    if continue
    then (
      let elt, _ = List.hd expanded in
      incr round;
      let expanded', b' = expand elt in
      add_to_dot id !round;
      run_iter !round d expanded' b')
    else
      List.iter
        (fun (elt, _) ->
           incr round;
           let prio_ = Zint.add (Zint.Of (d + 1)) (h elt) in
           OptIntPrioqueue.push prio_ (d + 1, !round, elt) pq;
           add_to_dot id !round)
        expanded
  in
  let rec aux () =
    if OptIntPrioqueue.is_empty pq || !round = limit
    then
      OptIntPrioqueue.print
        (fun (d, _, sigma) ->
           print_int d;
           print_string (" : " ^ SymbolicStore.to_string @@ State.get_store sigma))
        pq
    else (
      let prio, (d, id, sigma) = OptIntPrioqueue.pop pq in
      if yield prio d id sigma false file
      then (
        let expanded, b = expand sigma in
        run_iter id d expanded b;
        aux ()))
  in
  aux ();
  !round, !file
;;

let run_mem (pq : (int * int * State.t) OptIntPrioqueue.t) (h : State.t -> Zint.t)
  : int * string
  =
  let limit = 1000000 in
  let round = ref 0 in
  let file = ref "graph{\nnode[shape=point]\n" in
  let visited : int StateHashtabl.t = StateHashtabl.create 0 in
  let add_to_dot father son =
    file := !file ^ string_of_int father ^ " -- " ^ string_of_int son ^ "\n"
  in
  let rec run_iter id d (expanded : (State.t * string) list) (continue : bool) =
    if continue
    then (
      let elt, _ = List.hd expanded in
      match StateHashtabl.find_opt visited elt with
      | Some d_old when d_old <= d -> ()
      | _ ->
        incr round;
        StateHashtabl.replace visited elt (d + 1);
        let expanded', b' = expand elt in
        add_to_dot id !round;
        run_iter !round d expanded' b')
    else
      List.iter
        (fun (elt, _) ->
           match StateHashtabl.find_opt visited elt with
           | Some d_old when d_old <= d -> ()
           | _ ->
             incr round;
             StateHashtabl.replace visited elt (d + 1);
             let prio_ = Zint.add (Zint.Of (d + 1)) (h elt) in
             OptIntPrioqueue.push prio_ (d + 1, !round, elt) pq;
             add_to_dot id !round)
        expanded
  in
  let rec aux () =
    if OptIntPrioqueue.is_empty pq || !round = limit
    then
      OptIntPrioqueue.print
        (fun (d, _, sigma) ->
           print_int d;
           print_string (" : " ^ SymbolicStore.to_string @@ State.get_store sigma))
        pq
    else (
      let prio, (d, id, sigma) = OptIntPrioqueue.pop pq in
      if yield prio d id sigma false file
      then (
        let expanded, b = expand sigma in
        run_iter id d expanded b;
        aux ()))
  in
  aux ();
  !round, !file
;;

(**
  - [p] : the program to test
  - [tphi] : the targeted formula
*)
let find_bugs p (tphi : Cond.t) =
  let vars_tphi = Cond.variables tphi in
  let d_tphi = Optimize.vars_domain vars_tphi tphi in
  Hashtbl.iter (fun x d -> print_endline (x ^ " ∈ " ^ Zintervals.to_string d)) d_tphi;
  print_newline ();
  let h = fun s -> heuristique s d_tphi vars_tphi in
  let pq = OptIntPrioqueue.create () in
  let sigma = State.Sigma (Cte true, Hashtbl.create 0, p) in
  OptIntPrioqueue.push (h sigma) (0, 0, sigma) pq;
  let i, s = run pq h in
  i, s, pq.max_length
;;

let find_bugs_mem p (tphi : Cond.t) =
  let vars_tphi = Cond.variables tphi in
  let d_tphi = Optimize.vars_domain vars_tphi tphi in
  Hashtbl.iter (fun x d -> print_endline (x ^ " ∈ " ^ Zintervals.to_string d)) d_tphi;
  (* let store_test = Hashtbl.create 0 in
  Hashtbl.add "k1"
  let test = {State.path = Cte true; State.store = } in *)
  print_newline ();
  let h = fun s -> heuristique s d_tphi vars_tphi in
  let pq = OptIntPrioqueue.create () in
  let sigma = State.Sigma (Cte true, Hashtbl.create 0, p) in
  OptIntPrioqueue.push (h sigma) (0, 0, sigma) pq;
  run_mem pq h
;;

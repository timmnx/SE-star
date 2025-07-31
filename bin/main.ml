open Se_star
open Se_star.Syntax.Expr
open Se_star.Syntax.Cond
open Se_star.Syntax.Stmt
open Se_star.Numbers

module Examples = struct
  type t =
  { name: string
  ; target: Syntax.Cond.t
  ; prog: Syntax.Stmt.t
  }

  let phard k =
    { name = "phard" ^ string_of_int k
    ; target = And (Eq (Var "k", Int k), Le (Var "n", Int 0))
    ; prog = Follow
      ( Define ("n", Read),
        Follow
          ( Define ("k", Int 0),
            While
              ( Not (Eq (Var "n", Int 0)),
                Follow
                  ( If
                      ( Not (Le (Var "n", Int 0)),
                        Follow
                          ( Define ("k", Sub (Var "k", Int 1)),
                            If (Eq (Var "k", Int k), Fail, Skip) ),
                        Define ("k", Add (Var "k", Int 1)) ),
                    Define ("n", Read) ) ) ) )
    }
  ;;

  let phard_ssa k =
    { name = "phard_ssa" ^ string_of_int k
    ; target = And
           ( And (Eq (Var "k2", Int k), Eq (Var "k1", Int (k + 1))),
             Le (Var "n", Int 0) )
    ; prog = Follow
      ( Define ("n", Read),
        Follow
          ( Define ("k1", Int 0),
            While
              ( Not (Eq (Var "n", Int 0)),
                Follow
                  ( If
                      ( Not (Le (Var "n", Int 0)),
                        Follow
                          ( Define ("k2", Sub (Var "k1", Int 1)),
                            If (Eq (Var "k2", Int k), Fail, Skip) ),
                        Define ("k2", Add (Var "k1", Int 1)) ),
                    Follow (Define ("k1", Var "k2"), Define ("n", Read)) ) ) )
      )
    }
  ;;

  let _ = phard, phard_ssa

  type res =
  { name: string
  ; bfs_n: int
  ; bfs_pp: int
  ; bfs_t: float
  ; astar_n: int
  ; astar_pp: int
  ; astar_t: float
  }

  let test (ex : t) : res =
    print_endline ("Testing " ^ ex.name);
    let t0 = Sys.time () in
    let bfs_n, _, bfs_pp = Aux.find_bugs ex.prog (Cte true) in
    let t1 = Sys.time () in
    let astar_n, _, astar_pp = Aux.find_bugs ex.prog ex.target in
    let t2 = Sys.time () in
    { name = ex.name;
      bfs_n = bfs_n;      bfs_pp = bfs_pp;      bfs_t = t1 -. t0;
      astar_n = astar_n;  astar_pp = astar_pp;  astar_t = t2 -. t1
    }
  ;;

  let res_to_csv (r : res) : string =
    Printf.sprintf "%s;%d;%d;%.3f;%d;%d;%.3f"
    r.name r.bfs_n r.bfs_pp r.bfs_t r.astar_n r.astar_pp r.astar_t
  ;;
end

module PQueue = PrioqueueFoncteur.Make (Zint)

let () =
  let csv = "name;bfs_n;bfs_pp;bfs_t;astar_n;astar_pp;astar_t" in
  let progs =
    Examples.phard 2 :: Examples.phard_ssa 2 ::
    Examples.phard 4 :: Examples.phard_ssa 4 ::
    (* Examples.phard 6 :: Examples.phard_ssa 6 :: *)
    Examples.phard 8 :: Examples.phard_ssa 8 ::
    (* Examples.phard 10 :: Examples.phard_ssa 10 :: *)
    (* Examples.phard 12 :: Examples.phard_ssa 12 :: *)
    Examples.phard 16 :: Examples.phard_ssa 16 ::
    [] in
  let res = List.map (fun x -> Examples.res_to_csv @@ Examples.test x) progs in
  let file = open_out "data.csv" in
  Printf.fprintf file "%s" (List.fold_left (fun acc x -> acc ^ "\n" ^ x) csv res);
  close_out file;
  ()
;;

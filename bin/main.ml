open Se_star
open Se_star.Syntax.Expr
open Se_star.Syntax.Cond
open Se_star.Syntax.Stmt
open Se_star.Numbers

module Examples = struct
  let p1 =
    Follow
      ( Follow (Define ("a", Int 1), Define ("b", Var "a")),
        Follow (Define ("c", Var "c"), If (Eq (Var "c", Var "a"), Fail, Skip))
      )
  ;;

  let p2 =
    Follow
      ( Define ("k", Int 0),
        Follow
          ( While
              ( Le (Var "k", Int 10),
                Follow (Define ("n", Read), Define ("k", Add (Var "k", Int 1)))
              ),
            Follow
              ( Define ("k", Add (Var "k", Var "n")),
                Assert (Not (Eq (Var "k", Int 42))) ) ) )
  ;;

  let phard =
    Follow
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
                            If (Eq (Var "k", Int 42), Fail, Skip) ),
                        Define ("k", Add (Var "k", Int 1)) ),
                    Define ("n", Read) ) ) ) )
  ;;

  let phard_ssa k =
    Follow
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
  ;;

  let _ = (p1, p2, phard, phard_ssa)
end

module PQueue = PrioqueueFoncteur.Make (Zint)

let () =
  let n = 14 in
  let csv = ref "k;bfs(n);a*(n);bfs(t);a*(t);bfs_mem(n);a*_mem(n);bfs_mem(t);a*_mem(t)\n" in
  for k = 1 to n do
    let t0 = Sys.time () in
    let an, _ =
      Aux.find_bugs (Examples.phard_ssa k)
        (And
           ( And (Eq (Var "k2", Int k), Eq (Var "k1", Int (k + 1))),
             Le (Var "n", Int 0) ))
    in
    let t1 = Sys.time () in
    let bn, _ = Aux.find_bugs (Examples.phard_ssa k) (Cte true) in
    let t2 = Sys.time () in
    let anm, _ =
      Aux.find_bugs_mem (Examples.phard_ssa k)
        (And
           ( And (Eq (Var "k2", Int k), Eq (Var "k1", Int (k + 1))),
             Le (Var "n", Int 0) ))
    in
    let t3 = Sys.time () in
    let bnm, _ = Aux.find_bugs_mem (Examples.phard_ssa k) (Cte true) in
    let t4 = Sys.time () in
    csv :=
      !csv ^ string_of_int k ^ ";" ^ string_of_int bn ^ ";" ^ string_of_int an
      ^ ";"
      ^ string_of_float (t2 -. t1)
      ^ ";"
      ^ string_of_float (t1 -. t0)
      ^ ";"
      ^ string_of_int bnm ^ ";" ^ string_of_int anm
      ^ ";"
      ^ string_of_float (t4 -. t3)
      ^ ";"
      ^ string_of_float (t3 -. t2)
      ^ "\n"
  done;
  let file = open_out "data.csv" in
  Printf.fprintf file "%s" !csv;
  close_out file;
  ()
;;

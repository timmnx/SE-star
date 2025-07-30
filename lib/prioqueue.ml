type 'a t = { mutable length : int; mutable data : (int * 'a) option array }

let create () = { length = 0; data = [| None |] }
let is_empty pq = pq.length = 0

let print p pq =
  print_endline ("{ length = " ^ string_of_int pq.length);
  print_string "; data = [| ";
  Array.iter
    (function
      | None -> print_string "# "
      | Some (i, x) ->
          print_string ("(" ^ string_of_int i ^ ", ");
          p x;
          print_string ") ")
    pq.data;
  print_endline "|]";
  print_endline "}"
;;

let ratio pq =
  let len = pq.length in
  let size = Array.length pq.data in
  if len = size then 1 else if len < size / 2 then -1 else 0
;;

let resize pq =
  match ratio pq with
  | 1 -> pq.data <- Array.concat [ pq.data; Array.make pq.length None ]
  | -1 -> pq.data <- Array.sub pq.data 0 (Array.length pq.data / 2)
  | _ -> ()
;;

let rec up s_index s_prio s_elt pq : unit =
  let f_index = (s_index - 1) / 2 in
  (* if pq.data.(f_index) = None then failwith "Pas normal"; *)
  let f_prio, f_elt = Option.get pq.data.(f_index) in
  if s_prio < f_prio then (
    pq.data.(f_index) <- Some (s_prio, s_elt);
    pq.data.(s_index) <- Some (f_prio, f_elt);
    up f_index s_prio s_elt pq)
;;

let push prio elt pq =
  resize pq;
  pq.data.(pq.length) <- Some (prio, elt);
  pq.length <- pq.length + 1;
  up (pq.length - 1) prio elt pq
;;

let rec down f_index f_prio f_elt pq : unit =
  (* print_endline (string_of_int f_index ^" "^ string_of_int f_prio); *)
  (* print print_int pq; *)
  let lambda = fun i -> if i < pq.length then pq.data.(i) else None in
  let l_index = (2 * f_index) + 1 in
  let r_index = (2 * f_index) + 2 in
  let l, r = (lambda l_index, lambda r_index) in
  let s_prio_elt =
    match (l, r) with
    | None, None -> None
    | Some (l_prio, l_elt), None -> Some (l_index, l_prio, l_elt)
    | None, Some (r_prio, r_elt) -> Some (r_index, r_prio, r_elt)
    | Some (l_prio, l_elt), Some (r_prio, r_elt) ->
        if l_prio <= r_prio then Some (l_index, l_prio, l_elt)
        else Some (r_index, r_prio, r_elt)
  in
  match s_prio_elt with
  | None -> pq.data.(f_index) <- Some (f_prio, f_elt)
  | Some (s_index, s_prio, s_elt) ->
      if f_prio <= s_prio then ()
      else (
        pq.data.(f_index) <- Some (s_prio, s_elt);
        pq.data.(s_index) <- Some (f_prio, f_elt);
        down s_index f_prio f_elt pq)
;;

let pop pq =
  if pq.length = 0 then failwith "Cannot pop on empty!";
  (* if pq.data.(0) = None then failwith "Pas normal"; *)
  let prio, elt = Option.get pq.data.(0) in
  (* print print_int pq; *)
  pq.length <- pq.length - 1;
  (* print print_int pq; *)
  pq.data.(0) <- pq.data.(pq.length);
  (* print print_int pq; *)
  pq.data.(pq.length) <- None;
  (* print print_int pq; *)
  (* print_endline "Done"; *)
  if pq.length > 1 then (
    let last_prio, last_elt = Option.get pq.data.(0) in
    (* if pq.data.(pq.length-1) = None then failwith "Pas normal"; *)
    down 0 last_prio last_elt pq;
    resize pq);
  (prio, elt)
;;

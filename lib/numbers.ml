module Zint = struct
  type t = Of of int | Infinity | NegInfinity | Undef

  let to_string = function
    | Of i -> string_of_int i
    | Infinity -> "+oo"
    | NegInfinity -> "-oo"
    | Undef -> "undef"
  ;;

  let add a b =
    match (a, b) with
    | Of x, Of y -> Of (x + y)
    | Infinity, Of _ | Of _, Infinity -> Infinity
    | NegInfinity, Of _ | Of _, NegInfinity -> NegInfinity
    | _, _ -> Undef
  ;;

  let sub a b =
    match (a, b) with
    | Of x, Of y -> Of (x - y)
    | Infinity, Of _ | Of _, NegInfinity -> Infinity
    | NegInfinity, Of _ | Of _, Infinity -> NegInfinity
    | _, _ -> Undef
  ;;

  let absval = function
    | Of i -> Of (abs i)
    | Infinity | NegInfinity -> Infinity
    | Undef -> Undef
  ;;

  let compare a b =
    match (a, b) with
    | Of i, Of j -> compare i j
    | Infinity, Infinity -> 0
    | Infinity, _ -> 1
    | _, Infinity -> -1
    | NegInfinity, NegInfinity -> 0
    | NegInfinity, Of _ -> -1
    | NegInfinity, Undef -> 1
    | Of _, NegInfinity -> 1
    | Undef, NegInfinity -> -1
    | Undef, Undef -> -2
    | Undef, _ -> -1
    | _, Undef -> 1
  ;;
end

module Zintervals = struct
  type t = { inf : Zint.t; sup : Zint.t }

  let to_string s =
    "[" ^ Zint.to_string s.inf ^ "; " ^ Zint.to_string s.sup ^ "]"
  ;;

  let is_empty s =
    match (s.inf, s.sup) with
    | Zint.Infinity, Zint.NegInfinity -> true
    | Zint.Infinity, Zint.Infinity -> true
    | Zint.NegInfinity, Zint.NegInfinity -> true
    | Zint.Undef, _ | _, Zint.Undef -> true
    | _ -> false
  ;;

  let distinct a b =
    is_empty a || is_empty b
    || List.mem Zint.Undef [ a.inf; a.sup; b.inf; b.sup ]
    || Zint.compare a.sup b.inf = -1
    || Zint.compare b.sup a.inf = -1
  ;;

  let dist a b =
    if distinct a b then begin
      match
        ( Zint.absval @@ Zint.sub a.sup b.inf,
          Zint.absval @@ Zint.sub b.sup a.inf )
      with
      | Zint.Of i, Zint.Of j -> Zint.Of (min i j)
      | Zint.Of i, _ | _, Zint.Of i -> Zint.Of i
      | Zint.Infinity, Zint.Infinity -> Zint.Infinity
      | _ -> Zint.Undef
    end
    else Zint.Of 0
  ;;
end

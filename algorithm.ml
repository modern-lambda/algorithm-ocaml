(*
   val factorial : int -> int = <fun>
   val length : 'a list -> int = <fun>
   val take : int -> 'a list -> 'a list = <fun>
   val drop : int -> 'a list -> 'a list = <fun>
   val sort_insert : 'a -> 'a list -> 'a list = <fun>
   val sort : 'a list -> 'a list = <fun>
   val merge : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list = <fun>
   val split : 'a list -> 'a list -> 'a list -> 'a list * 'a list = <fun>
   val mergesort : ('a -> 'a -> bool) -> 'a list -> 'a list = <fun>
   val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
*)
let rec factorial a =
  match a with
  | 1 -> 1
  | _ -> a * factorial (a - 1)

let rec length l =
  match l with
  | [] -> 0
  | _ :: t -> 1 + length t

let rec take n l =
  match n with
  | 0 -> []
  | _ -> match l with
    | [] -> []
    | h :: t -> h :: take (n - 1) t

let rec drop n l =
  match n with
  | 0 -> l
  | _ -> match l with
    | [] -> []
    | h :: t -> drop (n - 1) t

let rec sort_insert x l =
  match l with
  | [] -> [x]
  | h :: t ->
    if x <= h
    then x :: h :: t
    else h :: sort_insert x t

let rec sort l =
  match l with
  | [] -> []
  | h :: t -> sort_insert h (sort t)

let rec merge cmp x y =
  match (x, y) with
  | ([], _) -> y
  | (_, []) -> x
  | (h1 :: t1, h2 :: t2) ->
    if cmp h2 h1
    then h1 :: (merge cmp t1 y)
    else h2 :: (merge cmp x t2)

and split x y z =
  match x with
  | [] -> (y, z)
  | x :: resto -> split resto z (x :: y)

and mergesort cmp x =
  match x with
  | ([] | _ :: []) -> x
  | _ -> let (pri, seg) = split x [] []
    in (merge cmp (mergesort cmp pri) (mergesort cmp seg))

let rec map f l =
  match l with
  | [] -> []
  | h::t -> f h :: map f t

(* factorial : int -> int *)
let rec factorial a =
  match a with
  | 1 -> 1
  | _ -> a * factorial (a - 1)

(* isvowel : int -> int *)
let rec isvowel c =
  c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u'


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


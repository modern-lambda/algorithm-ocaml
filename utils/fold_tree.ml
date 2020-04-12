(* api
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>
val all : bool list -> bool = <fun>
val any : bool list -> bool = <fun>
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
val copy : 'a list -> 'a list = <fun>
val append : 'a list -> 'a list -> 'a list = <fun>
val split : ('a * 'b) list -> 'a list * 'b list = <fun>
val concat : 'a list list -> 'a list = <fun>
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
val fold_tree : ('a -> 'b -> 'b -> 'b) -> 'b -> 'a tree -> 'b = <fun>
val tree_size : 'a tree -> int = <fun>
val tree_sum : int tree -> int = <fun>
val tree_preorder : 'a tree -> 'a list = <fun>
val tree_inorder : 'a tree -> 'a list = <fun>
val tree_postorder : 'a tree -> 'a list = <fun>
*)

let rec fold_left f a l =
  match l with
    [] -> a
  | h::t -> fold_left f (f a h) t

let rec fold_right f l a =
  match l with
    [] -> a
  | h::t -> f h (fold_right f t a)

let all l = fold_left ( && ) true l

let any l = fold_left ( || ) false l

let map f l =
  fold_right (fun e a -> f e :: a) l []

let copy l =
  fold_right (fun e a -> e :: a) l []

let append x y =
  fold_right (fun e a -> e :: a) x y

let split l =
  fold_right
    (fun (x, y) (xs, ys) -> (x :: xs, y :: ys))
    l
    ([], [])

let concat l = fold_left ( @ ) [] l

type 'a tree =
    Lf
  | Br of 'a * 'a tree * 'a tree

let rec fold_tree f e t =
  match t with
    Lf -> e
  | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let tree_size t =
  fold_tree (fun _ l r -> 1 + 1 + r) 0 t

let tree_sum t =
  fold_tree (fun x l r -> x + 1 + r) 0 t

let tree_preorder t  = fold_tree (fun x l r -> [x] @ l @ r) [] t
let tree_inorder t   = fold_tree (fun x l r -> l @ [x] @ r) [] t
let tree_postorder t = fold_tree (fun x l r -> l @ r @ [x]) [] t

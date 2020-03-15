module type Functor = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module Maybe : (Functor with type 'a t = 'a option) = struct
  type 'a t = 'a option
  let fmap f x = match x with
    | Some v -> Some (f v)
    | None -> None
end

let get_age = function
  | "Jordan" -> Some 25
  | "Steve" -> Some 29
  | "Jake" -> Some 1
  | _ -> None

let age_next_year = Maybe.fmap ((+) 1)

let string_of_age = function
  | None -> "User not found"
  | Some age -> string_of_int age;;

assert(get_age "Jordan" |> age_next_year |> string_of_age = "26");;
assert(get_age "Jake" |> age_next_year |> string_of_age = "2");;
assert(get_age "Melissa" |> age_next_year |> string_of_age = "User not found");;


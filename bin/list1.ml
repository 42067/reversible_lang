type 'a t = ( :: ) of 'a * 'a list

let singleton x = x :: []
let map f (x :: xs) = f x :: List.map f xs
let mapi f (x :: xs) = f 0 x :: List.mapi (fun i -> i + 1 |> f) xs
let fold_left f a (x :: xs) = List.fold_left f (f a x) xs
let fold_right f (x :: xs) a = f x (List.fold_right f xs a)
let to_list (x :: xs) = List.(x :: xs)
let eq_length (_ :: xs) (_ :: ys) = List.compare_lengths xs ys = 0
let combine (x :: xs) (y :: ys) = (x, y) :: List.combine xs ys

let of_list = function
  | List.(x :: xs) -> x :: xs
  | _ -> failwith "list is empty"

let combine_opt (x :: xs) (y :: ys) =
  let open Util in
  let+ zs = combine_opt xs ys in
  (x, y) :: zs

let split ((x, y) :: zs) =
  let xs, ys = List.split zs in
  (x :: xs, y :: ys)

let unwrap_opt = function
  | Some x :: xs ->
      let open Util in
      let+ xs = unwrap_opt xs in
      x :: xs
  | None :: _ -> None

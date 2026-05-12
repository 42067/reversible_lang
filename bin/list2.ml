type 'a t = ( :: ) of 'a * 'a List1.t

let map f (x :: xs) = f x :: List1.map f xs
let mapi f (x :: xs) = f 0 x :: List1.mapi (fun i -> i + 1 |> f) xs
let fold_left f a (x :: xs) = List1.fold_left f (f a x) xs
let fold_right f (x :: xs) a = f x (List1.fold_right f xs a)
let to_list (x :: xs) = List.(x :: List1.to_list xs)
let to_list1 (x :: xs) = List1.(x :: List1.to_list xs)
let eq_length (_ :: xs) (_ :: ys) = List1.eq_length xs ys
let combine (x :: xs) (y :: ys) = (x, y) :: List1.combine xs ys

let of_list = function
  | List.(x :: y :: ys) -> x :: List1.(y :: ys)
  | _ -> failwith "list is empty"

let combine_opt (x :: xs) (y :: ys) =
  let open Util in
  let+ zs = List1.combine_opt xs ys in
  (x, y) :: zs

let split ((x, y) :: zs) =
  let xs, ys = List1.split zs in
  (x :: xs, y :: ys)

let unwrap_opt = function
  | Some x :: xs ->
      let open Util in
      let+ xs = List1.unwrap_opt xs in
      x :: xs
  | None :: _ -> None

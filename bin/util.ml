module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type generator = int ref

let ( let+ ) x f = Option.map f x
let ( let* ) = Option.bind
let ( let++ ) x f = Result.map f x
let ( let** ) = Result.bind
let swap (x, y) = (y, x)
let combine_opt a b = try Some (List.combine a b) with _ -> None

let union ~weak ~strong =
  let merger _ _ y = Some y in
  IntMap.union merger weak strong

let rec unwrap_opt = function
  | [] -> Some []
  | Some x :: xs ->
      let+ xs = unwrap_opt xs in
      x :: xs
  | None :: _ -> None

let create () = ref 0

let fresh gen =
  let i = !gen in
  incr gen;
  i

let rec alphabet n =
  if n < 0 then failwith "negative"
  else if n < 26 then n + Char.code 'a' |> Char.chr |> String.make 1
  else alphabet ((n / 26) - 1) ^ alphabet (n mod 26)

let rec all_ok l =
  List.fold_left
    (fun acc -> function Ok _ -> acc | Error e -> Error e)
    (Ok ()) l

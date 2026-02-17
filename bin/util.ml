module StrMap = Map.Make (String)
module StrSet = Set.Make (String)

type 'a myresult = ('a, string) Result.t

let ( let* ) : 'a option -> ('a -> 'b option) -> 'b option = Option.bind
let ( let+ ) (x : 'a option) (f : 'a -> 'b) : 'b option = Option.map f x

let ( let** ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result =
  Result.bind

let ( let++ ) (x : ('a, 'b) result) (f : 'a -> 'c) : ('c, 'b) result =
  Result.map f x

let rec bind_all : 'a myresult list -> 'a list myresult = function
  | Error e :: _ -> Error e
  | Ok x :: tl ->
      let++ tl = bind_all tl in
      x :: tl
  | [] -> Ok []

let rec combine (l : 'a list) (r : 'b list) : ('a * 'b) list option =
  match (l, r) with
  | hdl :: tll, hdr :: tlr ->
      let+ tl = combine tll tlr in
      (hdl, hdr) :: tl
  | [], [] -> Some []
  | _ -> None

let extend (what : 'a StrMap.t) (list : (string * 'a) list) : 'a StrMap.t =
  let folder acc (key, value) = StrMap.add key value acc in
  List.fold_left folder what list

let is_variable (value : string) : bool =
  0 < String.length value
  &&
  let first = String.get value 0 |> Char.code in
  Char.code 'a' <= first && first <= Char.code 'z'

let is_type_variable : string -> bool = String.starts_with ~prefix:"'"

let rec for_all_pairs (f : 'a -> 'a -> unit myresult) : 'a list -> unit myresult
    = function
  | [] -> Ok ()
  | hd :: tl ->
      let** _ = List.map (f hd) tl |> bind_all in
      for_all_pairs f tl

let union_nodup (l : 'a StrMap.t) (r : 'a StrMap.t) : 'a StrMap.t myresult =
  let msg = ref None in
  let merger key _ y =
    msg := Some (key ^ " already exists");
    Some y
  in
  let merged = StrMap.union merger l r in
  match !msg with None -> Ok merged | Some msg -> Error msg

let union ~(weak : 'a StrMap.t) ~(strong : 'a StrMap.t) : 'a StrMap.t =
  let merger _ _ y = Some y in
  StrMap.union merger weak strong

let union_list (l : 'a StrMap.t list) : 'a StrMap.t =
  List.fold_left (fun weak strong -> union ~weak ~strong) StrMap.empty l

let show_listlike (f : 'a -> string) ~(left : string) ~(delim : string)
    ~(right : string) : 'a list -> string = function
  | [] -> left ^ right
  | hd :: tl ->
      let init = left ^ f hd in
      List.fold_left (fun acc a -> acc ^ delim ^ f a) init tl ^ right

let show_tuple (f : 'a -> string) : 'a list -> string =
  show_listlike f ~left:"(" ~delim:", " ~right:")"

let show_list (f : 'a -> string) : 'a list -> string =
  show_listlike f ~left:"[" ~delim:"; " ~right:"]"

let boldred (value : string) : string = "\x1b[1;31m" ^ value ^ "\x1b[0m"
let boldpurple (value : string) : string = "\x1b[1;35m" ^ value ^ "\x1b[0m"

let find_res (what : string) (map : 'a StrMap.t) : 'a myresult =
  StrMap.find_opt what map
  |> Option.to_result ~none:(what ^ " was not found in current context")

let rec repeat (what : string) (count : int) : string =
  if count < 1 then "" else what ^ repeat what (count - 1)

let rec chars_of_int (n : int) : string =
  if n < 0 then "wtf"
  else if n < 26 then n + Char.code 'a' |> Char.chr |> String.make 1
  else chars_of_int ((n / 26) - 1) ^ chars_of_int (n mod 26)

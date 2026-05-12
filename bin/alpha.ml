module StrMap = Hashtbl.Make (String)
module IntMap = Hashtbl.Make (Int)

type t = Util.generator * int StrMap.t
type u = Util.generator * int IntMap.t
type t_alphabet = Util.generator * string IntMap.t

let create gen = (gen, StrMap.create 0)
let create_u gen = (gen, IntMap.create 0)
let create_alphabet () = (Util.create (), IntMap.create 0)

let destruct (gen, map) =
  let map' =
    StrMap.to_seq map |> Seq.map (fun (s, i) -> (i, s)) |> IntMap.of_seq
  in
  (gen, map')

let destruct_u = Fun.id
let destruct_alphabet (_gen, map) = IntMap.to_seq map |> Util.IntMap.of_seq

let get (gen, map) x =
  match StrMap.find_opt map x with
  | Some v -> v
  | None ->
      let fresh = Util.fresh gen in
      StrMap.add map x fresh;
      fresh

let get_alphabet (gen, map) i =
  match IntMap.find_opt map i with
  | Some v -> v
  | None ->
      let fresh = Util.fresh gen |> Util.alphabet in
      IntMap.add map i fresh;
      fresh

let get_alphabet_upper (gen, map) i =
  match IntMap.find_opt map i with
  | Some v -> v
  | None ->
      let fresh = Util.fresh gen |> Util.alphabet |> String.uppercase_ascii in
      IntMap.add map i fresh;
      fresh

let fresh (gen, map) x =
  let fresh = Util.fresh gen in
  IntMap.add map fresh x;
  fresh

let compose_map map map' =
  IntMap.iter
    (fun k v ->
      match IntMap.find_opt map v with
      | Some found -> IntMap.replace map k found
      | None -> ())
    map'

let convert map = IntMap.to_seq map |> Util.IntMap.of_seq

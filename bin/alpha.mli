module StrMap : module type of Hashtbl.Make (String)
module IntMap : module type of Hashtbl.Make (Int)

type t
type u
type t_alphabet

val create : Util.generator -> t
val create_u : Util.generator -> u
val create_alphabet : unit -> t_alphabet
val destruct : t -> Util.generator * string IntMap.t
val destruct_u : u -> Util.generator * int IntMap.t
val destruct_alphabet : t_alphabet -> string Util.IntMap.t
val get : t -> string -> int
val get_alphabet : t_alphabet -> int -> string
val get_alphabet_upper : t_alphabet -> int -> string
val fresh : u -> int -> int
val compose_map : string IntMap.t -> int IntMap.t -> unit
val convert : 'a IntMap.t -> 'a Util.IntMap.t

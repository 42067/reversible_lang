module IntMap : module type of Map.Make (Int)
module IntSet : module type of Set.Make (Int)

type generator

val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( let++ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
val ( let** ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val swap : 'a * 'b -> 'b * 'a
val combine_opt : 'a list -> 'b list -> ('a * 'b) list option
val union : weak:'a IntMap.t -> strong:'a IntMap.t -> 'a IntMap.t
val unwrap_opt : 'a option list -> 'a list option
val create : unit -> generator
val fresh : generator -> int
val alphabet : int -> string
val all_ok : ('a, 'e) result list -> (unit, 'e) result

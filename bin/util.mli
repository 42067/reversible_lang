module StrMap : module type of Map.Make (String)
module StrSet : module type of Set.Make (String)

type 'a myresult = ('a, string) Result.t

val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
val ( let** ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val ( let++ ) : ('a, 'b) result -> ('a -> 'c) -> ('c, 'b) result
val bind_all : 'a myresult list -> 'a list myresult
val combine : 'a list -> 'b list -> ('a * 'b) list option
val extend : 'a StrMap.t -> (string * 'a) list -> 'a StrMap.t
val is_variable : string -> bool
val is_type_variable : string -> bool
val for_all_pairs : ('a -> 'a -> unit myresult) -> 'a list -> unit myresult
val union_nodup : 'a StrMap.t -> 'a StrMap.t -> 'a StrMap.t myresult
val union : weak:'a StrMap.t -> strong:'a StrMap.t -> 'a StrMap.t
val union_list : 'a StrMap.t list -> 'a StrMap.t

val show_listlike :
  ('a -> string) ->
  left:string ->
  delim:string ->
  right:string ->
  'a list ->
  string

val show_tuple : ('a -> string) -> 'a list -> string
val show_list : ('a -> string) -> 'a list -> string
val boldred : string -> string
val boldpurple : string -> string
val find_res : string -> 'a StrMap.t -> 'a myresult
val repeat : string -> int -> string
val chars_of_int : int -> string

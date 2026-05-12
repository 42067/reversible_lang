type 'a t = ( :: ) of 'a * 'a List1.t

val map : ('a -> 'b) -> 'a t -> 'b t
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val to_list : 'a t -> 'a list
val to_list1 : 'a t -> 'a List1.t
val eq_length : 'a t -> 'b t -> bool
val combine : 'a t -> 'b t -> ('a * 'b) t
val of_list : 'a list -> 'a t
val combine_opt : 'a t -> 'b t -> ('a * 'b) t option
val split : ('a * 'b) t -> 'a t * 'b t
val unwrap_opt : 'a option t -> 'a t option

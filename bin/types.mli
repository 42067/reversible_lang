type base =
  | BaseUnit
  | BaseIdent of int
  | BaseVar of int
  | BaseProd of base List2.t
  | BaseApp of base List1.t * int

type iso =
  | IsoBiArrow of base * base
  | IsoArrow of iso * iso
  | IsoVar of int
  | IsoInv of iso

type 'a subst = 'a * int

val is_fv_base : int -> base -> bool
val is_fv_iso : int -> iso -> bool
val invert : iso -> iso
val fv_base : base -> Util.IntSet.t
val fv_iso : iso -> Util.IntSet.t
val subst_base : base subst -> base -> base
val subst_iso : iso subst -> iso -> iso
val subst_base_iso : base subst -> iso -> iso
val subst_base_bulk : base subst list -> base -> base
val subst_iso_bulk : iso subst list -> base subst list -> iso -> iso
val pp_base : string Util.IntMap.t -> Format.formatter -> base -> unit
val pp_iso : string Util.IntMap.t -> Format.formatter -> iso -> unit
val create_map_base : base list -> string Util.IntMap.t
val create_map_iso : iso list -> string Util.IntMap.t
val push_inv : iso -> iso

type pat =
  | PatUnit
  | PatVar of int
  | PatCtor of int
  | PatApp of int * pat
  | PatTuple of pat List2.t
  | PatChar of char

type value =
  | ValueUnit
  | ValueCtor of int
  | ValueApp of int * value
  | ValueTuple of value List2.t
  | ValueChar of char

type expr =
  | ExprPat of pat
  | ExprLet of { p_1 : pat; p_2 : pat; e : expr }
  | ExprLetApp of { p_1 : pat; omega : iso; p_2 : pat; e : expr }

and iso =
  | IsoVar of int
  | IsoFix of int * iso
  | IsoFun of int * iso
  | IsoApp of iso * iso
  | IsoInv of iso
  | IsoCase of (pat * expr) List1.t

and term =
  | TermUnit
  | TermVar of int
  | TermCtor of int
  | TermCtorApp of int * term
  | TermTuple of term List2.t
  | TermIsoApp of iso * term
  | TermLet of { p : pat; t_1 : term; t_2 : term }
  | TermIso of { phi : int; omega : iso; t : term }
  | TermChar of char

type 'a subst = 'a * int

val term_of_value : value -> term
val term_of_pat : pat -> term
val term_of_expr : expr -> term
val pat_of_expr : expr -> pat
val subst_term : value subst -> term -> term
val subst_term_bulk : value subst list -> term -> term
val subst_iso : iso subst -> iso -> iso
val subst_iso_expr : iso subst -> expr -> expr
val subst_iso_term : iso subst -> term -> term
val invert : iso -> iso
val pp_value : string Util.IntMap.t -> Format.formatter -> value -> unit
val alpha_term : Alpha.u -> term -> term
val is_fv_pat : int -> pat -> bool
val fv_pat : pat -> int list

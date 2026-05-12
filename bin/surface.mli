module M (T : sig
  type t
end) : sig
  type t = T.t

  type pat =
    | PatUnit
    | PatVar of t
    | PatCtor of t
    | PatApp of t * pat
    | PatTuple of pat List2.t

  type epat =
    | EPatUnit
    | EPatVar of t
    | EPatCtor of t
    | EPatCtorApp of t * epat
    | EPatTuple of epat List2.t
    | EPatIsoApp of iso * epat

  and expr = ExprEPat of epat | ExprLet of { p : pat; ep : epat; e : expr }

  and iso =
    | IsoVar of t
    | IsoFix of t * iso
    | IsoFun of t * iso
    | IsoApp of iso * iso
    | IsoInv of iso
    | IsoCase of (pat * expr) List1.t

  and term =
    | TermUnit
    | TermVar of t
    | TermCtor of t
    | TermCtorApp of t * term
    | TermTuple of term List2.t
    | TermIsoApp of iso * term
    | TermLet of { p : pat; t_1 : term; t_2 : term }
    | TermIso of { phi : t; omega : iso; t : term }

  type base =
    | BaseUnit
    | BaseIdent of t
    | BaseVar of t
    | BaseProd of base List2.t
    | BaseApp of base List1.t * t

  type variant = t * base option
  type typedef = { params : t list; name : t; variants : variant list }
  type program = typedef list * term
end

module MInt : module type of M (Int)
module MStr : module type of M (String)

val cvt_program : Alpha.t -> MStr.program -> MInt.program
val expand_term : Util.generator -> MInt.term -> Terms.term
val base_silly : MInt.base -> Types.base
val alpha_typedef : Util.generator -> MInt.typedef -> MInt.typedef

val check_typedefs :
  map:string Util.IntMap.t -> MInt.typedef list -> (unit, string) result

val alpha_typedefs :
  map:string Util.IntMap.t ->
  Util.generator ->
  MInt.typedef list ->
  MInt.typedef list

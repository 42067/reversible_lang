module M (T : sig
  type t
end) =
struct
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

module MInt = M (Int)
module MStr = M (String)

let rec cvt_pat alpha : MStr.pat -> MInt.pat = function
  | PatUnit -> PatUnit
  | PatVar v -> PatVar (Alpha.get alpha v)
  | PatCtor c -> PatCtor (Alpha.get alpha c)
  | PatApp (c, p) -> PatApp (Alpha.get alpha c, cvt_pat alpha p)
  | PatTuple l -> PatTuple (List2.map (cvt_pat alpha) l)

let rec cvt_term alpha : MStr.term -> MInt.term = function
  | TermUnit -> TermUnit
  | TermVar v -> TermVar (Alpha.get alpha v)
  | TermCtor c -> TermCtor (Alpha.get alpha c)
  | TermCtorApp (c, t) -> TermCtorApp (Alpha.get alpha c, cvt_term alpha t)
  | TermTuple l -> TermTuple (List2.map (cvt_term alpha) l)
  | TermIsoApp (omega, t) -> TermIsoApp (cvt_iso alpha omega, cvt_term alpha t)
  | TermLet { p; t_1; t_2 } ->
      TermLet
        {
          p = cvt_pat alpha p;
          t_1 = cvt_term alpha t_1;
          t_2 = cvt_term alpha t_2;
        }
  | TermIso { phi; omega; t } ->
      TermIso
        {
          phi = Alpha.get alpha phi;
          omega = cvt_iso alpha omega;
          t = cvt_term alpha t;
        }

and cvt_iso alpha : MStr.iso -> MInt.iso = function
  | IsoVar phi -> IsoVar (Alpha.get alpha phi)
  | IsoFix (phi, omega) -> IsoFix (Alpha.get alpha phi, cvt_iso alpha omega)
  | IsoFun (phi, omega) -> IsoFun (Alpha.get alpha phi, cvt_iso alpha omega)
  | IsoApp (omega_1, omega_2) ->
      IsoApp (cvt_iso alpha omega_1, cvt_iso alpha omega_2)
  | IsoInv omega -> IsoInv (cvt_iso alpha omega)
  | IsoCase l ->
      let f (p, e) = (cvt_pat alpha p, cvt_expr alpha e) in
      IsoCase (List1.map f l)

and cvt_expr alpha : MStr.expr -> MInt.expr = function
  | ExprEPat ep -> ExprEPat (cvt_epat alpha ep)
  | ExprLet { p; ep; e } ->
      ExprLet
        { p = cvt_pat alpha p; ep = cvt_epat alpha ep; e = cvt_expr alpha e }

and cvt_epat alpha : MStr.epat -> MInt.epat = function
  | EPatUnit -> EPatUnit
  | EPatVar v -> EPatVar (Alpha.get alpha v)
  | EPatCtor c -> EPatCtor (Alpha.get alpha c)
  | EPatCtorApp (c, ep) -> EPatCtorApp (Alpha.get alpha c, cvt_epat alpha ep)
  | EPatTuple l -> EPatTuple (List2.map (cvt_epat alpha) l)
  | EPatIsoApp (omega, ep) -> EPatIsoApp (cvt_iso alpha omega, cvt_epat alpha ep)

let rec expand_pat : MInt.pat -> Terms.pat = function
  | PatUnit -> PatUnit
  | PatVar x -> PatVar x
  | PatCtor c -> PatCtor c
  | PatApp (c, p) -> PatApp (c, expand_pat p)
  | PatTuple l -> PatTuple (List2.map expand_pat l)

let rec cvt_base alpha : MStr.base -> MInt.base = function
  | BaseUnit -> BaseUnit
  | BaseIdent x -> BaseIdent (Alpha.get alpha x)
  | BaseVar v -> BaseVar (Alpha.get alpha v)
  | BaseProd l -> BaseProd (List2.map (cvt_base alpha) l)
  | BaseApp (l, x) -> BaseApp (List1.map (cvt_base alpha) l, Alpha.get alpha x)

let cvt_variant alpha ((c, a) : MStr.variant) : MInt.variant =
  (Alpha.get alpha c, Option.map (cvt_base alpha) a)

let cvt_typedef alpha MStr.{ params; name; variants } =
  MInt.
    {
      params = List.map (Alpha.get alpha) params;
      name = Alpha.get alpha name;
      variants = List.map (cvt_variant alpha) variants;
    }

let cvt_program alpha (ts, t) =
  (List.map (cvt_typedef alpha) ts, cvt_term alpha t)

let rec pump gen : MInt.epat -> (int * Terms.iso * Terms.pat) list * Terms.pat =
  function
  | EPatUnit -> ([], PatUnit)
  | EPatVar x -> ([], PatVar x)
  | EPatCtor c -> ([], PatCtor c)
  | EPatCtorApp (c, ep) ->
      let s, p = pump gen ep in
      (s, PatApp (c, p))
  | EPatTuple l ->
      let ss, ps = List2.map (pump gen) l |> List2.split in
      (List2.to_list ss |> List.flatten, PatTuple ps)
  | EPatIsoApp (omega, ep) ->
      let s, p = pump gen ep in
      let fresh = Util.fresh gen in
      ((fresh, expand_iso gen omega, p) :: s, PatVar fresh)

and expand_iso gen : MInt.iso -> Terms.iso = function
  | IsoVar phi -> IsoVar phi
  | IsoFix (phi, omega) -> IsoFix (phi, expand_iso gen omega)
  | IsoFun (phi, omega) -> IsoFun (phi, expand_iso gen omega)
  | IsoApp (omega_1, omega_2) ->
      IsoApp (expand_iso gen omega_1, expand_iso gen omega_2)
  | IsoInv omega -> IsoInv (expand_iso gen omega)
  | IsoCase l ->
      IsoCase (List1.map (fun (p, e) -> (expand_pat p, expand_expr gen e)) l)

and expand_expr gen : MInt.expr -> Terms.expr = function
  | ExprEPat ep ->
      let s, p = pump gen ep in
      let folder e (x, omega, p_2) =
        Terms.ExprLetApp { p_1 = Terms.PatVar x; omega; p_2; e }
      in
      let init = Terms.ExprPat p in
      List.fold_left folder init s
  | ExprLet { p; ep; e } ->
      let s, p' = pump gen ep in
      let folder e (x, omega, p_2) =
        Terms.ExprLetApp { p_1 = Terms.PatVar x; omega; p_2; e }
      in
      let init =
        Terms.ExprLet { p_1 = expand_pat p; p_2 = p'; e = expand_expr gen e }
      in
      List.fold_left folder init s

let rec expand_term gen : MInt.term -> Terms.term = function
  | TermUnit -> TermUnit
  | TermVar x -> TermVar x
  | TermCtor c -> TermCtor c
  | TermCtorApp (c, t) -> TermCtorApp (c, expand_term gen t)
  | TermTuple l -> TermTuple (List2.map (expand_term gen) l)
  | TermIsoApp (omega, t) -> TermIsoApp (expand_iso gen omega, expand_term gen t)
  | TermLet { p; t_1; t_2 } ->
      TermLet
        {
          p = expand_pat p;
          t_1 = expand_term gen t_1;
          t_2 = expand_term gen t_2;
        }
  | TermIso { phi; omega; t } ->
      TermIso { phi; omega = expand_iso gen omega; t = expand_term gen t }

let rec base_silly : MInt.base -> Types.base = function
  | BaseUnit -> BaseUnit
  | BaseIdent x -> BaseIdent x
  | BaseVar v -> BaseVar v
  | BaseProd l -> BaseProd (List2.map base_silly l)
  | BaseApp (l, x) -> BaseApp (List1.map base_silly l, x)

let alpha_typedef gen MInt.{ params; name; variants } =
  let map =
    List.map (fun v -> (v, Util.fresh gen)) params |> Util.IntMap.of_list
  in
  let params' = List.map (fun v -> Util.IntMap.find v map) params in
  let rec map_base : MInt.base -> MInt.base = function
    | BaseUnit -> BaseUnit
    | BaseIdent x -> BaseIdent x
    | BaseVar v -> BaseVar (Util.IntMap.find v map)
    | BaseProd l -> BaseProd (List2.map map_base l)
    | BaseApp (l, x) -> BaseApp (List1.map map_base l, x)
  in
  let variants' =
    List.map (fun (c, a) -> (c, Option.map map_base a)) variants
  in
  MInt.{ params = params'; name; variants = variants' }

let check_typedefs ~map (ts : MInt.typedef list) : (unit, string) result =
  let types_maybe_duped = List.map (fun MInt.{ name; _ } -> name) ts in
  let types = List.sort_uniq Int.compare types_maybe_duped in
  let duped = List.compare_lengths types_maybe_duped types <> 0 in
  if duped then Error "duplicated type definitions"
  else
    let arity_map =
      List.map (fun MInt.{ params; name; _ } -> (name, List.length params)) ts
      |> Util.IntMap.of_list
    in
    let find x =
      Util.IntMap.find_opt x arity_map
      |> Option.to_result
           ~none:begin
             Util.IntMap.find x map |> Format.sprintf "undefined type: %s"
           end
    in
    let rec check_base : MInt.base -> (unit, string) result = function
      | BaseUnit | BaseVar _ -> Ok ()
      | BaseIdent x ->
          let open Util in
          let** found = find x in
          if found = 0 then Ok () else Error "arity mismatch"
      | BaseProd l -> List2.to_list l |> List.map check_base |> Util.all_ok
      | BaseApp (l, x) ->
          let open Util in
          let list = List1.to_list l in
          let** found = find x in
          if found = List.length list then
            List.map check_base list |> Util.all_ok
          else Error "arity mismatch"
    in
    let check_variant params (_, a) =
      match a with
      | None -> Ok ()
      | Some a ->
          let undefs =
            Util.IntSet.diff
              (Types.fv_base (base_silly a))
              (Util.IntSet.of_list params)
          in
          let undef_opt = Util.IntSet.min_elt_opt undefs in
          begin match undef_opt with
          | Some v ->
              Util.IntMap.find v map
              |> Format.sprintf "undefined type varaible: %s"
              |> failwith
          | None -> check_base a
          end
    in
    let arity_ok =
      List.map
        (fun MInt.{ params; variants; _ } ->
          List.map (check_variant params) variants |> Util.all_ok)
        ts
      |> Util.all_ok
    in
    arity_ok

let alpha_typedefs ~map gen ts =
  check_typedefs ~map ts |> Result.error_to_failure;
  List.map (alpha_typedef gen) ts

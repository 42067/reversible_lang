type pat =
  | PatUnit
  | PatVar of int
  | PatCtor of int
  | PatApp of int * pat
  | PatTuple of pat List2.t

type value =
  | ValueUnit
  | ValueCtor of int
  | ValueApp of int * value
  | ValueTuple of value List2.t

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

type 'a subst = 'a * int

let rec term_of_value = function
  | ValueUnit -> TermUnit
  | ValueCtor c -> TermCtor c
  | ValueApp (c, v) -> TermCtorApp (c, term_of_value v)
  | ValueTuple l -> TermTuple (List2.map term_of_value l)

let rec term_of_pat = function
  | PatUnit -> TermUnit
  | PatVar x -> TermVar x
  | PatCtor c -> TermCtor c
  | PatApp (c, p) -> TermCtorApp (c, term_of_pat p)
  | PatTuple l -> TermTuple (List2.map term_of_pat l)

let rec term_of_expr = function
  | ExprPat p -> term_of_pat p
  | ExprLet { p_1; p_2; e } ->
      TermLet { p = p_1; t_1 = term_of_pat p_2; t_2 = term_of_expr e }
  | ExprLetApp { p_1; omega; p_2; e } ->
      TermLet
        {
          p = p_1;
          t_1 = TermIsoApp (omega, term_of_pat p_2);
          t_2 = term_of_expr e;
        }

let rec pat_of_expr = function
  | ExprPat p -> p
  | ExprLet { e; _ } | ExprLetApp { e; _ } -> pat_of_expr e

let rec subst_term ((v, x) as s) = function
  | TermUnit -> TermUnit
  | TermVar y -> if y = x then term_of_value v else TermVar y
  | TermCtor c -> TermCtor c
  | TermCtorApp (c, t) -> TermCtorApp (c, subst_term s t)
  | TermTuple l -> TermTuple (List2.map (subst_term s) l)
  | TermIsoApp (omega, t) -> TermIsoApp (omega, subst_term s t)
  | TermLet { p; t_1; t_2 } ->
      TermLet { p; t_1 = subst_term s t_1; t_2 = subst_term s t_2 }
  | TermIso { phi; omega; t } -> TermIso { phi; omega; t = subst_term s t }

let subst_term_bulk l t = List.fold_left (fun t s -> subst_term s t) t l

let rec subst_iso ((omega', phi) as s) = function
  | IsoVar psi -> if psi = phi then omega' else IsoVar psi
  | IsoFix (psi, omega) ->
      IsoFix (psi, subst_iso s omega) (* alpha-cvt assumed *)
  | IsoFun (psi, omega) ->
      IsoFun (psi, subst_iso s omega) (* alpha-cvt assumed *)
  | IsoApp (omega_1, omega_2) ->
      IsoApp (subst_iso s omega_1, subst_iso s omega_2)
  | IsoInv omega -> IsoInv (subst_iso s omega)
  | IsoCase l -> IsoCase (List1.map (fun (p, e) -> (p, subst_iso_expr s e)) l)

and subst_iso_expr ((omega', phi) as s) = function
  | ExprPat p -> ExprPat p
  | ExprLet { p_1; p_2; e } -> ExprLet { p_1; p_2; e = subst_iso_expr s e }
  | ExprLetApp { p_1; omega; p_2; e } ->
      ExprLetApp { p_1; omega = subst_iso s omega; p_2; e = subst_iso_expr s e }

let rec subst_iso_term ((omega', phi) as s) = function
  | (TermUnit | TermVar _ | TermCtor _) as t -> t
  | TermCtorApp (c, t) -> TermCtorApp (c, subst_iso_term s t)
  | TermTuple l -> TermTuple (List2.map (subst_iso_term s) l)
  | TermIsoApp (omega, t) -> TermIsoApp (subst_iso s omega, subst_iso_term s t)
  | TermLet { p; t_1; t_2 } ->
      TermLet { p; t_1 = subst_iso_term s t_1; t_2 = subst_iso_term s t_2 }
  | TermIso { phi = psi; omega; t } ->
      TermIso { phi = psi; omega = subst_iso s omega; t = subst_iso_term s t }

let rec invert = function
  | IsoVar phi -> IsoVar phi
  | IsoFix (phi, omega) -> IsoFix (phi, invert omega)
  | IsoFun (phi, omega) -> IsoFun (phi, invert omega)
  | IsoApp (omega_1, omega_2) -> IsoApp (invert omega_1, invert omega_2)
  | IsoInv omega -> IsoInv (invert omega)
  | IsoCase l ->
      IsoCase (List1.map (fun (p, e) -> invert_branch (ExprPat p) e) l)

and invert_branch (acc : expr) : expr -> pat * expr = function
  | ExprPat p -> (p, acc)
  | ExprLet { p_1; p_2; e } ->
      invert_branch (ExprLet { p_1 = p_2; p_2 = p_1; e = acc }) e
  | ExprLetApp { p_1; omega; p_2; e } ->
      invert_branch
        (ExprLetApp { p_1 = p_2; omega = invert omega; p_2 = p_1; e = acc })
        e

let rec pp_value map fmt =
  let f = Format.fprintf in
  let m c = Util.IntMap.find c map in
  function
  | ValueUnit -> f fmt "()"
  | ValueCtor c ->
      begin match m c with
      | "Z" -> f fmt "0"
      | "Nil" -> f fmt "[]"
      | s -> f fmt "%s" s
      end
  | ValueApp (c, v) ->
      begin match m c with
      | "S" ->
          let rec counter n = function
            | ValueApp (_, v) -> counter (n + 1) v
            | _ -> n
          in
          f fmt "%i" (counter 1 v)
      | "Cons" ->
          let rec cons first = function
            | ValueApp (_, ValueTuple List2.(v :: List1.(v' :: []))) ->
                if first then f fmt "[" else f fmt "; ";
                f fmt "%a" (pp_value map) v;
                cons false v'
            | _ -> f fmt "]"
          in
          cons true (ValueApp (c, v))
      | _ ->
          begin match v with
          | ValueApp (c', _) ->
              begin match m c' with
              | "S" | "Cons" -> f fmt "%s %a" (m c) (pp_value map) v
              | _ -> f fmt "%s (%a)" (m c) (pp_value map) v
              end
          | _ -> f fmt "%s %a" (m c) (pp_value map) v
          end
      end
  | ValueTuple List2.(v :: vs) ->
      f fmt "(%a" (pp_value map) v;
      List1.to_list vs |> List.iter (f fmt ", %a" (pp_value map));
      f fmt ")"

let pat_gen alpha p =
  let rec impl = function
    | PatVar x -> [ x ]
    | PatUnit -> []
    | PatCtor c -> []
    | PatApp (_, p) -> impl p
    | PatTuple l -> List2.map impl l |> List2.to_list |> List.flatten
  in
  impl p |> List.sort_uniq Int.compare
  |> List.map (fun x -> (Alpha.fresh alpha x, x))

let rec subst_pat ((x', x) as s : int subst) : pat -> pat = function
  | PatUnit -> PatUnit
  | PatVar y -> if y = x then PatVar x' else PatVar y
  | PatCtor c -> PatCtor c
  | PatApp (c, p) -> PatApp (c, subst_pat s p)
  | PatTuple l -> PatTuple (List2.map (subst_pat s) l)

let rec subst_term_var ((x', x) as s : int subst) : term -> term = function
  | TermUnit -> TermUnit
  | TermVar y -> if y = x then TermVar x' else TermVar y
  | TermCtor c -> TermCtor c
  | TermCtorApp (c, t) -> TermCtorApp (c, subst_term_var s t)
  | TermTuple l -> TermTuple (List2.map (subst_term_var s) l)
  | TermIsoApp (omega, t) -> TermIsoApp (omega, subst_term_var s t)
  | TermLet { p; t_1; t_2 } ->
      TermLet { p; t_1 = subst_term_var s t_1; t_2 = subst_term_var s t_2 }
  | TermIso { phi; omega; t } -> TermIso { phi; omega; t = subst_term_var s t }

let rec subst_expr ((x', x) as s : int subst) : expr -> expr = function
  | ExprPat p -> ExprPat (subst_pat s p)
  | ExprLet { p_1; p_2; e } ->
      ExprLet { p_1; p_2 = subst_pat s p_2; e = subst_expr s e }
  | ExprLetApp { p_1; omega; p_2; e } ->
      ExprLetApp { p_1; omega; p_2 = subst_pat s p_2; e = subst_expr s e }

let subst_pat_bulk l p = List.fold_left (fun p s -> subst_pat s p) p l
let subst_term_var_bulk l t = List.fold_left (fun t s -> subst_term_var s t) t l
let subst_expr_bulk l e = List.fold_left (fun e s -> subst_expr s e) e l

let rec alpha_term alpha = function
  | TermUnit -> TermUnit
  | TermVar x -> TermVar x
  | TermCtor c -> TermCtor c
  | TermCtorApp (c, t) -> TermCtorApp (c, alpha_term alpha t)
  | TermTuple l -> TermTuple (List2.map (alpha_term alpha) l)
  | TermIsoApp (omega, t) ->
      TermIsoApp (alpha_iso alpha omega, alpha_term alpha t)
  | TermLet { p; t_1; t_2 } ->
      let t_1' = alpha_term alpha t_1 in
      let t_2' = alpha_term alpha t_2 in
      let substs = pat_gen alpha p in
      let p' = subst_pat_bulk substs p in
      let t_2'' = subst_term_var_bulk substs t_2' in
      TermLet { p = p'; t_1 = t_1'; t_2 = t_2'' }
  | TermIso { phi; omega; t } ->
      let omega' = alpha_iso alpha omega in
      let t' = alpha_term alpha t in
      let fresh = Alpha.fresh alpha phi in
      let subst = (IsoVar fresh, phi) in
      let t'' = subst_iso_term subst t' in
      TermIso { phi = fresh; omega = omega'; t = t'' }

and alpha_iso alpha = function
  | IsoVar phi -> IsoVar phi
  | IsoFix (phi, omega) ->
      let omega' = alpha_iso alpha omega in
      let fresh = Alpha.fresh alpha phi in
      let subst = (IsoVar fresh, phi) in
      IsoFix (fresh, subst_iso subst omega')
  | IsoFun (phi, omega) ->
      let omega' = alpha_iso alpha omega in
      let fresh = Alpha.fresh alpha phi in
      let subst = (IsoVar fresh, phi) in
      IsoFun (fresh, subst_iso subst omega')
  | IsoApp (omega_1, omega_2) ->
      IsoApp (alpha_iso alpha omega_1, alpha_iso alpha omega_2)
  | IsoInv omega -> IsoInv (alpha_iso alpha omega)
  | IsoCase l -> IsoCase (List1.map (alpha_branch alpha) l)

and alpha_branch alpha (p, e) =
  let e' = alpha_expr alpha e in
  let substs = pat_gen alpha p in
  (subst_pat_bulk substs p, subst_expr_bulk substs e')

and alpha_expr alpha = function
  | ExprPat p -> ExprPat p
  | ExprLet { p_1; p_2; e } ->
      let e' = alpha_expr alpha e in
      let substs = pat_gen alpha p_1 in
      let p_1' = subst_pat_bulk substs p_1 in
      let e'' = subst_expr_bulk substs e' in
      ExprLet { p_1 = p_1'; p_2; e = e'' }
  | ExprLetApp { p_1; omega; p_2; e } ->
      let omega' = alpha_iso alpha omega in
      let e' = alpha_expr alpha e in
      let substs = pat_gen alpha p_1 in
      let p_1' = subst_pat_bulk substs p_1 in
      let e'' = subst_expr_bulk substs e' in
      ExprLetApp { p_1 = p_1'; omega = omega'; p_2; e = e'' }

let rec is_fv_pat x = function
  | PatUnit | PatCtor _ -> false
  | PatVar y -> y = x
  | PatApp (_, p) -> is_fv_pat x p
  | PatTuple l -> List2.fold_left (fun b p -> b || is_fv_pat x p) false l

let fv_pat p =
  let rec fv_pat_list = function
    | PatUnit | PatCtor _ -> []
    | PatVar x -> [ x ]
    | PatApp (_, p) -> fv_pat_list p
    | PatTuple l -> List2.map fv_pat_list l |> List2.to_list |> List.flatten
  in
  fv_pat_list p |> List.sort_uniq Int.compare

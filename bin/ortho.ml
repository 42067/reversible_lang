type subst = Terms.pat * int
type eq = Terms.pat * Terms.pat
type ok = { linear : bool; left : bool; right : bool }

let rec subst_pat ((p, x) as s) : Terms.pat -> Terms.pat = function
  | PatUnit -> PatUnit
  | PatCtor c -> PatCtor c
  | PatVar y -> if y = x then p else PatVar y
  | PatApp (c, p) -> PatApp (c, subst_pat s p)
  | PatTuple l -> PatTuple (List2.map (subst_pat s) l)

let subst_eq s (p, q) = (subst_pat s p, subst_pat s q)
let subst_eqs s l = List.map (subst_eq s) l

let rec unify : eq list -> subst list option = function
  | [] -> Some []
  | e :: es -> begin
      let open Util in
      match e with
      | a, b when a = b -> unify es
      | PatVar x, b when Terms.is_fv_pat x b |> not ->
          let s = (b, x) in
          let+ unified = subst_eqs s es |> unify in
          s :: unified
      | a, PatVar x when Terms.is_fv_pat x a |> not ->
          let s = (a, x) in
          let+ unified = subst_eqs s es |> unify in
          s :: unified
      | PatApp (c_1, p_1), PatApp (c_2, p_2) when c_1 = c_2 ->
          (p_1, p_2) :: es |> unify
      | PatTuple a, PatTuple b when List2.eq_length a b ->
          (List2.combine a b |> List2.to_list) @ es |> unify
      | _ -> None
    end

let is_ortho e = unify [ e ] |> Option.is_none

let rec is_ortho_all_perm = function
  | [] -> true
  | p :: ps ->
      let ok = List.fold_left (fun ok p' -> ok && is_ortho (p, p')) true ps in
      ok && is_ortho_all_perm ps

let is_linear (p, e) =
  let rec vs_positive = function
    | Terms.ExprPat p -> []
    | Terms.ExprLet { p_1; e } -> Terms.fv_pat p_1 @ vs_positive e
    | Terms.ExprLetApp { p_1; e } -> Terms.fv_pat p_1 @ vs_positive e
  in
  let rec vs_negative = function
    | Terms.ExprPat p -> Terms.fv_pat p
    | Terms.ExprLet { p_2; e } -> Terms.fv_pat p_2 @ vs_negative e
    | Terms.ExprLetApp { p_2; e } -> Terms.fv_pat p_2 @ vs_negative e
  in
  let positive = Terms.fv_pat p @ vs_positive e in
  let negative = vs_negative e in
  let module Counter = Hashtbl.Make (Int) in
  let counter = Counter.create 0 in
  let increment x =
    match Counter.find_opt counter x with
    | None -> Counter.add counter x 1
    | Some y -> Counter.replace counter x (y + 1)
  in
  let decrement x =
    match Counter.find_opt counter x with
    | None -> Counter.add counter x (-1)
    | Some y -> Counter.replace counter x (y - 1)
  in
  List.iter increment positive;
  List.iter decrement negative;
  Counter.fold (fun _key value acc -> acc && value = 0) counter true

let auto l =
  let linear =
    List.fold_left (fun acc ((p, e) as b) -> acc && is_linear b) true l
  in
  let ps_left, es = List.split l in
  let ps_right = List.map Terms.pat_of_expr es in
  {
    linear;
    left = is_ortho_all_perm ps_left;
    right = is_ortho_all_perm ps_right;
  }

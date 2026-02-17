open Types
open Util

let matches (u : value) (v : value) : bool =
  let map_u = build_storage None u |> ref in

  let matches x v map =
    match StrMap.find_opt x !map with
    | None | Some None -> true
    (* more than one occurrence but not memoed *)
    | Some (Some None) ->
        map := StrMap.add x (Some (Some v)) !map;
        true
    (* memoed *)
    | Some (Some (Some v')) -> v = v'
  in

  let rec body ((u, v) : value * value) : bool =
    match (u, v) with
    | Unit, Unit -> true
    | Var x, _ -> matches x v map_u
    | Ctor x, Ctor y -> x = y
    | Cted { c = c_1; v = v_1 }, Cted { c = c_2; v = v_2 } ->
        c_1 = c_2 && body (v_1, v_2)
    | Tuple l, Tuple r ->
        Option.map (List.for_all body) (combine l r)
        |> Option.value ~default:false
    | _ -> false
  in

  body (u, v)

let rec invert (omega : iso) : iso =
  match omega with
  | Pairs p ->
      let rec invert_expr (e : expr) (acc : expr) =
        match e with
        | Value v -> (v, acc)
        | Let { p_1; omega; p_2; e } ->
            Let { p_1 = p_2; omega = invert omega; p_2 = p_1; e = acc }
            |> invert_expr e
        | LetVal { p; v; e } ->
            LetVal { p = v; v = p; e = acc } |> invert_expr e
      in
      let invert_pair (v, e) = invert_expr e (Value v) in
      Pairs (List.map invert_pair p)
  | Fix { phi; omega } -> Fix { phi; omega = invert omega }
  | Lambda { psi; omega } -> Lambda { psi; omega = invert omega }
  | Var _ (* fix i guess *) -> omega
  | App { omega_1; omega_2 } ->
      App { omega_1 = invert omega_1; omega_2 = invert omega_2 }
  | Invert omega -> omega

let rec subst ~(from : string) ~(into : term) ~(what : term) : term =
  let subst what = subst ~from ~into ~what in
  match what with
  | Var x when x = from -> into
  | Cted { c; t } -> Cted { c; t = subst t }
  | Tuple l -> Tuple (List.map subst l)
  | App { omega; t } -> App { omega; t = subst t }
  | Let { p; t_1; t_2 } when contains_value from p ->
      Let { p; t_1 = subst t_1; t_2 }
  | Let { p; t_1; t_2 } -> Let { p; t_1 = subst t_1; t_2 = subst t_2 }
  | LetIso { phi; omega; t } when phi <> from ->
      LetIso { phi; omega; t = subst t }
  | _ -> what

let rec subst_iso ~(from : string) ~(into : iso) ~(what : iso) : iso =
  let subst_iso what = subst_iso ~from ~into ~what in
  match what with
  | Pairs p when not (contains_pairs from p) ->
      let pairs =
        List.map (fun (v, e) -> (v, subst_iso_in_expr ~from ~into ~what:e)) p
      in
      Pairs pairs
  | Fix { phi; omega } when phi <> from -> Fix { phi; omega = subst_iso omega }
  | Lambda { psi; omega } when psi <> from ->
      Lambda { psi; omega = subst_iso omega }
  | Var x when x = from -> into
  | App { omega_1; omega_2 } ->
      App { omega_1 = subst_iso omega_1; omega_2 = subst_iso omega_2 }
  | Invert omega -> Invert (subst_iso omega)
  | _ -> what

and subst_iso_in_expr ~(from : string) ~(into : iso) ~(what : expr) : expr =
  match what with
  | Value _ -> what
  | Let { p_1; omega; p_2; e } ->
      let omega = subst_iso ~from ~into ~what:omega in
      let e =
        if contains_value from p_1 then e
        else subst_iso_in_expr ~from ~into ~what:e
      in
      Let { p_1; omega; p_2; e }
  | LetVal { p; v; e } ->
      let e =
        if contains_value from p then e
        else subst_iso_in_expr ~from ~into ~what:e
      in
      LetVal { p; v; e }

let rec subst_iso_in_term ~(from : string) ~(into : iso) ~(what : term) : term =
  let subst_iso_in_term what = subst_iso_in_term ~from ~into ~what in
  let subst_iso what = subst_iso ~from ~into ~what in
  match what with
  | Tuple l -> Tuple (List.map subst_iso_in_term l)
  | Cted { c; t } -> Cted { c; t = subst_iso_in_term t }
  | App { omega; t } -> App { omega = subst_iso omega; t = subst_iso_in_term t }
  | Let { p; t_1; t_2 } when contains_value from p ->
      Let { p; t_1 = subst_iso_in_term t_1; t_2 }
  | Let { p; t_1; t_2 } ->
      Let { p; t_1 = subst_iso_in_term t_1; t_2 = subst_iso_in_term t_2 }
  | LetIso { phi; omega; t } when phi = from ->
      LetIso { phi; omega = subst_iso omega; t }
  | LetIso { phi; omega; t } ->
      LetIso { phi; omega = subst_iso omega; t = subst_iso_in_term t }
  | _ -> what

let rec value_of_term (t : term) : value myresult =
  match t with
  | Unit -> Ok Unit
  | Var x -> Ok (Var x)
  | Ctor x -> Ok (Ctor x)
  | Tuple l ->
      let++ l = List.map value_of_term l |> bind_all in
      let lmao : value = Tuple l in
      lmao
  | Cted { c; t : term } ->
      let++ v = value_of_term t in
      let lmao : value = Cted { c; v } in
      lmao
  | _ -> Error ("unreachable (unreduced term: " ^ show_term t ^ ")")

let match_pair (l : (value * expr) list) (v : value) : (value * expr) option =
  List.find_opt (fun (v', _) -> matches v' v) l

let rec unify_value (u : value) (v : value) : (string * value) list myresult =
  match (u, v) with
  | Unit, Unit -> Ok []
  | Var x, _ -> Ok [ (x, v) ]
  | Ctor x, Ctor y when x = y -> Ok []
  | Cted { c = c_1; v = v_1 }, Cted { c = c_2; v = v_2 } when c_1 = c_2 ->
      unify_value v_1 v_2
  | Tuple l, Tuple r when List.compare_lengths l r = 0 ->
      let combined = List.combine l r in
      let++ unified =
        List.map (fun (u, v) -> unify_value u v) combined |> bind_all
      in
      List.flatten unified
  | _ -> Error ("unable to unify " ^ show_value u ^ " and " ^ show_value v)

let rec eval (t : term) : term myresult =
  match t with
  | Tuple l ->
      let++ l = List.map eval l |> bind_all in
      Tuple l
  | Cted { c; t } ->
      let++ t = eval t in
      Cted { c; t }
  | App { omega; t = u } -> begin
      let omega = eval_iso omega in
      let** v' = Result.bind (eval u) value_of_term in
      match omega with
      | Pairs p ->
          let** v, e =
            let msg = lazy ("out of domain: " ^ show_pairs_lhs v' p) in
            match_pair p v' |> Option.to_result ~none:(Lazy.force msg)
          in
          let** unified = unify_value v v' in
          List.fold_left
            (fun t (from, into) ->
              subst ~from ~into:(term_of_value into) ~what:t)
            (term_of_expr e) unified
          |> eval
      | _ -> Ok t
    end
  | Let { p; t_1; t_2 } -> begin
      let** t_1 = Result.bind (eval t_1) value_of_term in
      if matches p t_1 then
        let** unified = unify_value p t_1 in
        List.fold_left
          (fun t (from, into) -> subst ~from ~into:(term_of_value into) ~what:t)
          t_2 unified
        |> eval
      else Error ("unable to unify " ^ show_value p ^ " and " ^ show_value t_1)
    end
  | LetIso { phi; omega; t } ->
      let omega = eval_iso omega in
      subst_iso_in_term ~from:phi ~into:omega ~what:t |> eval
  | _ -> Ok t

and eval_iso (omega : iso) : iso =
  match omega with
  | Fix { phi; omega = omega'; _ } ->
      subst_iso ~from:phi ~into:omega ~what:omega' |> eval_iso
  | App { omega_1; omega_2 } -> begin
      match eval_iso omega_1 with
      | Lambda { psi; omega; _ } ->
          subst_iso ~from:psi ~into:omega_2 ~what:omega |> eval_iso
      | _ -> omega
    end
  | Invert omega -> eval_iso omega |> invert |> eval_iso
  | _ -> omega

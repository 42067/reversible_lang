open Terms

let unify (pv : pat * value) : value subst list option =
  let rec impl = function
    | PatUnit, ValueUnit -> Some []
    | PatVar x, v -> Some [ (v, x) ]
    | PatCtor c, ValueCtor c' -> if c = c' then Some [] else None
    | PatApp (c, p), ValueApp (c', v) -> if c = c' then impl (p, v) else None
    | PatTuple l, ValueTuple l' ->
        let open Util in
        let* combined = List2.combine_opt l l' in
        let+ substs_list =
          List.map impl (List2.to_list combined) |> unwrap_opt
        in
        List.flatten substs_list
    | PatChar c, ValueChar c' -> if c = c' then Some [] else None
    | _ -> None
  in
  let open Util in
  let* unchecked = impl pv in
  let sorted = List.sort (fun (_, x) (_, y) -> compare x y) unchecked in
  let eq_check (ok, prev) ((v, x) as s) =
    match prev with
    | Some (v_prev, x_prev) ->
        if x_prev = x then (ok && v_prev = v, Some s) else (ok, Some s)
    | None -> (ok, Some s)
  in
  let ok, _ = List.fold_left eq_check (true, None) sorted in
  if ok then Some sorted else None

let rec eval_term = function
  | TermUnit -> ValueUnit
  | TermVar _ -> failwith "stuck: TermVar (unreachable)"
  | TermCtor c -> ValueCtor c
  | TermCtorApp (c, t) -> ValueApp (c, eval_term t)
  | TermTuple l -> ValueTuple (List2.map eval_term l)
  | TermIsoApp (omega, t) ->
      begin match eval_iso omega with
      | IsoCase l ->
          let v = eval_term t in
          let finder (p, e) =
            let open Util in
            let+ unified = unify (p, v) in
            (unified, e)
          in
          begin match List1.to_list l |> List.find_map finder with
          | Some (substs, e) ->
              subst_term_bulk substs (term_of_expr e) |> eval_term
          | None -> failwith "stuck: IsoCase (no branch matched)"
          end
      | _ -> failwith "stuck: TermIsoApp (unreachable)"
      end
  | TermLet { p; t_1; t_2 } -> begin
      let v = eval_term t_1 in
      match unify (p, v) with
      | Some substs -> subst_term_bulk substs t_2 |> eval_term
      | None -> failwith "stuck: TermLet (pattern did not match)"
    end
  | TermIso { phi; omega; t } -> subst_iso_term (omega, phi) t |> eval_term
  | TermChar c -> ValueChar c

and eval_iso = function
  | (IsoVar _ | IsoFun _ | IsoCase _) as omega -> omega
  | IsoFix (phi, omega) as omega' -> subst_iso (omega', phi) omega |> eval_iso
  | IsoApp (omega_1, omega_2) ->
      begin match eval_iso omega_1 with
      | IsoFun (phi, omega) -> subst_iso (omega_2, phi) omega |> eval_iso
      | _ -> failwith "stuck: IsoApp (unreachable)"
      end
  | IsoInv omega -> invert omega |> eval_iso

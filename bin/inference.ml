type 'a subst = 'a * int
type 'a scheme = { forall : int list; ty : 'a }
type 'a ctx = 'a scheme Util.IntMap.t
type 'a eq = 'a * 'a
type eq_combined = EqBase of Types.base eq | EqIso of Types.iso eq

type 'a inferred = {
  ty : 'a;
  e_base : Types.base eq list;
  e_iso : Types.iso eq list;
}

let first_generalization = ref true

let subst_eq_base s =
  List.map begin function
      | EqBase (a, b) -> EqBase (Types.subst_base s a, Types.subst_base s b)
      | EqIso (t_1, t_2) ->
          EqIso (Types.subst_base_iso s t_1, Types.subst_base_iso s t_2)
    end

let subst_eq_iso s =
  List.map begin function
      | EqBase e -> EqBase e
      | EqIso (t_1, t_2) -> EqIso (Types.subst_iso s t_1, Types.subst_iso s t_2)
    end

let instantiate_base gen { forall; ty } =
  let folder ty v = Types.subst_base (Types.BaseVar (Util.fresh gen), v) ty in
  List.fold_left folder ty forall

let instantiate_iso gen { forall; ty } =
  let folder_base ty v =
    Types.subst_base_iso (Types.BaseVar (Util.fresh gen), v) ty
  in
  let folder_iso ty v = Types.subst_iso (Types.IsoVar (Util.fresh gen), v) ty in
  let mid = List.fold_left folder_base ty forall in
  List.fold_left folder_iso mid forall

let fv_phi phi =
  let open Util in
  let fv { forall; ty } =
    IntSet.diff (Types.fv_iso ty) (IntSet.of_list forall)
  in
  IntMap.fold (fun _ scm acc -> fv scm |> IntSet.union acc) phi IntSet.empty

let fv_delta delta =
  let open Util in
  let fv { forall; ty } =
    IntSet.diff (Types.fv_base ty) (IntSet.of_list forall)
  in
  IntMap.fold (fun _ scm acc -> fv scm |> IntSet.union acc) delta IntSet.empty

let subst_phi ((_, v) as s) =
  Util.IntMap.map begin fun { forall; ty } ->
      if List.for_all (( <> ) v) forall then
        { forall; ty = Types.subst_iso s ty }
      else { forall; ty }
    end

let subst_phi_base ((_, v) as s) =
  Util.IntMap.map begin fun { forall; ty } ->
      if List.for_all (( <> ) v) forall then
        { forall; ty = Types.subst_base_iso s ty }
      else { forall; ty }
    end

let subst_delta ((_, v) as s) =
  Util.IntMap.map begin fun { forall; ty } ->
      if List.for_all (( <> ) v) forall then
        { forall; ty = Types.subst_base s ty }
      else { forall; ty }
    end

let subst_phi_bulk substs_iso substs_base phi =
  let partial = List.fold_left (fun phi s -> subst_phi s phi) phi substs_iso in
  List.fold_left (fun phi s -> subst_phi_base s phi) partial substs_base

let subst_delta_bulk substs_base delta =
  List.fold_left (fun delta s -> subst_delta s delta) delta substs_base

let combine_eq es_base es_iso =
  List.map (fun e -> EqBase e) es_base @ List.map (fun e -> EqIso e) es_iso

let split_eq =
  List.fold_left
    (fun (l, r) -> function EqBase e -> (e :: l, r) | EqIso e -> (l, e :: r))
    ([], [])

let find_generalizable_base phi delta a =
  let open Util in
  IntSet.union (fv_phi phi) (fv_delta delta)
  |> IntSet.diff (Types.fv_base a)
  |> IntSet.to_list

let find_generalizable_iso phi delta t =
  let open Util in
  IntSet.union (fv_phi phi) (fv_delta delta)
  |> IntSet.diff (Types.fv_iso t)
  |> IntSet.to_list

let rec unify ~map =
  let open Types in
  function
  | [] -> ([], [])
  | EqBase e :: es ->
      begin match e with
      | a, b when a = b -> unify ~map es
      | BaseProd la, BaseProd lb when List2.eq_length la lb ->
          let wrapped = List2.combine la lb |> List2.map (fun e -> EqBase e) in
          List2.to_list wrapped @ es |> unify ~map
      | BaseApp (l_1, x_1), BaseApp (l_2, x_2)
        when x_1 = x_2 && List1.eq_length l_1 l_2 ->
          let wrapped =
            List1.combine l_1 l_2 |> List1.map (fun e -> EqBase e)
          in
          List1.to_list wrapped @ es |> unify ~map
      | BaseVar v, a when is_fv_base v a |> not ->
          let s = (a, v) in
          let substs_iso, substs_base = subst_eq_base s es |> unify ~map in
          (substs_iso, s :: substs_base)
      | a, BaseVar v when is_fv_base v a |> not ->
          let s = (a, v) in
          let substs_iso, substs_base = subst_eq_base s es |> unify ~map in
          (substs_iso, s :: substs_base)
      | a, b ->
          let map' = create_map_base [ a; b ] in
          let pp = Util.union ~weak:map ~strong:map' |> pp_base in
          Format.asprintf "unable to unify `%a` and `%a`" pp a pp b |> failwith
      end
  | EqIso e :: es ->
      begin match e with
      | t_1, t_2 when t_1 = t_2 -> unify ~map es
      | IsoArrow (t_11, t_12), IsoArrow (t_21, t_22) ->
          EqIso (t_11, t_21) :: EqIso (t_12, t_22) :: es |> unify ~map
      | IsoBiArrow (a_1, b_1), IsoBiArrow (a_2, b_2) ->
          EqBase (a_1, a_2) :: EqBase (b_1, b_2) :: es |> unify ~map
      | IsoInv t_1, IsoInv t_2 -> EqIso (t_1, t_2) :: es |> unify ~map
      | IsoInv t, IsoBiArrow (a, b) | IsoBiArrow (a, b), IsoInv t ->
          EqIso (t, IsoBiArrow (b, a)) :: es |> unify ~map
      | IsoInv t, IsoArrow (t_1, t_2) | IsoArrow (t_1, t_2), IsoInv t ->
          EqIso (t, IsoArrow (invert t_1, invert t_2)) :: es |> unify ~map
      | IsoVar v, t when is_fv_iso v t |> not ->
          let s = (t, v) in
          let substs_iso, substs_base = subst_eq_iso s es |> unify ~map in
          (s :: substs_iso, substs_base)
      | t, IsoVar v when is_fv_iso v t |> not ->
          let s = (t, v) in
          let substs_iso, substs_base = subst_eq_iso s es |> unify ~map in
          (s :: substs_iso, substs_base)
      | t_1, t_2 ->
          let map' = create_map_iso [ t_1; t_2 ] in
          let pp = Util.union ~weak:map ~strong:map' |> pp_iso in
          Format.asprintf "unable to unify `%a` and `%a`" pp t_1 pp t_2
          |> failwith
      end

let pat_gen gen p =
  let rec impl gen = function
    | Terms.PatVar v -> [ (v, Types.BaseVar (Util.fresh gen)) ]
    | Terms.PatUnit | PatCtor _ -> []
    | Terms.PatApp (_, p) -> impl gen p
    | Terms.PatTuple l ->
        List2.map (impl gen) l |> List2.to_list |> List.flatten
  in
  impl gen p |> Util.IntMap.of_list

let generalize_iso ~map phi delta es v t =
  let substs_iso, substs_base = unify ~map es in
  let t' = Types.subst_iso_bulk substs_iso substs_base t in
  let phi' = subst_phi_bulk substs_iso substs_base phi in
  let delta' = subst_delta_bulk substs_base delta in
  let forall = find_generalizable_iso phi' delta' t' in
  let generalized = { forall; ty = t' } in
  let name =
    let name = Util.IntMap.find v map in
    if !first_generalization then
      let () = first_generalization := false in
      "inv (built-in)"
    else name
  in
  Format.printf "%s : \x1b[35m%a\x1b[0m\n" name
    (Types.pp_iso (Util.union ~weak:map ~strong:(Types.create_map_iso [ t' ])))
    (Types.push_inv t');
  Util.IntMap.add v generalized phi'

let rec generalize_base ?(disabled = false) ~map gen phi delta es p a =
  let substs_iso, substs_base = unify ~map es in
  let a' = Types.subst_base_bulk substs_base a in
  let phi' = subst_phi_bulk substs_iso substs_base phi in
  let delta' = subst_delta_bulk substs_base delta in
  let forall = find_generalizable_base phi' delta' a' in

  let extracted = pat_gen gen p in
  let delta'' =
    let wrapped = Util.IntMap.map (fun ty -> { forall = []; ty }) extracted in
    Util.union ~weak:delta' ~strong:wrapped
  in
  let { ty = ty_pat; e_base = e_base_pat; e_iso = e_iso_pat } =
    Terms.term_of_pat p |> infer_term ~map gen phi' delta''
  in
  let es_pat = EqBase (ty_pat, a') :: combine_eq e_base_pat e_iso_pat in
  let substs_iso_pat, substs_base_pat = unify ~map es_pat in
  let generalized =
    let lift a =
      {
        forall = (if disabled then [] else forall);
        ty = Types.subst_base_bulk substs_base_pat a;
      }
    in
    Util.IntMap.map lift extracted
  in
  (Util.union ~weak:delta'' ~strong:generalized, es_pat)

and infer_term ~map gen psi delta =
  let open Terms in
  let open Types in
  let open Util in
  function
  | TermUnit -> { ty = BaseUnit; e_base = []; e_iso = [] }
  | TermVar x ->
      let found =
        try IntMap.find x delta
        with _ ->
          Format.sprintf "undefined non-iso variable: %s" (IntMap.find x map)
          |> failwith
      in
      { ty = instantiate_base gen found; e_base = []; e_iso = [] }
  | TermCtor c ->
      let found =
        try IntMap.find c delta
        with _ ->
          Format.sprintf "undefined nullary constructor: %s" (IntMap.find c map)
          |> failwith
      in
      { ty = instantiate_base gen found; e_base = []; e_iso = [] }
  | TermCtorApp (c, t) -> begin
      let found =
        try IntMap.find c psi
        with _ ->
          Format.sprintf "undefined unary constructor: %s" (IntMap.find c map)
          |> failwith
      in
      match instantiate_iso gen found with
      | IsoBiArrow (a, b) ->
          let { ty = a'; e_base; e_iso } = infer_term ~map gen psi delta t in
          { ty = b; e_base = (a, a') :: e_base; e_iso }
      | _ -> failwith "inference failed: TermCtorApp (unreachable)"
    end
  | TermTuple l ->
      let list = List2.map (infer_term ~map gen psi delta) l in
      {
        ty = BaseProd (List2.map (fun { ty; _ } -> ty) list);
        e_base =
          List2.to_list list
          |> List.fold_left (fun acc { e_base; _ } -> acc @ e_base) [];
        e_iso =
          List2.to_list list
          |> List.fold_left (fun acc { e_iso; _ } -> acc @ e_iso) [];
      }
  | TermIsoApp (omega, t) ->
      let { ty = ty_omega; e_base = e_base_omega; e_iso = e_iso_omega } =
        infer_iso ~map gen psi delta omega
      in
      let { ty = ty_t; e_base = e_base_t; e_iso = e_iso_t } =
        infer_term ~map gen psi delta t
      in
      let fresh = Types.BaseVar (Util.fresh gen) in
      {
        ty = fresh;
        e_base = e_base_omega @ e_base_t;
        e_iso =
          ((ty_omega, Types.IsoBiArrow (ty_t, fresh)) :: e_iso_omega) @ e_iso_t;
      }
  | TermIso { phi; omega; t } ->
      let { ty = ty_omega; e_base = e_base_omega; e_iso = e_iso_omega } =
        infer_iso ~map gen psi delta omega
      in
      let psi' =
        let combined = combine_eq e_base_omega e_iso_omega in
        generalize_iso ~map psi delta combined phi ty_omega
      in
      let { ty = ty_t; e_base = e_base_t; e_iso = e_iso_t } =
        infer_term ~map gen psi' delta t
      in
      {
        ty = ty_t;
        e_base = e_base_omega @ e_base_t;
        e_iso = e_iso_omega @ e_iso_t;
      }
  | TermLet { p; t_1; t_2 } ->
      let { ty = ty_1; e_base = e_base_1; e_iso = e_iso_1 } =
        infer_term ~map gen psi delta t_1
      in
      let delta', es =
        let combined = combine_eq e_base_1 e_iso_1 in
        generalize_base ~map gen psi delta combined p ty_1
      in
      let { ty = ty_2; e_base = e_base_2; e_iso = e_iso_2 } =
        infer_term ~map gen psi delta' t_2
      in
      let es_base, es_iso = split_eq es in
      {
        ty = ty_2;
        e_base = e_base_1 @ e_base_2 @ es_base;
        e_iso = e_iso_1 @ e_iso_2 @ es_iso;
      }

and infer_expr ~map gen psi delta =
  let open Terms in
  function
  | ExprPat p -> infer_term ~map gen psi delta (term_of_pat p)
  | ExprLet { p_1; p_2; e } ->
      let { ty = ty_2; e_base = e_base_2; e_iso = e_iso_2 } =
        infer_term ~map gen psi delta (term_of_pat p_2)
      in
      let delta', es =
        let combined = combine_eq e_base_2 e_iso_2 in
        generalize_base ~disabled:true ~map gen psi delta combined p_1 ty_2
      in
      let { ty = ty_e; e_base = e_base_e; e_iso = e_iso_e } =
        infer_expr ~map gen psi delta' e
      in
      let es_base, es_iso = split_eq es in
      {
        ty = ty_e;
        e_base = e_base_2 @ e_base_e @ es_base;
        e_iso = e_iso_2 @ e_iso_e @ es_iso;
      }
  | ExprLetApp { p_1; omega; p_2; e } ->
      let { ty = ty_2; e_base = e_base_2; e_iso = e_iso_2 } =
        TermIsoApp (omega, term_of_pat p_2) |> infer_term ~map gen psi delta
      in
      let delta', es =
        let combined = combine_eq e_base_2 e_iso_2 in
        generalize_base ~disabled:true ~map gen psi delta combined p_1 ty_2
      in
      let { ty = ty_e; e_base = e_base_e; e_iso = e_iso_e } =
        infer_expr ~map gen psi delta' e
      in
      let es_base, es_iso = split_eq es in
      {
        ty = ty_e;
        e_base = e_base_2 @ e_base_e @ es_base;
        e_iso = e_iso_2 @ e_iso_e @ es_iso;
      }

and infer_branch ~map gen psi delta (p, e) =
  let delta' =
    Util.union ~weak:delta
      ~strong:(pat_gen gen p |> Util.IntMap.map (fun ty -> { forall = []; ty }))
  in
  let { ty = ty_p; e_base = e_base_p; e_iso = e_iso_p } =
    Terms.term_of_pat p |> infer_term ~map gen psi delta'
  in
  let { ty = ty_e; e_base = e_base_e; e_iso = e_iso_e } =
    infer_expr ~map gen psi delta' e
  in
  {
    ty = Types.IsoBiArrow (ty_p, ty_e);
    e_base = e_base_p @ e_base_e;
    e_iso = e_iso_p @ e_iso_e;
  }

and infer_iso ~map gen psi delta =
  let open Terms in
  function
  | IsoFix (phi, omega) ->
      let fresh = Types.IsoVar (Util.fresh gen) in
      let psi' = Util.IntMap.add phi { forall = []; ty = fresh } psi in
      let { ty; e_base; e_iso } = infer_iso ~map gen psi' delta omega in
      { ty; e_base; e_iso = (fresh, ty) :: e_iso }
  | IsoFun (phi, omega) ->
      let fresh = Types.IsoVar (Util.fresh gen) in
      let psi' = Util.IntMap.add phi { forall = []; ty = fresh } psi in
      let { ty; e_base; e_iso } = infer_iso ~map gen psi' delta omega in
      { ty = IsoArrow (fresh, ty); e_base; e_iso }
  | IsoVar phi ->
      let found =
        try Util.IntMap.find phi psi
        with _ ->
          Format.sprintf "undefined iso variable: %s" (Util.IntMap.find phi map)
          |> failwith
      in
      { ty = instantiate_iso gen found; e_base = []; e_iso = [] }
  | IsoApp (omega_1, omega_2) ->
      let { ty = ty_1; e_base = e_base_1; e_iso = e_iso_1 } =
        infer_iso ~map gen psi delta omega_1
      in
      let { ty = ty_2; e_base = e_base_2; e_iso = e_iso_2 } =
        infer_iso ~map gen psi delta omega_2
      in
      let e_base = e_base_1 @ e_base_2 in
      let e_iso = e_iso_1 @ e_iso_2 in
      let fresh = Types.IsoVar (Util.fresh gen) in
      { ty = fresh; e_base; e_iso = (ty_1, IsoArrow (ty_2, fresh)) :: e_iso }
  | IsoInv omega ->
      let { ty; e_base; e_iso } = infer_iso ~map gen psi delta omega in
      let fresh = Types.IsoVar (Util.fresh gen) in
      { ty = fresh; e_base; e_iso = (fresh, IsoInv ty) :: e_iso }
  | IsoCase List1.(b :: bs) ->
      let () =
        let Ortho.{ linear; left; right } = Ortho.auto (b :: bs) in
        if not linear then failwith "not linear"
        else if not left then failwith "overlapping (left)"
        else if not right then failwith "overlapping (right)"
        else ()
      in
      let { ty = ty_1; e_base = e_base_1; e_iso = e_iso_1 } =
        infer_branch ~map gen psi delta b
      in
      let es_base, es_iso =
        let folder (es_base, es_iso) b =
          let { ty; e_base; e_iso } = infer_branch ~map gen psi delta b in
          (e_base @ es_base, ((ty_1, ty) :: e_iso) @ es_iso)
        in
        List.fold_left folder (e_base_1, e_iso_1) bs
      in
      { ty = ty_1; e_base = es_base; e_iso = es_iso }

let auto ~map gen phi delta t =
  let { ty; e_base; e_iso } = infer_term ~map gen phi delta t in
  let combined = combine_eq e_base e_iso in
  let _substs_iso, substs_base = unify ~map combined in
  Types.subst_base_bulk substs_base ty

let init_ctx ts =
  let impl Surface.MInt.{ params; name; variants } =
    let folder (phi, delta) =
      let to_var a = Types.BaseVar a in
      function
      | c, None ->
          let ty =
            match params with
            | [] -> Types.BaseIdent name
            | a :: aa ->
                let l = List1.map to_var List1.(a :: aa) in
                Types.BaseApp (l, name)
          in
          let scm = { forall = params; ty } in
          (phi, (c, scm) :: delta)
      | c, Some a ->
          let ty =
            let a = Surface.base_silly a in
            match params with
            | [] -> Types.IsoBiArrow (a, Types.BaseIdent name)
            | b :: bb ->
                let l = List1.map to_var List1.(b :: bb) in
                Types.IsoBiArrow (a, Types.BaseApp (l, name))
          in
          let scm = { forall = params; ty } in
          ((c, scm) :: phi, delta)
    in
    List.fold_left folder ([], []) variants
  in
  let psi, delta =
    List.fold_left
      (fun (psi, delta) t ->
        let psi', delta' = impl t in
        (psi' @ psi, delta' @ delta))
      ([], []) ts
  in
  (Util.IntMap.of_list psi, Util.IntMap.of_list delta)

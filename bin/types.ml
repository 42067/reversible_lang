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

let rec is_fv_base v = function
  | BaseUnit | BaseIdent _ -> false
  | BaseVar v' -> v' = v
  | BaseProd l -> List2.fold_left (fun acc a -> acc || is_fv_base v a) false l
  | BaseApp (l, _) ->
      List1.fold_left (fun acc a -> acc || is_fv_base v a) false l

let rec is_fv_iso v = function
  | IsoBiArrow (a, b) -> is_fv_base v a || is_fv_base v b
  | IsoArrow (t_1, t_2) -> is_fv_iso v t_1 || is_fv_iso v t_2
  | IsoVar v' -> v' = v
  | IsoInv t -> is_fv_iso v t

let rec invert = function
  | IsoBiArrow (a, b) -> IsoBiArrow (b, a)
  | IsoArrow (t_1, t_2) -> IsoArrow (invert t_1, invert t_2)
  | IsoVar v -> IsoInv (IsoVar v)
  | IsoInv t -> t

let rec fv_base_list : base -> int list = function
  | BaseUnit | BaseIdent _ -> []
  | BaseVar v -> [ v ]
  | BaseProd l -> List2.map fv_base_list l |> List2.to_list |> List.flatten
  | BaseApp (l, _) -> List1.map fv_base_list l |> List1.to_list |> List.flatten

let fv_base a = fv_base_list a |> Util.IntSet.of_list

let rec fv_iso_list : iso -> int list = function
  | IsoBiArrow (a, b) -> fv_base_list a @ fv_base_list b
  | IsoArrow (t_1, t_2) -> fv_iso_list t_1 @ fv_iso_list t_2
  | IsoVar v -> [ v ]
  | IsoInv t -> fv_iso_list t

let fv_iso t = fv_iso_list t |> Util.IntSet.of_list

let rec fv_iso_sep_list = function
  | IsoBiArrow (a, b) ->
      fv_base_list a @ fv_base_list b |> List.map (fun v -> `Base v)
  | IsoArrow (t_1, t_2) -> fv_iso_sep_list t_1 @ fv_iso_sep_list t_2
  | IsoVar v -> [ `Iso v ]
  | IsoInv t -> fv_iso_sep_list t

let rec subst_base ((a, v) as s) = function
  | BaseUnit -> BaseUnit
  | BaseIdent x -> BaseIdent x
  | BaseVar v' -> if v' = v then a else BaseVar v'
  | BaseProd l -> BaseProd (List2.map (subst_base s) l)
  | BaseApp (l, x) -> BaseApp (List1.map (subst_base s) l, x)

let rec subst_iso ((t, v) as s) = function
  | IsoBiArrow (a, b) -> IsoBiArrow (a, b)
  | IsoArrow (t_1, t_2) -> IsoArrow (subst_iso s t_1, subst_iso s t_2)
  | IsoVar v' -> if v' = v then t else IsoVar v'
  | IsoInv t' -> IsoInv (subst_iso s t')

let rec subst_base_iso ((a, v) as s) = function
  | IsoBiArrow (a, b) -> IsoBiArrow (subst_base s a, subst_base s b)
  | IsoArrow (t_1, t_2) -> IsoArrow (subst_base_iso s t_1, subst_base_iso s t_2)
  | IsoVar v' -> IsoVar v'
  | IsoInv t -> IsoInv (subst_base_iso s t)

let subst_base_bulk substs_base a =
  List.fold_left (fun a s -> subst_base s a) a substs_base

let subst_iso_bulk substs_iso substs_base t =
  let m = List.fold_left (fun t s -> subst_iso s t) t substs_iso in
  List.fold_left (fun t s -> subst_base_iso s t) m substs_base

let rec pp_base map fmt =
  let f = Format.fprintf in
  let m x = Util.IntMap.find x map in
  function
  | BaseUnit -> f fmt "unit"
  | BaseIdent x -> f fmt "%s" (m x)
  | BaseVar v -> f fmt "'%s" (m v)
  | BaseProd List2.(a :: aa) ->
      begin match a with
      | BaseUnit | BaseIdent _ | BaseVar _ | BaseApp _ ->
          f fmt "%a" (pp_base map) a
      | BaseProd _ -> f fmt "(%a)" (pp_base map) a
      end;
      List.iter
        begin fun a ->
          match a with
          | BaseUnit | BaseIdent _ | BaseVar _ | BaseApp _ ->
              f fmt " * %a" (pp_base map) a
          | BaseProd _ -> f fmt " * (%a)" (pp_base map) a
        end
        (List1.to_list aa)
  | BaseApp (List1.(a :: aa), x) ->
      begin match aa with
      | [] ->
          begin match a with
          | BaseProd _ -> f fmt "(%a) %s" (pp_base map) a (m x)
          | BaseUnit | BaseIdent _ | BaseVar _ | BaseApp _ ->
              f fmt "%a %s" (pp_base map) a (m x)
          end
      | _ ->
          f fmt "(%a" (pp_base map) a;
          List.iter (f fmt ", %a" (pp_base map)) aa;
          f fmt ") %s" (m x)
      end

let create_map_base ts =
  let fv = List.map fv_base_list ts |> List.flatten in
  let map = Alpha.create_alphabet () in
  List.iter
    begin fun v ->
      let _ = Alpha.get_alphabet map v in
      ()
    end
    fv;
  Alpha.destruct_alphabet map

let rec pp_iso map fmt =
  let f = Format.fprintf in
  let m x = Util.IntMap.find x map in
  function
  | IsoBiArrow (a, b) -> f fmt "%a <-> %a" (pp_base map) a (pp_base map) b
  | IsoArrow (t_1, t_2) ->
      begin match t_1 with
      | IsoBiArrow _ | IsoArrow _ -> f fmt "(%a)" (pp_iso map) t_1
      | IsoVar _ | IsoInv _ -> f fmt "%a" (pp_iso map) t_1
      end;
      begin match t_2 with
      | IsoBiArrow _ -> f fmt " -> (%a)" (pp_iso map) t_2
      | IsoArrow _ | IsoVar _ | IsoInv _ -> f fmt " -> %a" (pp_iso map) t_2
      end
  | IsoVar v -> f fmt "'%s" (m v)
  | IsoInv t ->
      begin match t with
      | IsoBiArrow _ | IsoArrow _ -> f fmt "~(%a)" (pp_iso map) t
      | IsoVar _ | IsoInv _ -> f fmt "~%a" (pp_iso map) t
      end

let create_map_iso ts =
  let fv = List.map fv_iso_sep_list ts |> List.flatten in
  let map_base = Alpha.create_alphabet () in
  let map_iso = Alpha.create_alphabet () in
  List.iter
    begin function
      | `Base v ->
          let _ = Alpha.get_alphabet map_base v in
          ()
      | `Iso v ->
          let _ = Alpha.get_alphabet_upper map_iso v in
          ()
    end
    fv;
  Util.union
    ~weak:(Alpha.destruct_alphabet map_base)
    ~strong:(Alpha.destruct_alphabet map_iso)

let rec push_inv = function
  | (IsoBiArrow _ | IsoVar _ | IsoInv (IsoVar _)) as t -> t
  | IsoArrow (t_1, t_2) -> IsoArrow (push_inv t_1, push_inv t_2)
  | IsoInv ((IsoBiArrow _ | IsoArrow _) as t) -> invert t |> push_inv
  | IsoInv (IsoInv t) -> t |> push_inv

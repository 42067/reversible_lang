open Util

type any =
  | Unit
  | Product of any list
  | Named of string
  | BiArrow of { a : any; b : any }
  | Arrow of { a : any; b : any }
  | Var of int
  | Ctor of any list * string
  | Inverted of any

type equation = any * any
type subst = { what : int; into : any }
type inferred_pair = { a_v : any; a_e : any; e : equation list }
type inferred = { a : any; e : equation list }
type elt = Mono of any | Scheme of { forall : int list; a : any }
type context = elt StrMap.t

let rec subst (s : subst) : any -> any = function
  | Var x when x = s.what -> s.into
  | Product l -> Product (List.map (subst s) l)
  | BiArrow { a; b } -> BiArrow { a = subst s a; b = subst s b }
  | Arrow { a; b } -> Arrow { a = subst s a; b = subst s b }
  | Ctor (l, x) -> Ctor (List.map (subst s) l, x)
  | Inverted a -> Inverted (subst s a)
  | otherwise -> otherwise

let tvar_map (a : any list) : (int * int) list =
  let module IntMap = Map.Make (Int) in
  let map = ref IntMap.empty in
  let gen = Types.new_generator () in
  let get x =
    match IntMap.find_opt x !map with
    | Some _ -> ()
    | None -> map := IntMap.add x (Types.fresh gen) !map
  in
  let rec collect = function
    | Unit | Named _ -> ()
    | Product l | Ctor (l, _) -> List.iter collect l
    | BiArrow { a; b } | Arrow { a; b } ->
        collect a;
        collect b
    | Var i -> get i
    | Inverted a -> collect a
  in
  List.iter collect a;
  IntMap.to_list !map

let rec invert_iso_type : any -> any myresult = function
  | BiArrow { a; b } -> Ok (BiArrow { a = b; b = a })
  | Arrow { a; b } ->
      let** a = invert_iso_type a in
      let++ b = invert_iso_type b in
      Arrow { a; b }
  (* is this needed? *)
  | Inverted a -> Ok a
  | Var x -> Ok (Inverted (Var x))
  | otherwise -> Error (show_any [] otherwise ^ " is not an iso type")

and normalize_inv : any -> any myresult = function
  | BiArrow { a; b } -> Ok (BiArrow { a; b })
  | Arrow { a; b } ->
      let** a = normalize_inv a in
      let++ b = normalize_inv b in
      Arrow { a; b }
  | Inverted a -> invert_iso_type a
  | Var x -> Ok (Var x)
  | otherwise -> Error (show_any [] otherwise ^ " is not an iso type")

and base_of_any : any -> Types.base_type myresult = function
  | Unit -> Ok Types.Unit
  | Product l ->
      let++ l = List.map base_of_any l |> bind_all in
      Types.Product l
  | Named x -> Ok (Types.Named x)
  | Var x -> Ok (Types.Var ("'" ^ chars_of_int x))
  | Ctor (l, x) ->
      let++ l = List.map base_of_any l |> bind_all in
      let lmao : Types.base_type = Types.Ctor (l, x) in
      lmao
  | _ -> Error "base type is expected"

and iso_of_any : any -> Types.iso_type myresult = function
  | BiArrow { a; b } ->
      let** a = base_of_any a in
      let++ b = base_of_any b in
      Types.BiArrow { a; b }
  | Arrow { a; b } ->
      let** t_1 = iso_of_any a in
      let++ t_2 = iso_of_any b in
      Types.Arrow { t_1; t_2 }
  | Var x -> Ok (Types.Var ("'" ^ chars_of_int x))
  | Inverted (Var x) -> Ok (Types.Var ("~'" ^ chars_of_int x))
  | Inverted a ->
      let** inv = invert_iso_type a in
      iso_of_any inv
  | _ -> Error "iso type is expected"

(* todo: update *)
and show_any (map : (int * int) list) (a : any) : string =
  let a =
    List.fold_left
      (fun a (what, into) -> subst { what; into = Var into } a)
      a map
  in
  match base_of_any a with
  | Ok a -> Types.show_base_type a
  | Error _ -> begin
      match iso_of_any a with
      | Ok a -> Types.show_iso_type a
      | Error _ -> "unreachable (neither base or iso)"
    end

let show_elt : elt -> string = function
  | Mono a -> show_any [] a
  | Scheme { forall; a } ->
      "forall "
      ^ show_list (fun x -> "'" ^ chars_of_int x) forall
      ^ ". " ^ show_any [] a

let show_context (ctx : context) : string =
  StrMap.to_list ctx |> show_list (fun (k, e) -> k ^ " : " ^ show_elt e)

let show_equation ((a, b) : equation) : string =
  show_any [] a ^ " = " ^ show_any [] b

let show_equations : equation list -> string = show_list show_equation

let subst_in_context (s : subst) : context -> context =
  StrMap.map begin function
      | Mono a -> Mono (subst s a)
      | Scheme { forall; a } when List.for_all (( <> ) s.what) forall ->
          Scheme { forall; a = subst s a }
      | otherwise -> otherwise
    end

let subst_in_equations (s : subst) : equation list -> equation list =
  List.map (fun (a, b) -> (subst s a, subst s b))

let instantiate (gen : Types.generator) : elt -> any = function
  | Mono a -> a
  | Scheme { forall; a } ->
      List.fold_left
        (fun a what -> subst { what; into = Var (Types.fresh gen) } a)
        a forall

let rec occurs (x : int) : any -> bool = function
  | Product l | Ctor (l, _) -> List.exists (occurs x) l
  | BiArrow { a; b } | Arrow { a; b } -> occurs x a || occurs x b
  | Var y -> x = y
  | Inverted a -> occurs x a
  | _ -> false

let rec unify : equation list -> subst list myresult = function
  | [] -> Ok []
  | e :: e' -> begin
      match e with
      | a, b when a = b -> unify e'
      | Inverted a, Inverted b -> (a, b) :: e' |> unify
      | Inverted i, BiArrow { a; b } ->
          (i, BiArrow { a = b; b = a }) :: e' |> unify
      | BiArrow { a; b }, Inverted i ->
          (i, BiArrow { a = b; b = a }) :: e' |> unify
      | Inverted i, Arrow { a; b } ->
          let** a = invert_iso_type a in
          let** b = invert_iso_type b in
          (i, Arrow { a; b }) :: e' |> unify
      | Arrow { a; b }, Inverted i ->
          let** a = invert_iso_type a in
          let** b = invert_iso_type b in
          (i, Arrow { a; b }) :: e' |> unify
      | Var x, b when occurs x b |> not ->
          let s = { what = x; into = b } in
          let++ unified = subst_in_equations s e' |> unify in
          s :: unified
      | a, Var x when occurs x a |> not ->
          let s = { what = x; into = a } in
          let++ unified = subst_in_equations s e' |> unify in
          s :: unified
      | Product l, Product r when List.compare_lengths l r = 0 ->
          List.combine l r @ e' |> unify
      | BiArrow { a = a_1; b = b_1 }, BiArrow { a = a_2; b = b_2 } ->
          (a_1, a_2) :: (b_1, b_2) :: e' |> unify
      | Arrow { a = a_1; b = b_1 }, Arrow { a = a_2; b = b_2 } ->
          (a_1, a_2) :: (b_1, b_2) :: e' |> unify
      | Ctor (l_1, x_1), Ctor (l_2, x_2)
        when x_1 = x_2 && List.compare_lengths l_1 l_2 = 0 ->
          List.combine l_1 l_2 @ e' |> unify
      | a, b ->
          let map = tvar_map [ a; b ] in
          Error ("unable to unify " ^ show_any map a ^ " and " ^ show_any map b)
    end

let finalize ({ a; e } : inferred) : any myresult =
  let++ substs = unify e in
  List.fold_left (fun a s -> subst s a) a substs

(* todo: optimization *)
let find_generalizable (a : any) (ctx : context) : int list =
  let module IntSet = Set.Make (Int) in
  let rec find_in_any = function
    | Unit | Named _ -> IntSet.empty
    | Product l | Ctor (l, _) ->
        List.fold_left
          (fun acc a -> find_in_any a |> IntSet.union acc)
          IntSet.empty l
    | BiArrow { a; b } | Arrow { a; b } ->
        IntSet.union (find_in_any a) (find_in_any b)
    | Var x -> IntSet.singleton x
    | Inverted a -> find_in_any a
  in
  let find_in_context ctx =
    let f = function
      | Mono a -> find_in_any a
      | Scheme { forall; a } ->
          IntSet.diff (find_in_any a) (IntSet.of_list forall)
    in
    StrMap.fold (fun _ a acc -> f a |> IntSet.union acc) ctx IntSet.empty
  in
  IntSet.diff (find_in_any a) (find_in_context ctx) |> IntSet.to_list

let rec extract_named (gen : Types.generator) (v : Types.value) : any StrMap.t =
  match v with
  | Var x ->
      let var = Var (Types.fresh gen) in
      StrMap.singleton x var
  | Unit | Ctor _ -> StrMap.empty
  | Cted { v; _ } -> extract_named gen v
  | Tuple l -> union_list (List.map (extract_named gen) l)

let invert_pairs (pairs : (Types.value * Types.expr) list) :
    (Types.value * Types.expr) list =
  let rec invert_expr (e : Types.expr) (acc : Types.expr) =
    match e with
    | Value v -> (v, acc)
    | Let { p_1; omega; p_2; e } ->
        Let { p_1 = p_2; omega = Invert omega; p_2 = p_1; e = acc }
        |> invert_expr e
    | LetVal { p; v; e } -> LetVal { p = v; v = p; e = acc } |> invert_expr e
  in
  let invert_pair (v, e) = invert_expr e (Value v) in
  List.map invert_pair pairs

let check_pair ((v, e) : Types.value * Types.expr) : unit myresult =
  let v', e' = Ortho.convert_pair (v, e) in
  let msg =
    lazy
      (" in branch " ^ Types.show_value v' ^ " <->\n  " ^ Types.show_expr e'
     ^ "\nsource: " ^ Types.show_value v ^ " <->\n  " ^ Types.show_expr e)
  in

  (* string -> bool (consumed or not) *)
  let set = ref StrMap.empty in
  let add x = set := StrMap.add x false !set in
  let consume x =
    match StrMap.find_opt x !set with
    | Some false -> Ok (set := StrMap.add x true !set)
    | Some true -> Error (x ^ " is already consumed" ^ Lazy.force msg)
    | None -> Error (x ^ " is not in context" ^ Lazy.force msg)
  in
  let ensure_existence_nonconsumed x =
    match StrMap.find_opt x !set with
    | Some false -> Ok ()
    | Some true -> Error (x ^ " is already consumed" ^ Lazy.force msg)
    | None -> Error (x ^ " is not in context" ^ Lazy.force msg)
  in
  let rec collect_in_value : Types.value -> unit = function
    | Types.Cted { v; _ } -> collect_in_value v
    | Types.Unit -> ()
    | Types.Var x -> add x
    | Types.Ctor _ -> ()
    | Types.Tuple l -> List.iter collect_in_value l
  in
  let rec check_in_value : Types.value -> unit myresult = function
    | Types.Cted { v; _ } -> check_in_value v
    | Types.Unit -> Ok ()
    | Types.Var x -> ensure_existence_nonconsumed x
    | Types.Ctor _ -> Ok ()
    | Types.Tuple l ->
        let++ _ = List.map check_in_value l |> bind_all in
        ()
  in
  let rec check_in_expr : Types.expr -> _ = function
    | Types.Let { p_1; p_2; e; _ } | Types.LetVal { p = p_1; v = p_2; e } ->
        let vars = Types.collect_vars p_2 in
        let** _ = List.map consume vars |> bind_all in
        collect_in_value p_1;
        check_in_expr e
    | Types.Value v -> check_in_value v
  in
  collect_in_value v';
  check_in_expr e'

let generalize_iso (e : equation list) (ctx : context) (phi : string) (a : any)
    : context myresult =
  let** substs = unify e in
  let u = List.fold_left (fun a s -> subst s a) a substs in
  let** _ = iso_of_any u in
  let++ u_show = normalize_inv u in
  let name =
    if 12 < String.length phi then String.sub phi 0 9 ^ "..."
    else Printf.sprintf "%-12s" phi
  in
  "| " ^ name ^ " : " ^ show_any (tvar_map [ u_show ]) u_show
  |> boldpurple |> print_endline;
  let ctx = List.fold_left (fun ctx s -> subst_in_context s ctx) ctx substs in
  let generalized = Scheme { forall = find_generalizable u ctx; a = u } in
  StrMap.add phi generalized ctx

let rec generalize ?(disabled = false) (e : equation list) (ctx : context)
    (p : Types.value) (a : any) (gen : Types.generator) :
    (context * equation list) myresult =
  let** substs = unify e in
  let u = List.fold_left (fun a s -> subst s a) a substs in
  let** _ = base_of_any u in
  let ctx = List.fold_left (fun ctx s -> subst_in_context s ctx) ctx substs in
  let forall = find_generalizable u ctx in

  let extracted = extract_named gen p in
  let ctx = union ~weak:ctx ~strong:(StrMap.map (fun x -> Mono x) extracted) in
  let** { a = a'; e = e' } = infer_term (Types.term_of_value p) gen ctx in
  let es = (a', u) :: e' in
  let++ substs = unify es in
  let generalized =
    StrMap.map
      (fun a ->
        let a = List.fold_left (fun a s -> subst s a) a substs in
        if disabled then Mono a else Scheme { forall; a })
      extracted
  in

  (union ~weak:ctx ~strong:generalized, es)

and infer_pair (gen : Types.generator) (ctx : context)
    ((v, e) : Types.value * Types.expr) : inferred_pair myresult =
  let ctx =
    union ~weak:ctx ~strong:(extract_named gen v |> StrMap.map (fun x -> Mono x))
  in
  let** { a = a_v; e = e_v } = infer_term (Types.term_of_value v) gen ctx in
  let++ { a = a_e; e = e_e } = infer_expr e gen ctx in
  { a_v; a_e; e = e_v @ e_e }

and infer_term (t : Types.term) (gen : Types.generator) (ctx : context) :
    inferred myresult =
  match t with
  | Unit -> Ok { a = Unit; e = [] }
  | Var x | Ctor x ->
      let++ elt = find_res x ctx in
      { a = instantiate gen elt; e = [] }
  | Tuple l ->
      let++ inferred = List.map (fun t -> infer_term t gen ctx) l |> bind_all in
      let product = List.map (fun { a; _ } -> a) inferred in
      let e = List.map (fun { e; _ } -> e) inferred |> List.flatten in
      { a = Product product; e }
  | App { omega; t } ->
      let** { a = a_1; e = e_1 } = infer_iso omega gen ctx in
      let++ { a = a_2; e = e_2 } = infer_term t gen ctx in
      let e = e_1 @ e_2 in
      let fresh = Var (Types.fresh gen) in
      { a = fresh; e = (a_1, BiArrow { a = a_2; b = fresh }) :: e }
  | Cted { c; t } ->
      let** elt = find_res c ctx in
      let a_1 = instantiate gen elt in
      let++ { a = a_2; e } = infer_term t gen ctx in
      let fresh = Var (Types.fresh gen) in
      { a = fresh; e = (a_1, BiArrow { a = a_2; b = fresh }) :: e }
  | Let { p; t_1; t_2 } ->
      let** { a = a_1; e = e_1 } = infer_term t_1 gen ctx in
      let** ctx, es = generalize e_1 ctx p a_1 gen in
      let++ { a = a_2; e = e_2 } = infer_term t_2 gen ctx in
      { a = a_2; e = e_1 @ es @ e_2 }
  | LetIso { phi; omega; t } ->
      let** { a = a_1; e = e_1 } = infer_iso omega gen ctx in
      let** ctx = generalize_iso e_1 ctx phi a_1 in
      let++ { a = a_2; e = e_2 } = infer_term t gen ctx in
      { a = a_2; e = e_1 @ e_2 }

and infer_expr (expr : Types.expr) (gen : Types.generator) (ctx : context) :
    inferred myresult =
  match expr with
  | Value v -> infer_term (Types.term_of_value v) gen ctx
  | Let { p_1; omega; p_2; e = expr } ->
      let t_1 = Types.App { omega; t = Types.term_of_value p_2 } in
      let** { a = a_1; e = e_1 } = infer_term t_1 gen ctx in
      let** ctx, es = generalize ~disabled:true e_1 ctx p_1 a_1 gen in
      let++ { a = a_2; e = e_2 } = infer_expr expr gen ctx in
      { a = a_2; e = e_1 @ es @ e_2 }
  | LetVal { p; v; e = expr } ->
      let** { a = a_1; e = e_1 } = infer_term (Types.term_of_value v) gen ctx in
      let** ctx, es = generalize ~disabled:true e_1 ctx p a_1 gen in
      let++ { a = a_2; e = e_2 } = infer_expr expr gen ctx in
      { a = a_2; e = e_1 @ es @ e_2 }

and infer_iso (omega : Types.iso) (gen : Types.generator) (ctx : context) :
    inferred myresult =
  match omega with
  | Pairs p ->
      let check_orth p =
        let** _ = List.map (fun (v, e) -> check_pair (v, e)) p |> bind_all in
        let left = List.map (fun (v, _) -> v) p in
        let right = List.map (fun (_, e) -> Types.value_of_expr e) p in
        let** () = for_all_pairs Ortho.is_orthogonal left in
        for_all_pairs Ortho.is_orthogonal right
      in
      let infer p =
        let++ pairs = List.map (infer_pair gen ctx) p |> bind_all in
        let types_v, types_e =
          List.map (fun { a_v; a_e; _ } -> (a_v, a_e)) pairs |> List.split
        in
        let es =
          List.map (fun ({ e; _ } : inferred_pair) -> e) pairs |> List.flatten
        in
        let a = List.hd types_v in
        let b = List.hd types_e in
        let types_v' = List.drop 1 types_v @ [ a ] in
        let types_e' = List.drop 1 types_e @ [ b ] in
        let e_v = List.map2 (fun a b -> (a, b)) types_v types_v' in
        let e_e = List.map2 (fun a b -> (a, b)) types_e types_e' in
        { a = BiArrow { a; b }; e = e_v @ e_e @ es }
      in
      let** () = check_orth p in
      let** () = invert_pairs p |> check_orth in
      infer p
  | Fix { phi; omega } ->
      let fresh = Var (Types.fresh gen) in
      let ctx = StrMap.add phi (Mono fresh) ctx in
      let++ { a; e } = infer_iso omega gen ctx in
      { a; e = (fresh, a) :: e }
  | Lambda { psi; omega } ->
      let fresh = Var (Types.fresh gen) in
      let ctx = StrMap.add psi (Mono fresh) ctx in
      let++ { a; e } = infer_iso omega gen ctx in
      { a = Arrow { a = fresh; b = a }; e }
  | Var omega ->
      let++ elt = find_res omega ctx in
      { a = instantiate gen elt; e = [] }
  | App { omega_1; omega_2 } ->
      let** { a = a_1; e = e_1 } = infer_iso omega_1 gen ctx in
      let++ { a = a_2; e = e_2 } = infer_iso omega_2 gen ctx in
      let e = e_1 @ e_2 in
      let fresh = Var (Types.fresh gen) in
      { a = fresh; e = (a_1, Arrow { a = a_2; b = fresh }) :: e }
  | Invert omega ->
      let++ { a; e } = infer_iso omega gen ctx in
      let fresh = Var (Types.fresh gen) in
      { a = fresh; e = (fresh, Inverted a) :: e }

let rec any_of_base ~(var_map : int StrMap.t) ~(arity_map : int StrMap.t) :
    Types.base_type -> any myresult = function
  | Unit -> Ok Unit
  | Product l ->
      let++ l = List.map (any_of_base ~var_map ~arity_map) l |> bind_all in
      Product l
  | Named x ->
      let** arity_actual = find_res x arity_map in
      if arity_actual = 0 then Ok (Named x)
      else
        Error
          (x ^ " expects arity of " ^ string_of_int arity_actual
         ^ " but provided with 0")
  | Var x ->
      let++ x = find_res x var_map in
      Var x
  | Ctor (l, x) ->
      let** arity_actual = find_res x arity_map in
      let arity_found = List.length l in
      if arity_actual = arity_found then
        let++ l = List.map (any_of_base ~var_map ~arity_map) l |> bind_all in
        Ctor (l, x)
      else
        Error
          (x ^ " expects arity of " ^ string_of_int arity_actual
         ^ " but provided with " ^ string_of_int arity_found)

let arity_map (defs : Types.typedef list) : int StrMap.t =
  List.fold_left
    (fun acc Types.{ vars; t; _ } -> StrMap.add t (List.length vars) acc)
    StrMap.empty defs

let build_ctx (gen : Types.generator) (defs : Types.typedef list) :
    context myresult =
  let arity_map = arity_map defs in
  let occured = ref StrSet.empty in
  let register t =
    match StrSet.find_opt t !occured with
    | None -> Ok (occured := StrSet.add t !occured)
    | Some _ -> Error (t ^ " is defined more than once")
  in
  let build Types.{ vars; t; vs } =
    let** () = register t in
    let var_map =
      List.fold_left
        (fun acc x -> StrMap.add x (Types.fresh gen) acc)
        StrMap.empty vars
    in
    let** forall = List.map (fun x -> find_res x var_map) vars |> bind_all in
    let inner =
      match forall with
      | [] -> Named t
      | _ -> Ctor (List.map (fun x -> Var x) forall, t)
    in
    let f = function
      | Types.Value x -> Ok (x, Scheme { forall; a = inner })
      | Types.Iso { c; a } ->
          let++ a = any_of_base ~var_map ~arity_map a in
          let inner = BiArrow { a; b = inner } in
          (c, Scheme { forall; a = inner })
    in
    List.map f vs |> bind_all
  in
  let++ acc = List.map build defs |> bind_all in
  acc |> List.flatten |> StrMap.of_list

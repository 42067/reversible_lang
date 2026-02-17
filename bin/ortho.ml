open Types
open Util

type equation = value * value
type subst = { what : string; into : value }

let rec subst (s : subst) : value -> value = function
  | Unit -> Unit
  | Var x when x = s.what -> s.into
  | Var x -> Var x
  | Ctor x -> Ctor x
  | Cted { c; v } -> Cted { c; v = subst s v }
  | Tuple l -> Tuple (List.map (subst s) l)

let subst_in_equations (s : subst) : equation list -> equation list =
  List.map (fun (a, b) -> (subst s a, subst s b))

let rec subst_in_expr ~(what : string) ~(into : string) : expr -> expr =
  function
  | Value v -> Value (subst { what; into = Var into } v)
  | Let { p_1; omega; p_2; e } when contains_value what p_1 ->
      Let { p_1; omega; p_2 = subst { what; into = Var into } p_2; e }
  | Let { p_1; omega; p_2; e } ->
      Let
        {
          p_1;
          omega;
          p_2 = subst { what; into = Var into } p_2;
          e = subst_in_expr ~what ~into e;
        }
  | LetVal { p; v; e } when contains_value what p ->
      LetVal { p; v = subst { what; into = Var into } v; e }
  | LetVal { p; v; e } ->
      LetVal
        {
          p;
          v = subst { what; into = Var into } v;
          e = subst_in_expr ~what ~into e;
        }

let rec occurs (x : string) : value -> bool = function
  | Unit -> false
  | Var y -> x = y
  | Ctor _ -> false
  | Cted { v; _ } -> occurs x v
  | Tuple l -> List.exists (occurs x) l

let is_free (x : string) (v : value) : bool = not (occurs x v)

let rec unify : equation list -> (subst list, unit) result = function
  | [] -> Ok []
  | e :: e' -> begin
      match e with
      | a, b when a = b -> unify e'
      | Var x, b when is_free x b ->
          let s = { what = x; into = b } in
          let++ unified = subst_in_equations s e' |> unify in
          s :: unified
      | a, Var x when is_free x a ->
          let s = { what = x; into = a } in
          let++ unified = subst_in_equations s e' |> unify in
          s :: unified
      | Cted { c = c_1; v = v_1 }, Cted { c = c_2; v = v_2 } when c_1 = c_2 ->
          (v_1, v_2) :: e' |> unify
      | Tuple a, Tuple b when List.compare_lengths a b = 0 ->
          List.combine a b @ e' |> unify
      | _ -> Error ()
    end

let rec reduce : subst list -> subst list = function
  | [] -> []
  | s :: s' ->
      let s' =
        List.map (fun { what; into } -> { what; into = subst s into }) s'
        |> reduce
      in
      s :: s'

let is_orthogonal (u : value) (v : value) : unit myresult =
  let gen = new_generator () in

  let convert_value v =
    let fresh_name () =
      let lmao : value = Var (chars_of_int (fresh gen)) in
      lmao
    in
    let vars = collect_vars v |> StrSet.of_list in
    let substs =
      StrSet.fold
        (fun x substs -> { what = x; into = fresh_name () } :: substs)
        vars []
    in
    List.fold_left (fun v s -> subst s v) v substs
  in

  let u' = convert_value u in
  let v' = convert_value v in

  match unify [ (u', v') ] with
  | Ok l ->
      let msg =
        show_value u' ^ " and " ^ show_value v' ^ " are not orthogonal"
        ^ "\nexample: "
        ^ show_list
            (fun { what; into } -> what ^ " = " ^ show_value into)
            (List.rev l |> reduce
            |> List.sort (fun l r -> compare l.what r.what))
        ^ "\nsource: " ^ show_value u ^ " and " ^ show_value v
      in
      Error msg
  | Error () -> Ok ()

let convert_pair ((v, e) : value * expr) : value * expr =
  let gen = new_generator () in

  (* ' is needed for creating fresh names due to the let exprs *)
  let fresh_name () = "'" ^ chars_of_int (fresh gen) in
  let substs = collect_vars v |> List.map (fun x -> (x, fresh_name ())) in
  let v =
    List.fold_left
      (fun v (what, into) -> subst { what; into = Var into } v)
      v substs
  in
  let e =
    List.fold_left (fun e (what, into) -> subst_in_expr ~what ~into e) e substs
  in

  let rec process_expr : expr -> expr =
    let process p_1 e =
      let vars = collect_vars p_1 in
      let substs = List.map (fun x -> (x, fresh_name ())) vars in
      let p_1 =
        List.fold_left
          (fun p (what, into) -> subst { what; into = Var into } p)
          p_1 substs
      in
      let e =
        List.fold_left
          (fun e (what, into) -> subst_in_expr ~what ~into e)
          e substs
      in
      let e = process_expr e in
      (p_1, e)
    in
    function
    | Value e -> Value e
    | Let { p_1; omega; p_2; e } ->
        let p_1, e = process p_1 e in
        Let { p_1; omega; p_2; e }
    | LetVal { p; v; e } ->
        let p, e = process p e in
        LetVal { p; v; e }
  in

  (v, process_expr e)

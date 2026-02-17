open Util

type base_type =
  | Unit
  | Product of base_type list
  | Named of string
  | Var of string
  | Ctor of base_type list * string

type iso_type =
  | BiArrow of { a : base_type; b : base_type }
  | Arrow of { t_1 : iso_type; t_2 : iso_type }
  | Var of string

type value =
  | Unit
  | Var of string
  | Ctor of string
  | Cted of { c : string; v : value }
  | Tuple of value list

type expr =
  | Value of value
  | Let of { p_1 : value; omega : iso; p_2 : value; e : expr }
  | LetVal of { p : value; v : value; e : expr }

and iso =
  | Pairs of (value * expr) list
  | Fix of { phi : string; omega : iso }
  | Lambda of { psi : string; omega : iso }
  | Var of string
  | App of { omega_1 : iso; omega_2 : iso }
  | Invert of iso

type term =
  | Unit
  | Var of string
  | Ctor of string
  | Cted of { c : string; t : term }
  | Tuple of term list
  | App of { omega : iso; t : term }
  | Let of { p : value; t_1 : term; t_2 : term }
  | LetIso of { phi : string; omega : iso; t : term }

type expr_intermediate =
  | IValue of term
  | ILet of { p_1 : value; p_2 : term; e : expr_intermediate }

type variant = Value of string | Iso of { c : string; a : base_type }
type typedef = { vars : string list; t : string; vs : variant list }
type program = { ts : typedef list; t : term }
type generator = { mutable i : int }

let rec term_of_value : value -> term = function
  | Unit -> Unit
  | Var x -> Var x
  | Ctor x -> Ctor x
  | Cted { c; v } -> Cted { c; t = term_of_value v }
  | Tuple l -> Tuple (List.map term_of_value l)

let rec term_of_expr : expr -> term = function
  | Value v -> term_of_value v
  | Let { p_1; omega; p_2; e } ->
      Let
        {
          p = p_1;
          t_1 = App { omega; t = term_of_value p_2 };
          t_2 = term_of_expr e;
        }
  | LetVal { p; v; e } -> Let { p; t_1 = term_of_value v; t_2 = term_of_expr e }

let rec value_of_expr : expr -> value = function
  | Value v -> v
  | Let { e; _ } -> value_of_expr e
  | LetVal { e; _ } -> value_of_expr e

let rec contains_value (what : string) : value -> bool = function
  | Unit -> false
  | Var x -> x = what
  | Ctor _ -> false
  | Cted { v; _ } -> contains_value what v
  | Tuple l -> List.exists (contains_value what) l

let contains_pairs (what : string) (pairs : (value * expr) list) : bool =
  List.exists (fun (v, _) -> contains_value what v) pairs

let rec lambdas_of_params : string list -> iso -> iso = function
  | [] -> fun omega -> omega
  | psi :: tl -> fun omega -> Lambda { psi; omega = lambdas_of_params tl omega }

let rec is_list_value : value -> bool = function
  | Cted { c = "Cons"; v = Tuple [ _; v ] } -> is_list_value v
  | Ctor "Nil" -> true
  | _ -> false

let rec is_list_term : term -> bool = function
  | Cted { c = "Cons"; t = Tuple [ _; t ] } -> is_list_term t
  | Ctor "Nil" -> true
  | _ -> false

let rec is_int_value : value -> bool = function
  | Cted { c = "S"; v } -> is_int_value v
  | Ctor "Z" -> true
  | _ -> false

let rec is_int_term : term -> bool = function
  | Cted { c = "S"; t } -> is_int_term t
  | Ctor "Z" -> true
  | _ -> false

let rec show_base_type : base_type -> string = function
  | Unit -> "unit"
  | Product l ->
      let lmao = function
        | Product l -> "(" ^ show_base_type (Product l) ^ ")"
        | otherwise -> show_base_type otherwise
      in
      show_listlike lmao ~left:"" ~delim:" * " ~right:"" l
  | Named x | Var x -> x
  | Ctor ([], _) -> "unreachable (type constructor with Z arity)"
  | Ctor ([ (Product _ as x) ], a) -> "(" ^ show_base_type x ^ ") " ^ a
  | Ctor ([ x ], a) -> show_base_type x ^ " " ^ a
  | Ctor (l, a) -> show_tuple show_base_type l ^ " " ^ a

let rec show_iso_type : iso_type -> string = function
  | BiArrow { a; b } -> show_base_type a ^ " <-> " ^ show_base_type b
  | Arrow { t_1 = Var _ as t_1; t_2 = BiArrow _ as t_2 } ->
      show_iso_type t_1 ^ " -> (" ^ show_iso_type t_2 ^ ")"
  | Arrow { t_1 = Var _ as t_1; t_2 } ->
      show_iso_type t_1 ^ " -> " ^ show_iso_type t_2
  | Arrow { t_1; t_2 = BiArrow _ as t_2 } ->
      "(" ^ show_iso_type t_1 ^ ") -> (" ^ show_iso_type t_2 ^ ")"
  | Arrow { t_1; t_2 } -> "(" ^ show_iso_type t_1 ^ ") -> " ^ show_iso_type t_2
  | Var x -> x

let rec show_value : value -> string = function
  | Unit -> "()"
  | Ctor "Z" -> "0"
  | Ctor "Nil" -> "[]"
  | Ctor x | Var x -> x
  | what when is_int_value what -> begin
      let rec lmao acc : value -> int = function
        | Cted { v; _ } -> lmao (acc + 1) v
        | _ -> acc
      in
      lmao 0 what |> string_of_int
    end
  | Cted { c = "Cons"; v = Tuple [ v_1; v_2 ] } as v ->
      if is_list_value v then
        let rec lmao : value -> string = function
          | Cted { c = "Cons"; v = Tuple [ v_1; v_2 ] } ->
              "; " ^ show_value v_1 ^ lmao v_2
          | Ctor "Nil" -> ""
          | otherwise -> "; " ^ show_value otherwise
        in
        "[" ^ show_value v_1 ^ lmao v_2 ^ "]"
      else
        let rec lmao : value -> string = function
          | Cted { c = "Cons"; v = Tuple [ v_1; v_2 ] } ->
              " :: " ^ show_value v_1 ^ lmao v_2
          | otherwise -> " :: " ^ show_value otherwise
        in
        show_value v_1 ^ lmao v_2
  | Tuple l -> show_tuple show_value l
  | Cted { c; v = Cted _ as v } when is_int_value v || is_list_value v ->
      c ^ " " ^ show_value v
  | Cted { c; v = Cted _ as v } -> c ^ " (" ^ show_value v ^ ")"
  | Cted { c; v } -> c ^ " " ^ show_value v

let rec show_expr : expr -> string = function
  | Value v -> show_value v
  | Let
      {
        p_1;
        omega = (Pairs _ | Fix _ | Lambda _) as omega;
        p_2 = Cted _ as p_2;
        e;
      }
    when is_int_value p_2 || is_list_value p_2 ->
      "let " ^ show_value p_1 ^ " = {" ^ show_iso omega ^ "} " ^ show_value p_2
      ^ " in\n  " ^ show_expr e
  | Let
      {
        p_1;
        omega = (Pairs _ | Fix _ | Lambda _) as omega;
        p_2 = Cted _ as p_2;
        e;
      } ->
      "let " ^ show_value p_1 ^ " = {" ^ show_iso omega ^ "} (" ^ show_value p_2
      ^ ") in\n  " ^ show_expr e
  | Let { p_1; omega = (Pairs _ | Fix _ | Lambda _) as omega; p_2; e } ->
      "let " ^ show_value p_1 ^ " = {" ^ show_iso omega ^ "} " ^ show_value p_2
      ^ " in\n  " ^ show_expr e
  | Let { p_1; omega; p_2 = Cted _ as p_2; e }
    when is_int_value p_2 || is_list_value p_2 ->
      "let " ^ show_value p_1 ^ " = " ^ show_iso omega ^ " " ^ show_value p_2
      ^ " in\n  " ^ show_expr e
  | Let { p_1; omega; p_2 = Cted _ as p_2; e } ->
      "let " ^ show_value p_1 ^ " = " ^ show_iso omega ^ " (" ^ show_value p_2
      ^ ") in\n  " ^ show_expr e
  | Let { p_1; omega; p_2; e } ->
      "let " ^ show_value p_1 ^ " = " ^ show_iso omega ^ " " ^ show_value p_2
      ^ " in\n  " ^ show_expr e
  | LetVal { p; v; e } ->
      "let " ^ show_value p ^ " = " ^ show_value v ^ " in\n  " ^ show_expr e

and show_pairs (pairs : (value * expr) list) : string =
  List.fold_left
    (fun acc (v, e) -> acc ^ "\n  | " ^ show_value v ^ " <-> " ^ show_expr e)
    "case" pairs

and show_iso : iso -> string = function
  | Pairs p -> show_pairs p
  | Fix { phi; omega; _ } -> "fix " ^ phi ^ ". " ^ show_iso omega
  | Lambda { psi; omega; _ } -> "fun " ^ psi ^ " -> " ^ show_iso omega
  | Var omega -> omega
  | App { omega_1; omega_2 = Var _ as omega_2 } ->
      show_iso omega_1 ^ " " ^ show_iso omega_2
  | App { omega_1; omega_2 } -> show_iso omega_1 ^ " {" ^ show_iso omega_2 ^ "}"
  | Invert (Var _ as omega) -> "inv " ^ show_iso omega
  | Invert omega -> "inv {" ^ show_iso omega ^ "}"

let show_pairs_lhs (v : value) (pairs : (value * expr) list) : string =
  let init = "match " ^ show_value v ^ " with" in
  List.fold_left
    (fun acc (v, _) -> acc ^ "\n  | " ^ show_value v ^ " <-> ...")
    init pairs

let rec show_term : term -> string = function
  | Unit -> "()"
  | Ctor "Z" -> "0"
  | Ctor "Nil" -> "[]"
  | Var x | Ctor x -> x
  | Tuple l -> show_tuple show_term l
  | what when is_int_term what -> begin
      let rec lmao acc = function
        | Cted { t; _ } -> lmao (acc + 1) t
        | _ -> acc
      in
      lmao 0 what |> string_of_int
    end
  | Cted { c = "Cons"; t = Tuple [ t_1; t_2 ] } as t ->
      if is_list_term t then
        let rec lmao = function
          | Cted { c = "Cons"; t = Tuple [ t_1; t_2 ] } ->
              "; " ^ show_term t_1 ^ lmao t_2
          | Ctor "Nil" -> ""
          | otherwise -> "; " ^ show_term otherwise
        in
        "[" ^ show_term t_1 ^ lmao t_2 ^ "]"
      else
        let rec lmao = function
          | Cted { c = "Cons"; t = Tuple [ t_1; t_2 ] } ->
              " :: " ^ show_term t_1 ^ lmao t_2
          | otherwise -> " :: " ^ show_term otherwise
        in
        show_term t_1 ^ lmao t_2
  | Cted { c; t } when is_int_term t || is_list_term t -> c ^ " " ^ show_term t
  | Cted { c; t = (Cted _ | App _ | Let _ | LetIso _) as t } ->
      c ^ " (" ^ show_term t ^ ")"
  | Cted { c; t } -> c ^ " " ^ show_term t
  | App { omega = (Pairs _ | Fix _ | Lambda _) as omega; t }
    when is_int_term t || is_list_term t ->
      "{" ^ show_iso omega ^ "} " ^ show_term t
  | App
      {
        omega = (Pairs _ | Fix _ | Lambda _) as omega;
        t = (Cted _ | App _ | Let _ | LetIso _) as t;
      } ->
      "{" ^ show_iso omega ^ "} (" ^ show_term t ^ ")"
  | App { omega = (Pairs _ | Fix _ | Lambda _) as omega; t } ->
      "{" ^ show_iso omega ^ "} " ^ show_term t
  | App { omega; t } when is_int_term t || is_list_term t ->
      show_iso omega ^ " " ^ show_term t
  | App { omega; t = (Cted _ | App _ | Let _ | LetIso _) as t } ->
      show_iso omega ^ " (" ^ show_term t ^ ")"
  | App { omega; t } -> show_iso omega ^ " " ^ show_term t
  | Let { p; t_1; t_2 } ->
      "let " ^ show_value p ^ " = " ^ show_term t_1 ^ "\nin\n\n" ^ show_term t_2
  | LetIso { phi; omega; t } ->
      "let iso " ^ phi ^ " = " ^ show_iso omega ^ "\nin\n\n" ^ show_term t

let rec nat_of_int (n : int) : value =
  if n < 1 then Ctor "Z" else Cted { c = "S"; v = nat_of_int (n - 1) }

let rec build_storage (default : 'a) : value -> 'a option StrMap.t = function
  | Unit -> StrMap.empty
  | Var x -> StrMap.singleton x None
  | Ctor _ -> StrMap.empty
  | Cted { v; _ } -> build_storage default v
  | Tuple l ->
      List.map (build_storage default) l
      |> List.fold_left
           (StrMap.union (fun _ _ _ -> Some (Some default)))
           StrMap.empty

let collect_vars (v : value) : string list =
  let rec collect : value -> string list = function
    | Unit -> []
    | Var x -> [ x ]
    | Ctor _ -> []
    | Cted { v; _ } -> collect v
    | Tuple l -> List.map collect l |> List.flatten
  in
  collect v |> List.sort_uniq compare

let new_generator () : generator = { i = 0 }

let fresh (gen : generator) : int =
  let i = gen.i in
  gen.i <- i + 1;
  i

let rec expand (gen : generator) :
    term -> ((value * iso * value) list * value) myresult = function
  | Unit -> Ok ([], Unit)
  | Var x -> Ok ([], Var x)
  | Ctor x -> Ok ([], Ctor x)
  | Tuple t ->
      let++ l = List.map (expand gen) t |> bind_all in
      let l, t = List.fold_left_map (fun l (l', v) -> (l @ l', v)) [] l in
      let tuple : value = Tuple t in
      (l, tuple)
  | Cted { c; t } ->
      let++ l, v = expand gen t in
      let silly : value = Cted { c; v } in
      (l, silly)
  | App { omega; t } ->
      let++ l, v = expand gen t in
      let name : value = Var ("_" ^ chars_of_int (fresh gen)) in
      ((name, omega, v) :: l, name)
  | Let _ -> Error "nested let is not supported (yet)"
  | LetIso _ -> Error "nested iso binding is not supported (yet)"

let rec expand_expr (gen : generator) : expr_intermediate -> expr myresult =
  function
  | IValue t ->
      let++ l, v = expand gen t in
      let init : expr = Value v in
      List.fold_left
        (fun e (p_1, omega, p_2) : expr -> Let { p_1; omega; p_2; e })
        init l
  | ILet { p_1; p_2; e } ->
      let** l, v = expand gen p_2 in
      let++ e = expand_expr gen e in
      let init = LetVal { p = p_1; v; e } in
      List.fold_left
        (fun e (p_1, omega, p_2) : expr -> Let { p_1; omega; p_2; e })
        init l

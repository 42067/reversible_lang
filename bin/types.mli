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
type generator

val term_of_value : value -> term
val term_of_expr : expr -> term
val value_of_expr : expr -> value
val contains_value : string -> value -> bool
val contains_pairs : string -> (value * expr) list -> bool
val lambdas_of_params : string list -> iso -> iso
val is_list_value : value -> bool
val is_list_term : term -> bool
val is_int_value : value -> bool
val is_int_term : term -> bool
val show_base_type : base_type -> string
val show_iso_type : iso_type -> string
val show_value : value -> string
val show_expr : expr -> string
val show_pairs : (value * expr) list -> string
val show_iso : iso -> string
val show_pairs_lhs : value -> (value * expr) list -> string
val show_term : term -> string
val nat_of_int : int -> value
val build_storage : 'a -> value -> 'a option StrMap.t
val collect_vars : value -> string list
val new_generator : unit -> generator
val fresh : generator -> int
val expand : generator -> term -> ((value * iso * value) list * value) myresult
val expand_expr : generator -> expr_intermediate -> expr myresult

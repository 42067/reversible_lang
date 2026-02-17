open Types
open Util

type equation
type subst

val subst : subst -> value -> value
val subst_in_equations : subst -> equation list -> equation list
val subst_in_expr : what:string -> into:string -> expr -> expr
val occurs : string -> value -> bool
val is_free : string -> value -> bool
val unify : equation list -> (subst list, unit) result
val reduce : subst list -> subst list
val is_orthogonal : value -> value -> unit myresult
val convert_pair : value * expr -> value * expr

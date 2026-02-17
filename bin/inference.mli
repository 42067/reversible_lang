open Util

type any
type equation
type subst
type inferred_pair
type inferred
type elt
type context

val subst : subst -> any -> any
val tvar_map : any list -> (int * int) list
val invert_iso_type : any -> any myresult
val normalize_inv : any -> any myresult
val base_of_any : any -> Types.base_type myresult
val iso_of_any : any -> Types.iso_type myresult
val show_any : (int * int) list -> any -> string
val show_elt : elt -> string
val show_context : context -> string
val show_equation : equation -> string
val show_equations : equation list -> string
val subst_in_context : subst -> context -> context
val subst_in_equations : subst -> equation list -> equation list
val instantiate : Types.generator -> elt -> any
val occurs : int -> any -> bool
val unify : equation list -> subst list myresult
val finalize : inferred -> any myresult
val find_generalizable : any -> context -> int list
val extract_named : Types.generator -> Types.value -> any StrMap.t

val invert_pairs :
  (Types.value * Types.expr) list -> (Types.value * Types.expr) list

val check_pair : Types.value * Types.expr -> unit myresult

val generalize_iso :
  equation list -> context -> string -> any -> context myresult

val generalize :
  ?disabled:bool ->
  equation list ->
  context ->
  Types.value ->
  any ->
  Types.generator ->
  (context * equation list) myresult

val infer_pair :
  Types.generator ->
  context ->
  Types.value * Types.expr ->
  inferred_pair myresult

val infer_term : Types.term -> Types.generator -> context -> inferred myresult
val infer_expr : Types.expr -> Types.generator -> context -> inferred myresult
val infer_iso : Types.iso -> Types.generator -> context -> inferred myresult

val any_of_base :
  var_map:int StrMap.t ->
  arity_map:int StrMap.t ->
  Types.base_type ->
  any myresult

val arity_map : Types.typedef list -> int StrMap.t
val build_ctx : Types.generator -> Types.typedef list -> context myresult

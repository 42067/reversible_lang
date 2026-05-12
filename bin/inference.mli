type 'a subst
type 'a scheme
type 'a ctx
type 'a eq
type eq_combined
type 'a inferred

val subst_eq_base : Types.base subst -> eq_combined list -> eq_combined list
val subst_eq_iso : Types.iso subst -> eq_combined list -> eq_combined list
val instantiate_base : Util.generator -> Types.base scheme -> Types.base
val instantiate_iso : Util.generator -> Types.iso scheme -> Types.iso
val fv_phi : Types.iso scheme Util.IntMap.t -> Util.IntSet.t
val fv_delta : Types.base scheme Util.IntMap.t -> Util.IntSet.t
val subst_phi : Types.iso subst -> Types.iso ctx -> Types.iso ctx
val subst_phi_base : Types.base subst -> Types.iso ctx -> Types.iso ctx
val subst_delta : Types.base subst -> Types.base ctx -> Types.base ctx

val subst_phi_bulk :
  Types.iso subst list ->
  Types.base subst list ->
  Types.iso ctx ->
  Types.iso ctx

val subst_delta_bulk : Types.base subst list -> Types.base ctx -> Types.base ctx
val combine_eq : Types.base eq list -> Types.iso eq list -> eq_combined list
val split_eq : eq_combined list -> Types.base eq list * Types.iso eq list

val find_generalizable_base :
  Types.iso ctx -> Types.base ctx -> Types.base -> int list

val find_generalizable_iso :
  Types.iso ctx -> Types.base ctx -> Types.iso -> int list

val unify :
  map:string Util.IntMap.t ->
  eq_combined list ->
  Types.iso subst list * Types.base subst list

val pat_gen : Util.generator -> Terms.pat -> Types.base Util.IntMap.t

val generalize_iso :
  map:string Util.IntMap.t ->
  Types.iso ctx ->
  Types.base ctx ->
  eq_combined list ->
  int ->
  Types.iso ->
  Types.iso ctx

val generalize_base :
  ?disabled:bool ->
  map:string Util.IntMap.t ->
  Util.generator ->
  Types.iso ctx ->
  Types.base ctx ->
  eq_combined list ->
  Terms.pat ->
  Types.base ->
  Types.base ctx * eq_combined list

val infer_term :
  map:string Util.IntMap.t ->
  Util.generator ->
  Types.iso ctx ->
  Types.base ctx ->
  Terms.term ->
  Types.base inferred

val infer_expr :
  map:string Util.IntMap.t ->
  Util.generator ->
  Types.iso ctx ->
  Types.base ctx ->
  Terms.expr ->
  Types.base inferred

val infer_branch :
  map:string Util.IntMap.t ->
  Util.generator ->
  Types.iso ctx ->
  Types.base ctx ->
  Terms.pat * Terms.expr ->
  Types.iso inferred

val infer_iso :
  map:string Util.IntMap.t ->
  Util.generator ->
  Types.iso ctx ->
  Types.base ctx ->
  Terms.iso ->
  Types.iso inferred

val auto :
  map:string Util.IntMap.t ->
  Util.generator ->
  Types.iso ctx ->
  Types.base ctx ->
  Terms.term ->
  Types.base

val init_ctx : Surface.MInt.typedef list -> Types.iso ctx * Types.base ctx

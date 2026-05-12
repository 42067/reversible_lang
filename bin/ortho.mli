type subst
type eq
type ok = { linear : bool; left : bool; right : bool }

val subst_pat : subst -> Terms.pat -> Terms.pat
val unify : eq list -> subst list option
val is_ortho : Terms.pat * Terms.pat -> bool
val is_ortho_all_perm : Terms.pat list -> bool
val is_linear : Terms.pat * Terms.expr -> bool
val auto : (Terms.pat * Terms.expr) list -> ok

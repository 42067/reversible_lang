%{
  open Surface.MStr

  let rec nat_of_int_pat n =
    if n = 0 then PatCtor "Z"
             else PatApp ("S", nat_of_int_pat (n - 1))

  let rec nat_of_int_epat n =
    if n = 0 then EPatCtor "Z"
             else EPatCtorApp ("S", nat_of_int_epat (n - 1))

  let rec nat_of_int_term n =
    if n = 0 then TermCtor "Z"
             else TermCtorApp ("S", nat_of_int_term (n - 1))

  let rec char_list_of_s s =
    let folder c acc = PatApp ("Cons", PatTuple (List2.of_list [PatChar c; acc])) in
    List.fold_right folder s (PatCtor "Nil")

  let rec char_list_of_es s =
    let folder c acc = EPatCtorApp ("Cons", EPatTuple (List2.of_list [EPatChar c; acc])) in
    List.fold_right folder s (EPatCtor "Nil")

  let rec char_list_of_ts s =
    let folder c acc = TermCtorApp ("Cons", TermTuple (List2.of_list [TermChar c; acc])) in
    List.fold_right folder s (TermCtor "Nil")

  let rec list_of_ps ps =
    let folder p acc = PatApp ("Cons", PatTuple (List2.of_list [p; acc])) in
    List1.fold_right folder ps (PatCtor "Nil")

  let rec list_of_eps eps =
    let folder ep acc = EPatCtorApp ("Cons", EPatTuple (List2.of_list [ep; acc])) in
    List1.fold_right folder eps (EPatCtor "Nil")

  let rec list_of_terms terms =
    let folder t acc = TermCtorApp ("Cons", TermTuple (List2.of_list [t; acc])) in
    List1.fold_right folder terms (TermCtor "Nil")

  let rec lambdas_of_params = function
    | [] -> fun omega -> omega
    | psi :: tl -> fun omega -> IsoFun (psi, lambdas_of_params tl omega)

  type ambiguous = AmbVar of string | AmbApp of iso * ambiguous

  let rec iso_of_amb = function
    | AmbVar phi -> IsoVar phi
    | AmbApp (omega, a) -> IsoApp (omega, iso_of_amb a)

  let rec term_of_amb = function
    | AmbVar x -> TermVar x
    | AmbApp (omega, a) -> TermIsoApp (omega, term_of_amb a)

  let rec epat_of_amb = function
    | AmbVar x -> EPatVar x
    | AmbApp (omega, a) -> EPatIsoApp (omega, epat_of_amb a)

  let make_dot l =
    let p = PatVar "x" in
    let ep = List2.fold_right (fun omega acc -> EPatIsoApp (omega, acc)) l (EPatVar "x") in
    let e = ExprEPat ep in
    IsoCase (List1.singleton (p, e))

  let make_times l =
    let p = PatTuple (List2.mapi (fun i _ -> PatVar (Util.alphabet i)) l) in
    let ep = EPatTuple (List2.mapi (fun i omega -> EPatIsoApp (omega, EPatVar (Util.alphabet i))) l) in
    let e = ExprEPat ep in
    IsoCase (List1.singleton (p, e))

  let make_plus omega_1 omega_2 =
    let pl = PatApp ("Left", PatVar "x") in
    let pr = PatApp ("Right", PatVar "x") in
    let epl = EPatCtorApp ("Left", EPatIsoApp (omega_1, EPatVar "x")) in
    let epr = EPatCtorApp ("Right", EPatIsoApp (omega_2, EPatVar "x")) in
    let el = ExprEPat epl in
    let er = ExprEPat epr in
    IsoCase (List1.of_list [(pl, el); (pr, er)])
%}

%token EOF LPAREN RPAREN LBRACKET RBRACKET DOT TIMES PLUS PIPE COMMA SEMICOLON CONS TRIANGLE
       ARROW BIARROW EQUAL UNIT CHARTYPE LET ISO IN FIX TYPE REC OF FUN CASE MATCH WITH
%token <int> NAT
%token <string> TICKED LOWER UPPER
%token <char> CHAR
%token <char list> STRING

%start <program> program
%type <typedef> typedef
%type <base> base_grouped base
%type <variant> variant
%type <pat> pat_grouped_novar pat_grouped pat_app pat
%type <ambiguous> ambiguous_grouped ambiguous_app ambiguous
%type <epat> epat_grouped epat_app epat_cons epat_triangle epat_top epat
%type <expr> expr expr_nocase
%type <pat * expr> branch
%type <iso> iso_grouped iso_app iso_app_or_ambiguous_app iso_dot
            iso_dot_or_ambiguous_app iso_times iso_plus iso_triangle iso_top iso
%type <term> term_grouped term_app term_cons term_triangle term_top term
%%

list1_impl(separator, X):
  | x = X; { [x] }
  | x = X; separator; xs = list1_impl(separator, X); { x :: xs }

list1(separator, X):
  | l = list1_impl(separator, X); { List1.of_list l }

list2_impl(separator, X):
  | x = X; separator; y = X; { [x; y] }
  | x = X; separator; xs = list2_impl(separator, X); { x :: xs }

list2(separator, X):
  | l = list2_impl(separator, X); { List2.of_list l }

program:
  | ts = typedef*; SEMICOLON; SEMICOLON; t = term; EOF;
    {
      let omega = IsoFun ("f", IsoInv (IsoVar "f")) in
      (ts, TermIso { phi = "inv"; omega; t })
    }

typedef:
  | TYPE; name = LOWER; EQUAL; PIPE?; variants = list1(PIPE, variant);
    { { params = []; name; variants = List1.to_list variants } }

  | TYPE; param = TICKED; name = LOWER; EQUAL; PIPE?; variants = list1(PIPE, variant);
    { { params = [param]; name; variants = List1.to_list variants } }

  | TYPE; LPAREN; params = list2(COMMA, TICKED); RPAREN; name = LOWER;
    EQUAL; PIPE?; variants = list1(PIPE, variant);
    { { params = List2.to_list params; name; variants = List1.to_list variants } }

base_grouped:
  | LPAREN; a = base; RPAREN; { a }
  | UNIT; { BaseUnit }
  | CHARTYPE; { BaseChar }
  | x = LOWER; { BaseIdent x }
  | v = TICKED; { BaseVar v }
  | a = base_grouped; x = LOWER; { BaseApp (List1.of_list [a], x) }
  | LPAREN; aa = list2(COMMA, base); RPAREN; x = LOWER; { BaseApp (List2.to_list1 aa, x) }

base:
  | a = base_grouped; { a }
  | l = list2(TIMES, base_grouped); { BaseProd l }

variant:
  | c = UPPER; OF; a = base; { (c, Some a) }
  | c = UPPER; { (c, None) }

pat_grouped_novar:
  | LPAREN; p = pat; RPAREN; { p }
  | LPAREN; RPAREN; { PatUnit }
  | s = STRING; { char_list_of_s s }
  | c = CHAR; { PatChar c }
  | LPAREN; l = list2(COMMA, pat); RPAREN; { PatTuple l }
  | c = UPPER; { PatCtor c }
  | n = NAT; { nat_of_int_pat n }
  | LBRACKET; RBRACKET; { PatCtor "Nil" }
  | LBRACKET; ps = list1(SEMICOLON, pat); RBRACKET; { list_of_ps ps }

pat_grouped:
  | LPAREN; p = pat; RPAREN; { p }
  | LPAREN; RPAREN; { PatUnit }
  | s = STRING; { char_list_of_s s }
  | c = CHAR; { PatChar c }
  | LPAREN; l = list2(COMMA, pat); RPAREN; { PatTuple l }
  | x = LOWER; { PatVar x }
  | c = UPPER; { PatCtor c }
  | n = NAT; { nat_of_int_pat n }
  | LBRACKET; RBRACKET; { PatCtor "Nil" }
  | LBRACKET; ps = list1(SEMICOLON, pat); RBRACKET; { list_of_ps ps }

pat_app:
  | p = pat_grouped; { p }
  | c = UPPER; p = pat_grouped; { PatApp (c, p) }

pat:
  | p = pat_app; { p }
  | p_1 = pat_app; CONS; p_2 = pat; { PatApp ("Cons", PatTuple (List2.of_list [p_1; p_2])) }

ambiguous_grouped:
  | x = LOWER; { AmbVar x }
  | LPAREN; a = ambiguous; RPAREN; { a }

ambiguous_app:
  | a = ambiguous_grouped; { a }
  | a = ambiguous_app; b = ambiguous_grouped; { AmbApp (iso_of_amb a, b) }
  | omega = iso_app; a = ambiguous_grouped; { AmbApp (omega, a) }

ambiguous:
  | a = ambiguous_app; { a }
  | b = ambiguous; TRIANGLE; a = ambiguous_app; { AmbApp (iso_of_amb a, b) }
  | a = ambiguous; TRIANGLE; omega = iso_plus; { AmbApp (omega, a) }

epat_grouped:
  | LPAREN; ep = epat_top; RPAREN; { ep }
  | LPAREN; RPAREN; { EPatUnit }
  | c = CHAR; { EPatChar c }
  | s = STRING; { char_list_of_es s }
  | LPAREN; l = list2(COMMA, epat); RPAREN; { EPatTuple l }
  | c = UPPER; { EPatCtor c }
  | n = NAT; { nat_of_int_epat n }
  | LBRACKET; RBRACKET; { EPatCtor "Nil" }
  | LBRACKET; eps = list1(SEMICOLON, epat); RBRACKET; { list_of_eps eps }

epat_app:
  | ep = epat_grouped; { ep }
  | c = UPPER; ep = epat_grouped; { EPatCtorApp (c, ep) }
  | c = UPPER; ep = ambiguous_grouped; { EPatCtorApp (c, epat_of_amb ep) }
  | omega = iso_app; ep = epat_grouped; { EPatIsoApp (omega, ep) }
  | omega = ambiguous_app; ep = epat_grouped; { EPatIsoApp (iso_of_amb omega, ep) }

epat_cons:
  | ep = epat_app; { ep }
  | ep_1 = epat_app; CONS; ep_2 = epat_cons;
    { EPatCtorApp ("Cons", EPatTuple (List2.of_list [ep_1; ep_2])) }
  | ep_1 = epat_app; CONS; ep_2 = ambiguous_app;
    { EPatCtorApp ("Cons", EPatTuple (List2.of_list [ep_1; epat_of_amb ep_2])) }
  | ep_1 = ambiguous_app; CONS; ep_2 = epat_cons;
    { EPatCtorApp ("Cons", EPatTuple (List2.of_list [epat_of_amb ep_1; ep_2])) }
  | ep_1 = ambiguous_app; CONS; ep_2 = ambiguous_app;
    { EPatCtorApp ("Cons", EPatTuple (List2.of_list [epat_of_amb ep_1; epat_of_amb ep_2])) }

epat_triangle:
  | ep = epat_cons; { ep }
  | ep = epat_triangle; TRIANGLE; omega = iso_plus; { EPatIsoApp (omega, ep) }
  | ep = epat_triangle; TRIANGLE; omega = ambiguous_app; { EPatIsoApp (iso_of_amb omega, ep) }

epat_top:
  | ep = epat_triangle; { ep }
  | MATCH; e = epat; WITH; PIPE?; l = list1(PIPE, branch);
    { EPatIsoApp (IsoCase l, e) }

epat:
  | a = ambiguous; { epat_of_amb a }
  | ep = epat_top; { ep }

expr:
  | ep = epat_triangle; { ExprEPat ep }
  | ep = ambiguous; { ExprEPat (epat_of_amb ep) }
  | LET; p = pat; EQUAL; ep = epat; IN; e = expr;
  | LPAREN; LET; p = pat; EQUAL; ep = epat; IN; e = expr_nocase; RPAREN;
    { ExprLet { p; ep; e } }

expr_nocase:
  | ep = epat; { ExprEPat ep }
  | LET; p = pat; EQUAL; ep = epat; IN; e = expr_nocase;
  | LPAREN; LET; p = pat; EQUAL; ep = epat; IN; e = expr_nocase; RPAREN;
    { ExprLet { p; ep; e } }

branch:
  | p = pat; BIARROW; e = expr; { (p, e) }

iso_grouped:
  | LPAREN; omega = iso_top; RPAREN; { omega }

iso_app:
  | omega = iso_grouped; { omega }
  | omega_1 = iso_app; omega_2 = iso_grouped; { IsoApp (omega_1, omega_2) }
  | omega_1 = ambiguous_app; omega_2 = iso_grouped; { IsoApp (iso_of_amb omega_1, omega_2) }

iso_app_or_ambiguous_app:
  | omega = iso_app; { omega }
  | a = ambiguous_app; { iso_of_amb a }

iso_dot:
  | omega = iso_app; { omega }
  | l = list2(DOT, iso_app_or_ambiguous_app); { make_dot l }

iso_dot_or_ambiguous_app:
  | omega = iso_dot; { omega }
  | a = ambiguous_app; { iso_of_amb a }

iso_times:
  | omega = iso_dot; { omega }
  | l = list2(TIMES, iso_dot_or_ambiguous_app); { make_times l }

iso_plus:
  | omega = iso_times; { omega }
  | omega_1 = iso_plus; PLUS; omega_2 = iso_times;
    { make_plus omega_1 omega_2 }

  | omega_1 = iso_plus; PLUS; omega_2 = ambiguous_app;
    { make_plus omega_1 (iso_of_amb omega_2) }

  | omega_1 = ambiguous_app; PLUS; omega_2 = iso_times;
    { make_plus (iso_of_amb omega_1) omega_2 }

  | omega_1 = ambiguous_app; PLUS; omega_2 = ambiguous_app;
    { make_plus (iso_of_amb omega_1) (iso_of_amb omega_2) }

iso_triangle:
  | omega = iso_plus; { omega }
  | omega_2 = iso_triangle; TRIANGLE; omega_1 = iso_plus; { IsoApp (omega_1, omega_2) }
  | omega_2 = iso_triangle; TRIANGLE; omega_1 = ambiguous_app; { IsoApp (iso_of_amb omega_1, omega_2) }

iso_top:
  | omega = iso_triangle; { omega }
  | CASE; PIPE?; l = list1(PIPE, branch); { IsoCase l }
  | FIX; phi = LOWER; ARROW; omega = iso; { IsoFix (phi, omega) }
  | FUN; params = LOWER+; ARROW; omega = iso; { lambdas_of_params params omega }

iso:
  | a = ambiguous; { iso_of_amb a }
  | omega = iso_top; { omega }

term_grouped:
  | LPAREN; t = term_top; RPAREN; { t }
  | LPAREN; RPAREN; { TermUnit }
  | c = CHAR; { TermChar c }
  | s = STRING; { char_list_of_ts s }
  | LPAREN; l = list2(COMMA, term); RPAREN; { TermTuple l }
  | c = UPPER; { TermCtor c }
  | n = NAT; { nat_of_int_term n }
  | LBRACKET; RBRACKET; { TermCtor "Nil" }
  | LBRACKET; l = list1(SEMICOLON, term); RBRACKET; { list_of_terms l }

term_app:
  | t = term_grouped; { t }
  | c = UPPER; t = term_grouped; { TermCtorApp (c, t) }
  | c = UPPER; t = ambiguous_grouped; { TermCtorApp (c, term_of_amb t) }
  | omega = iso_app; t = term_grouped; { TermIsoApp (omega, t) }
  | omega = ambiguous_app; t = term_grouped; { TermIsoApp (iso_of_amb omega, t) }

term_cons:
  | t = term_app; { t }
  | t_1 = term_app; CONS; t_2 = term_cons;
    { TermCtorApp ("Cons", TermTuple (List2.of_list [t_1; t_2])) }
  | t_1 = term_app; CONS; t_2 = ambiguous_app;
    { TermCtorApp ("Cons", TermTuple (List2.of_list [t_1; term_of_amb t_2])) }
  | t_1 = ambiguous_app; CONS; t_2 = term_cons;
    { TermCtorApp ("Cons", TermTuple (List2.of_list [term_of_amb t_1; t_2])) }
  | t_1 = ambiguous_app; CONS; t_2 = ambiguous_app;
    { TermCtorApp ("Cons", TermTuple (List2.of_list [term_of_amb t_1; term_of_amb t_2])) }

term_triangle:
  | t = term_cons; { t }
  | t = term_triangle; TRIANGLE; omega = iso_plus; { TermIsoApp (omega, t) }
  | t = term_triangle; TRIANGLE; omega = ambiguous_app; { TermIsoApp (iso_of_amb omega, t) }

term_top:
  | t = term_triangle; { t }
  | MATCH; t = term; WITH; PIPE?; l = list1(PIPE, branch); { TermIsoApp (IsoCase l, t) }
  | LET; p = pat; EQUAL; t_1 = term; IN; t_2 = term; { TermLet { p; t_1; t_2 } }
  | ISO; phi = LOWER; params = LOWER*; EQUAL; omega = iso; IN; t = term;
    { TermIso { phi; omega = lambdas_of_params params omega; t } }

  | ISO; phi = LOWER; params = LOWER*; p = pat_grouped_novar; EQUAL; e = expr_nocase; IN; t = term;
    {
      let omega = IsoCase List1.((p, e) :: []) in
      TermIso { phi; omega = lambdas_of_params params omega; t }
    }

  | ISO; REC; phi = LOWER; params = LOWER*; EQUAL; omega = iso; IN; t = term;
    {
      let omega = lambdas_of_params params omega in
      TermIso { phi; omega = IsoFix (phi, omega); t }
    }

  | ISO; REC; phi = LOWER; params = LOWER*; p = pat_grouped_novar; EQUAL; e = expr_nocase; IN; t = term;
    {
      let omega = IsoCase List1.((p, e) :: []) in
      let omega = lambdas_of_params params omega in
      TermIso { phi; omega = IsoFix (phi, omega); t }
    }

term:
  | a = ambiguous; { term_of_amb a }
  | t = term_top; { t }

%{
  open Types
%}

%token EOF LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET TIMES PIPE COMMA SEMICOLON CONS
       ARROW BIARROW EQUAL UNIT LET IN ISO FIX TYPE INVERT REC OF FUN CASE MATCH WITH
%token <int> NAT
%token <string> TVAR VAR CTOR

%start <program> program
%type <typedef> typedef
%type <base_type> base_type_grouped base_type
%type <variant> variant
%type <value> value_grouped value_almost value
%type <expr_intermediate> expr_intermediate
%type <expr> expr
%type <value * expr> biarrowed
%type <iso> iso_grouped iso_almost iso
%type <term> term_grouped term_almost term term_nonlet
%%

wtf(separator, X):
  | x = X; separator; y = X; { [x; y] }
  | x = X; separator; xs = wtf(separator, X); { x :: xs }

program:
  | ts = typedef*; t = term; EOF; { { ts; t } }

typedef:
  | TYPE; t = VAR; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant);
    { { vars = []; t; vs } }

  | TYPE; var = TVAR; t = VAR; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant);
    { { vars = [var]; t; vs } }

  | TYPE; LPAREN; vars = wtf(COMMA, TVAR); RPAREN; t = VAR; EQUAL; PIPE?; vs = separated_nonempty_list(PIPE, variant);
    { { vars; t; vs } }

base_type_grouped:
  | LPAREN; t = base_type; RPAREN; { t }
  | UNIT; { Unit }
  | x = VAR; { Named x }
  | x = TVAR; { Var x }
  | t = base_type_grouped; a = VAR; { Ctor ([t], a) }
  | LPAREN; ts = wtf(COMMA, base_type); RPAREN; a = VAR; { Ctor (ts, a) }

base_type:
  | ts = wtf(TIMES, base_type_grouped); { Product ts }
  | t = base_type_grouped; { t }

variant:
  | c = CTOR; OF; a = base_type; { Iso { c; a } }
  | c = CTOR; { Value c }

value_grouped:
  | LPAREN; v = value; RPAREN; { v }
  | LPAREN; RPAREN; { Unit }
  | LPAREN; vs = wtf(COMMA, value); RPAREN; { Tuple vs }
  | x = VAR; { Var x }
  | x = CTOR; { Ctor x }
  | n = NAT; { nat_of_int n }
  | LBRACKET; RBRACKET; { Ctor "Nil" }
  | LBRACKET; vs = separated_nonempty_list(SEMICOLON, value); RBRACKET;
    {
      let f value acc : value = Cted { c = "Cons"; v = Tuple [value; acc] } in
      List.fold_right f vs (Ctor "Nil")
    }

value_almost:
  | v = value_grouped; { v }
  | c = CTOR; v = value_grouped; { let lmao : value = Cted { c ; v } in lmao }

value:
  | v = value_almost; { v }
  | v_1 = value_almost; CONS; v_2 = value; { Cted { c = "Cons"; v = Tuple [v_1; v_2] } }

expr_intermediate:
  | t = term_nonlet; { IValue t }
  | LET; p_1 = value; EQUAL; p_2 = term_nonlet; IN; e = expr_intermediate; { ILet { p_1; p_2; e } }
  | LET; p_1 = value; EQUAL; MATCH; p_2 = term_nonlet; WITH;
    PIPE?; p = separated_nonempty_list(PIPE, biarrowed); IN; e = expr_intermediate;
    { ILet { p_1; p_2 = App { omega = Pairs p; t = p_2 }; e } }

expr:
  | e = expr_intermediate;
    {
      let gen = new_generator () in
      expand_expr gen e |> Result.error_to_failure
    }

biarrowed:
  | v = value; BIARROW; e = expr; { (v, e) }

iso_grouped:
  | LBRACE; omega = iso; RBRACE; { omega }
  | x = VAR; { let lmao : iso = Var x in lmao }

iso_almost:
  | omega = iso_grouped; { omega }
  | INVERT; omega = iso_grouped; { Invert omega }
  | omega_1 = iso_almost; omega_2 = iso_grouped; { App { omega_1; omega_2 } }

iso:
  | omega = iso_almost; { omega }
  | CASE; PIPE?; p = separated_nonempty_list(PIPE, biarrowed); { Pairs p }
  | FIX; phi = VAR; ARROW; omega = iso; { Fix { phi; omega } }
  | FUN; params = VAR+; ARROW; omega = iso; { lambdas_of_params params omega }

term_grouped:
  | LPAREN; t = term; RPAREN; { t }
  | LPAREN; RPAREN; { Unit }
  | LPAREN; ts = wtf(COMMA, term); RPAREN; { Tuple ts }
  | x = VAR; { Var x }
  | x = CTOR; { Ctor x }
  | n = NAT; { nat_of_int n |> term_of_value }
  | LBRACKET; RBRACKET; { Ctor "Nil" }
  | LBRACKET; ts = separated_nonempty_list(SEMICOLON, term); RBRACKET;
    {
      let f t acc = Cted { c = "Cons"; t = Tuple [t; acc] } in
      List.fold_right f ts (Ctor "Nil")
    }

term_almost:
  | t = term_grouped; { t }
  | c = CTOR; t = term_grouped; { Cted { c; t } }
  | omega = iso_almost; t = term_grouped; { App { omega; t } }

term:
  | t = term_almost; { t }
  | t_1 = term_almost; CONS; t_2 = term; { Cted { c = "Cons"; t = Tuple [t_1; t_2] } }
  | LET; p = value; EQUAL; t_1 = term; IN; t_2 = term; { Let { p; t_1; t_2 } }
  | ISO; phi = VAR; params = VAR*; EQUAL; omega = iso; IN; t = term;
    { LetIso { phi; omega = lambdas_of_params params omega; t } }

  | MATCH; t = term; WITH; PIPE?; p = separated_nonempty_list(PIPE, biarrowed);
    { App { omega = Pairs p; t } }

  | ISO; REC; phi = VAR; params = VAR*; EQUAL; omega = iso; IN; t = term;
    {
      let omega = lambdas_of_params params omega in
      LetIso { phi; omega = Fix { phi; omega }; t }
    }

term_nonlet:
  | t = term_almost; { t }
  | t_1 = term_almost; CONS; t_2 = term_nonlet; { Cted { c = "Cons"; t = Tuple [t_1; t_2] } }
  | MATCH; t = term_nonlet; WITH; PIPE?; p = separated_nonempty_list(PIPE, biarrowed);
    { App { omega = Pairs p; t } }


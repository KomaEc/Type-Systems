%{

%}

%token LAMBDA LET FORALL IN ARROW
%token LPAREN RPAREN
%token <string * Location.t> IDENT
%token EQ
%token EOF

%start <Syntax.term> term_eof
%%

term_eof:
  | t = term; EOF 
    { t }
  ;

term:
  | LPAREN; t = term; RPAREN
    { t }
  | id_and_loc = IDENT
    { let (id, _) = id_and_loc in Var (id) }
  | LAMBDA; id_and_loc = IDENT; ARROW; t = term 
    { let (id, _) = id_and_loc in Lam (id, t) }
  | t1 = term; t2 = term 
    { Call (t1, t2) }
  | LET; id_and_loc = IDENT; EQ; t1 = term; IN; t2 = term 
    { let (id, _) = id_and_loc in Let (id, t1, t2) }
  ;

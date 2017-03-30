%{

open QueryAST

let make_index_slice start stop step =
  let step =
    match step with
    | None -> 1
    | Some step -> step
  in
  IndexSlice { start; stop; step }

%}

%start<QueryAST.query> query
%token AT COLON LBRACKET RBRACKET PIPE LPAREN EOF STAR RPAREN LBRACE RBRACE
%token QMARK LT LTE EQ GTE GT NOTEQ BANG DOT OR AND COMMA AMPERSAND

%token<string> QUOTED_STRING UNQUOTED_STRING RAW_STRING
%token<int> NUMBER
%token<Yojson.Basic.json> LITERAL

%nonassoc PIPE
%left DOT
%right BANG
%left EQ NOTEQ
%left LT LTE GTE GT
%nonassoc OR AND
%nonassoc LBRACKET



%%

query:
    e=expression EOF
    { e }

expression:
    STAR
    { Any }
  | AT
    { CurrentNode }
  | id=identifier
    { ExprIdentifier id }
  | s=RAW_STRING
    { RawString s }
  | l=LITERAL
    { RawJson l }
  | LPAREN e=expression RPAREN
    { e }
  | BANG e=expression
    { Not e }
  | e1=expression op=binary_operator e2=expression
    { BinaryExpression(op, e1, e2) }
  | e1=expression op=comparator_operator e2=expression
    { ComparatorExpression(op, e1, e2) }
  | e1=expression PIPE e2=expression
    { Pipe (e1, e2) }
  | fc=function_call
     { FunctionCall (fst fc, snd fc) }
  | e=expression DOT se=sub_expression
    { SubExpression(e, se) }
  | e=index_expression
    { e }
  (* Force two elements here otherwise it's possible to do a conflict with the
     rule bracket_specifier with the entry [*] *)
  | msl=two_elements_multi_select_list
    { ExprMultiSelectList msl }
  | msh=multi_select_hash
    { ExprMultiSelectHash msh }

sub_expression:
    STAR
    { SubExprAny }
  | id=identifier
    { SubExprIdentifier id }
  | msl=multi_select_list
    { SubExprMultiSelectList msl }
  | msh=multi_select_hash
    { SubExprMultiSelectHash msh }
  | fc=function_call
    { SubExprFunctionCall (fst fc, snd fc) }

function_call:
    name=UNQUOTED_STRING
      LPAREN ps=separated_list(COMMA, function_parameter) RPAREN
    { (name, ps) }

function_parameter:
    e=expression { FunParamExpr e }
  | AMPERSAND e=expression { FunParamExprTyp e }

multi_select_list:
    LBRACKET es=separated_nonempty_list(COMMA, expression) RBRACKET
    { es }

two_elements_multi_select_list:
    LBRACKET e1=expression COMMA
      es=separated_nonempty_list(COMMA, expression) RBRACKET
    { e1 :: es }

multi_select_hash:
    LBRACE es=separated_nonempty_list(COMMA, keypair) RBRACE
    { es }

%inline keypair:
    id=identifier COLON e=expression
    { (id, e) }

index_expression:
    bs=bracket_specifier
    { IndexExpression (None, bs) }
  | e=expression bs=bracket_specifier
    { IndexExpression (Some e, bs) }

bracket_specifier:
    LBRACKET RBRACKET
    { IndexEmpty }
  | LBRACKET STAR RBRACKET
    { IndexAny }
  | LBRACKET nb=NUMBER RBRACKET
    { Index nb }
  | LBRACKET e=slice_expression RBRACKET
    { e }
  | LBRACKET QMARK e=expression RBRACKET
    { IndexConditional e }

slice_expression:
    start=NUMBER? COLON stop=NUMBER? step=preceded(COLON,NUMBER)?
    { make_index_slice start stop step }

%inline binary_operator:
    OR
    { Or }
  | AND
    { And }

%inline comparator_operator:
    LT
    { LessThan }
  | LTE
    { LessOrEqual }
  | EQ
    { Equal }
  | GTE
    { GreaterOrEqual }
  | GT
    { GreaterThan }
  | NOTEQ
    { NotEqual }

identifier:
    id=QUOTED_STRING
    { Identifier id }
  | id=UNQUOTED_STRING
    { Identifier id }

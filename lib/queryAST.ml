
type query = expression

and expression =
  | Any
  | CurrentNode
  | ExprIdentifier of identifier
  | ExprMultiSelectList of multi_select_list
  | ExprMultiSelectHash of multi_select_hash
  | Not of expression
  | SubExpression of expression * sub_expression
  | BinaryExpression of binary_operator * expression * expression
  | ComparatorExpression of comparator_operator * expression * expression
  | IndexExpression of expression option * index_expression
  | Pipe of expression * expression
  | FunctionCall of string * fun_parameter list
  | RawString of string
  | RawJson of Yojson.Basic.json

and sub_expression =
  | SubExprAny
  | SubExprIdentifier of identifier
  | SubExprFunctionCall of string * fun_parameter list
  | SubExprMultiSelectList of multi_select_list
  | SubExprMultiSelectHash of multi_select_hash

and fun_parameter =
  | FunParamExpr of expression
  | FunParamExprTyp of expression

and multi_select_list = expression list

and multi_select_hash = (identifier * expression) list

and index_expression =
  | IndexAny
  | IndexEmpty
  | Index of number
  | IndexConditional of expression
  | IndexSlice of { start: number option;
                    stop: number option;
                    step: number; }

and binary_operator =
  | Or
  | And

and comparator_operator =
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
  | Equal
  | NotEqual

and identifier =
  | Identifier of string

and number = int


let rec pp_query fmt query =
  match query with
  | Any -> Format.pp_print_string fmt "*"
  | CurrentNode -> Format.pp_print_string fmt "@@"
  | ExprIdentifier id -> Format.fprintf fmt "%a" pp_identifier id
  | ExprMultiSelectList es -> Format.fprintf fmt "[%a]" pp_expression_list es
  | ExprMultiSelectHash es -> Format.fprintf fmt "[%a]" pp_select_hash_list es
  | Not e -> Format.fprintf fmt "!%a" pp_query e
  | SubExpression (e, se) ->
    Format.fprintf fmt "(%a).%a"
      pp_query e
      pp_sub_expression se
  | BinaryExpression (op, e1, e2) ->
    Format.fprintf fmt "%a %a %a"
      pp_query e1
      pp_binary_operator op
      pp_query e2
  | ComparatorExpression (op, e1, e2) ->
    Format.fprintf fmt "%a %a %a"
      pp_query e1
      pp_comparator_operator op
      pp_query e2
  | IndexExpression (o, e) ->
    Format.fprintf fmt "(%s)[%a]"
      (match o with
       | None -> ""
       | Some e -> Format.asprintf "%a" pp_query e)
      pp_index_expression e
  | Pipe (e1, e2) ->
    Format.fprintf fmt "%a | %a"
      pp_query e1
      pp_query e2
  | FunctionCall (name, parameters) ->
    Format.fprintf fmt "%s(%a)" name pp_parameters parameters
  | RawString s ->
    Format.fprintf fmt "'%s'" s
  | RawJson json ->
    Format.fprintf fmt "`%s`" (Yojson.Basic.to_string json)
and pp_parameters fmt ps =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
    pp_parameter
    fmt
    ps
and pp_parameter fmt p =
  match p with
  | FunParamExpr e -> pp_query fmt e
  | FunParamExprTyp e -> Format.fprintf fmt "&%a" pp_query e
and pp_sub_expression fmt sub =
  match sub with
  | SubExprAny ->
    Format.pp_print_string fmt "*"
  | SubExprIdentifier id ->
    Format.fprintf fmt "%a" pp_identifier id
  | SubExprFunctionCall (name, parameters) ->
    Format.fprintf fmt "%s(%a)" name pp_parameters parameters
  | SubExprMultiSelectList es ->
    Format.fprintf fmt "[%a]" pp_expression_list es
  | SubExprMultiSelectHash es ->
    Format.fprintf fmt "{%a}" pp_select_hash_list es
and pp_expression_list fmt es =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
    pp_query
    fmt
    es
and pp_key_pair fmt (id, e) =
  Format.fprintf fmt "%a : %a"
    pp_identifier id
    pp_query e
and pp_select_hash_list fmt es =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
    pp_key_pair
    fmt
    es
and pp_identifier fmt (Identifier id) =
  Format.pp_print_string fmt id
and pp_binary_operator fmt op =
  Format.pp_print_string fmt
    (match op with
     | Or -> "||"
     | And -> "&&")
and pp_comparator_operator fmt op =
  Format.pp_print_string fmt
    (match op with
     | LessThan -> "<"
     | LessOrEqual -> "<="
     | GreaterThan -> ">"
     | GreaterOrEqual -> ">="
     | Equal -> "=="
     | NotEqual -> "!=")
and pp_index_expression fmt e =
  match e with
  | IndexAny -> Format.pp_print_string fmt "*"
  | IndexEmpty -> ()
  | Index nb -> Format.pp_print_int fmt nb
  | IndexConditional e -> Format.fprintf fmt "? %a" pp_query e
  | IndexSlice {start; stop; step} ->
    Format.fprintf fmt
      "%s:%s:%d"
      (match start with None -> "" | Some s -> string_of_int s)
      (match stop with None -> "" | Some s -> string_of_int s)
      step


let rec pp_query_debug fmt query =
  match query with
  | Any ->
    Format.pp_print_string fmt "Any"
  | CurrentNode ->
    Format.pp_print_string fmt "CurrentNode"
  | ExprIdentifier id ->
    Format.fprintf fmt "ExprIdentifier (%a)" pp_identifier_debug id
  | ExprMultiSelectList es ->
    Format.fprintf fmt "ExprMultiSelectList (%a)" pp_expression_list_debug es
  | ExprMultiSelectHash es ->
    Format.fprintf fmt "ExprMultiSelectList (%a)" pp_select_hash_list_debug es
  | Not e ->
    Format.fprintf fmt "Not (%a)" pp_query_debug e
  | SubExpression (e, se) ->
    Format.fprintf fmt "SubExpression (%a, %a)"
      pp_query_debug e
      pp_sub_expression_debug se
  | BinaryExpression (op, e1, e2) ->
    Format.fprintf fmt "BinaryExpression (%a, %a, %a)"
      pp_binary_operator_debug op
      pp_query_debug e1
      pp_query_debug e2
  | ComparatorExpression (op, e1, e2) ->
    Format.fprintf fmt "ComparatorExpression (%a, %a, %a)"
      pp_comparator_operator_debug op
      pp_query_debug e1
      pp_query_debug e2
  | IndexExpression (o, e) ->
    Format.fprintf fmt "IndexExpression (%s, %a)"
      (match o with
       | None -> "None"
       | Some e -> Format.asprintf "Some(%a)" pp_query_debug e)
      pp_index_expression_debug e
  | Pipe (e1, e2) ->
    Format.fprintf fmt "Pipe (%a, %a)"
      pp_query_debug e1
      pp_query_debug e2
  | FunctionCall (name, parameters) ->
    Format.fprintf fmt "FunctionCall (%s, %a)"
      name pp_parameters_debug parameters
  | RawString s ->
    Format.fprintf fmt "RawString (%s)" s
  | RawJson j ->
    Format.fprintf fmt "RawJson (%s)" (Yojson.Basic.to_string j)
and pp_parameters_debug fmt ps =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
    pp_parameter_debug
    fmt
    ps
and pp_parameter_debug fmt p =
  match p with
  | FunParamExpr e -> Format.fprintf fmt "FunParamExpr (%a)" pp_query e
  | FunParamExprTyp e -> Format.fprintf fmt "FunParamExprTyp (%a)" pp_query e
and pp_sub_expression_debug fmt se =
  match se with
  | SubExprAny ->
    Format.pp_print_string fmt "SubExprAny"
  | SubExprIdentifier id ->
    pp_identifier_debug fmt id
  | SubExprFunctionCall (name, parameters) ->
    Format.fprintf fmt "SubExprFunctionCall (%s, %a)"
      name pp_parameters_debug parameters
  | SubExprMultiSelectList es ->
    Format.fprintf fmt "SubExprMultiSelectList (%a)"
      pp_expression_list_debug es
  | SubExprMultiSelectHash es ->
    Format.fprintf fmt "SubExprMultiSelectHash (%a)"
      pp_select_hash_list_debug es
and pp_key_pair_debug fmt (id, e) =
  Format.fprintf fmt "KeyPair(%a, %a)"
    pp_identifier_debug id
    pp_query_debug e
and pp_select_hash_list_debug fmt es =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
    pp_key_pair_debug
    fmt
    es
and pp_expression_list_debug fmt es =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
    pp_query_debug
    fmt
    es
and pp_identifier_debug fmt (Identifier id) =
  Format.fprintf fmt "Identifier %s" id
and pp_binary_operator_debug fmt op =
  Format.pp_print_string fmt
    (match op with
     | Or -> "Or"
     | And -> "And")
and pp_comparator_operator_debug fmt op =
  Format.pp_print_string fmt
    (match op with
     | LessThan -> "LessThan"
     | LessOrEqual -> "LessOrEqual"
     | GreaterThan -> "GreaterThan"
     | GreaterOrEqual -> "GreaterOrEqual"
     | Equal -> "Equal"
     | NotEqual -> "NotEqual")
and pp_index_expression_debug fmt e =
  match e with
  | IndexAny -> Format.pp_print_string fmt "IndexAny"
  | IndexEmpty -> Format.pp_print_string fmt "IndexEmpty"
  | Index nb -> Format.fprintf fmt "Index(%d)" nb
  | IndexConditional e ->
    Format.fprintf fmt "ConditionalIndex(%a)"
      pp_query_debug e
  | IndexSlice {start; stop; step} ->
    Format.fprintf fmt "Slice {start=%s; stop=%s; step=%d}"
      (match start with None -> "None" | Some s -> "Some " ^ string_of_int s)
      (match stop with None -> "None" | Some s -> "Some " ^ string_of_int s)
      step


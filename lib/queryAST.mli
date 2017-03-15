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

val pp_query: Format.formatter -> query -> unit
val pp_query_debug: Format.formatter -> query -> unit

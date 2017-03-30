
exception TypeError
exception IncorrectParameters of int * int

type jmespath_parameter =
  [ Yojson.Basic.json |
    `JMESPathExpr of (Yojson.Basic.json -> Yojson.Basic.json) ]

type jmespath_function = jmespath_parameter list -> Yojson.Basic.json

val abs: jmespath_function
val avg: jmespath_function
val contains: jmespath_function
val ceil: jmespath_function
val ends_with: jmespath_function
val floor: jmespath_function
val join: jmespath_function
val keys: jmespath_function
val length: jmespath_function
val map: jmespath_function
val max: jmespath_function
val max_by: jmespath_function
val merge: jmespath_function
val min: jmespath_function
val min_by: jmespath_function
val not_null: jmespath_function
val reverse: jmespath_function
val sort: jmespath_function
val sort_by: jmespath_function
val starts_with: jmespath_function
val sum: jmespath_function
val to_array: jmespath_function
val to_string: jmespath_function
val to_number: jmespath_function
val type_: jmespath_function
val values: jmespath_function


val get_function: string -> jmespath_function

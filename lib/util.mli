
val not_null: Yojson.Basic.json -> bool

val truth_value: Yojson.Basic.json -> bool

type 'a polymorphic_operator = { op: 'a. 'a -> 'a -> bool }

val compare_number :
  'a polymorphic_operator -> Yojson.Basic.json -> Yojson.Basic.json ->
  Yojson.Basic.json

val json_equal: Yojson.Basic.json -> Yojson.Basic.json -> bool

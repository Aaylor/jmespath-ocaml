
val query_of_string: string -> QueryAST.query

val query_of_channel: in_channel -> QueryAST.query

val query_of_file: string -> QueryAST.query

val eval_query: QueryAST.query -> Yojson.Basic.json -> Yojson.Basic.json

val eval_string: string -> Yojson.Basic.json -> Yojson.Basic.json

val eval_channel: in_channel -> Yojson.Basic.json -> Yojson.Basic.json

val eval_file: string -> Yojson.Basic.json -> Yojson.Basic.json



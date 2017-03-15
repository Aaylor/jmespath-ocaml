
let get_query lexbuf =
  QueryParser.query QueryLexer.query_lexing lexbuf

let query_of_string str =
  get_query (Lexing.from_string str)

let query_of_channel channel =
  get_query (Lexing.from_channel channel)

let query_of_file filename =
  let input_channel = open_in filename in
  let query = query_of_channel input_channel in
  close_in input_channel;
  query

let eval_query query json =
  QueryEvaluator.eval query json

let eval_string s json =
  eval_query (query_of_string s) json

let eval_channel channel json =
  eval_query (query_of_channel channel) json

let eval_file f json =
  eval_query (query_of_file f) json

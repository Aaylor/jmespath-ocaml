{

  open QueryParser

  let eval_literal l =
    try LITERAL (Yojson.Basic.from_string l)
    with _ -> failwith "invalid json"

}

let digit = [ '\x30' - '\x39' ]
let number = '-'? digit+

(* TODO: replace \x5f by the real value... *)
let unescaped_char = ['\x20'-'\x21' '\x23'-'\x5B' '\x5D'-'\x5F']
let escape = '\x5C'
let quote = '\x22'

let escaped_char =
  escape
    ('\x22' | '\x5C' | '\x2F' | '\x62' | '\x66' | '\x6E' | '\x72' |
     '\x74')
(* TODO: with 4 HEX digit *)

let unquoted_string =
  ['\x41'-'\x5A' '\x61'-'\x7A' '\x5F']
  (digit | ['\x41'-'\x5A' '\x5F' '\x61'-'\x7A'])*

let quoted_string =
  quote ((escaped_char | unescaped_char)+ as str) quote

let identifier = unquoted_string | quoted_string

let layout = [' ' '\n' '\t' '\r']

(* TODO: replace \x7A by the real value *)
let raw_string_char =
    ['\x20'-'\x26' '\x28'-'\x5B' '\x5D'-'\x7A'] |
    escape ['\x20'-'\x26' '\x28'-'\x5B' '\x5D'-'\x7A'] |
    escape ('\'' | escape)

let raw_string = '\'' (raw_string_char* as str) '\''

let literal = '`' (_* as l) '`'


rule query_lexing = parse
  (* Punctuation *)
  | '*'  { STAR }
  | '@'  { AT   }
  | ':'  { COLON }
  | '['  { LBRACKET }
  | ']'  { RBRACKET }
  | '|'  { PIPE }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | '?'  { QMARK }
  | '!'  { BANG }
  | '.'  { DOT }
  | ','  { COMMA }
  | '&'  { AMPERSAND }

  (* Binary Operator *)
  | "||" { OR }
  | "&&" { AND }

  (* Binary Comparator *)
  | '<'  { LT }
  | "<=" { LTE }
  | "==" { EQ }
  | ">=" { GTE }
  | ">"  { GT }
  | "!=" { NOTEQ }

  (* Number literal.
     FIXME: maybe an another thing than int of string ?? *)
  | number as nb { NUMBER (int_of_string nb) }

  (* Identifiers *)
  | unquoted_string as str { UNQUOTED_STRING str }
  | quoted_string { QUOTED_STRING str }

  (* Raw values *)
  | raw_string { RAW_STRING str }
  | literal { eval_literal l }

  (* End of File *)
  | layout { query_lexing lexbuf }
  | eof  { EOF  }

{
}

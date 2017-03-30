
(* Specific Type for Querying Functions *)

type jmespath_parameter =
  [ Yojson.Basic.json |
    `JMESPathExpr of (Yojson.Basic.json -> Yojson.Basic.json) ]

type jmespath_function = jmespath_parameter list -> Yojson.Basic.json

let coerce_to_json (p: jmespath_parameter): Yojson.Basic.json =
  match p with
  | `Bool b -> `Bool b
  | `Null -> `Null
  | `Assoc a -> `Assoc a
  | `Float f -> `Float f
  | `List l -> `List l
  | `String s -> `String s
  | `Int i -> `Int i
  | _ -> assert false


(* Types System *)

type jmespath_type =
  | Number
  | String
  | Boolean
  | Array of jmespath_type
  | Object
  | Null
  | Expression
  | Or of jmespath_type * jmespath_type
  | Any
  | Variadic of jmespath_type

type number = [ `Int of int | `Float of float ]


(* Typechecking *)

exception TypeError
exception IncorrectParameters of int * int

let rec compatible_type typ (json: jmespath_parameter) =
  match typ, json with
  | Number, (`Int _ | `Float _)
  | String, `String _
  | Boolean, `Bool _
  | Object, `Assoc _
  | Any, `JMESPathExpr _ ->
    false
  | Any, _ ->
    true
  | Array typ, `List elts ->
    List.for_all (compatible_type typ) (elts :> jmespath_parameter list)
  | Expression, `JMESPathExpr _ ->
    true
  | Or (t1, t2), _ ->
    compatible_type t1 json || compatible_type t2 json
  | _ ->
    false

let typecheck types params =
  let rec typecheck_aux index types params =
    match types, params with
    | [], [] ->
      ()
    | [ Variadic typ ], ys ->
      (* Special case for variadic functions *)
      List.iter
        (fun param ->
           if not (compatible_type typ param) then raise TypeError)
        ys
    | x :: xs, y :: ys ->
      if not (compatible_type x y) then raise TypeError;
      typecheck_aux (succ index) xs ys
    | [], ys ->
      raise (IncorrectParameters (index, List.length ys + index))
    | xs, [] ->
      raise (IncorrectParameters (List.length xs + index, index))
  in
  typecheck_aux 0 types params


(* Coercion function *)

let coerce_number (json: Yojson.Basic.json) : number =
  match json with
  | `Int i -> `Int i
  | `Float f -> `Float f
  | _ -> assert false


(* Helpers *)

let max_int_string left right =
  match left, right with
  | `Int i, `Int j -> `Int (max i j)
  | `String i, `String j -> `String (max i j)
  | _ ->
    Format.eprintf "max_int_string (%s) (%s): failure@."
      (Yojson.Basic.to_string left) (Yojson.Basic.to_string right);
    assert false

let min_int_string left right =
  match left, right with
  | `Int i, `Int j -> `Int (min i j)
  | `String i, `String j -> `String (min i j)
  | _ ->
    Format.eprintf "max_int_string (%s) (%s): failure@."
      (Yojson.Basic.to_string left) (Yojson.Basic.to_string right);
    assert false

(* Function register *)

exception UnknownFunction of string

module FunctionRegister : sig
  val register_function: string -> jmespath_function -> unit
  val get_function: string -> jmespath_function
end = struct
  let function_table = Hashtbl.create 13

  let register_function name m =
    Hashtbl.add function_table name m

  let get_function name =
    try Hashtbl.find function_table name
    with Not_found -> raise (UnknownFunction name)
end


(* Generic functions *)

let make_function ~name ~types ~run =
  let function_value parameters =
    typecheck types (parameters: jmespath_parameter list);
    run parameters
  in
  FunctionRegister.register_function name function_value;
  function_value


(* Functions *)

let abs =
  let run parameters =
    match parameters with
    | [ `Int i ] -> `Int (abs i)
    | [ `Float f ] -> `Float (abs_float f)
    | _ -> assert false
  in
  make_function ~name:"abs" ~types:[ Number ] ~run

let avg =
  let run = function
    | [ `List json ] ->
      let sum, nb =
        List.fold_left
          (fun (s, n) elt ->
             match coerce_number elt with
             | `Int i -> float_of_int i +. s, succ n
             | `Float f -> f +. s, succ n)
          (0., 0)
          json
      in
      `Float (sum /. (float_of_int nb))
    | _ -> assert false
  in
  make_function ~name:"avg" ~types:[ Array Number ] ~run

let contains =
  let run = function
    | [ `String subject; `String search] ->
      let pattern = Str.regexp_string search in
      let result =
        try
          ignore (Str.search_forward pattern subject 0);
          true
        with Not_found -> false
      in
      `Bool result
    | [ `List subject; `String search ] ->
      let result =
        List.exists
          (fun str ->
             match str with
             | `String s -> s = search
             | _ -> assert false)
          subject
      in
      `Bool result
    | [ (`String _ | `List _); _ ] ->
      `Bool false
    | _ ->
      assert false (* by typing *)
  in
  make_function ~name:"contains" ~types:[ Or (String, Array String); Any ] ~run

let ceil =
  let run = function
    | [ `Int i ] -> `Int i
    | [ `Float f ] -> `Float (ceil f)
    | _ -> assert false
  in
  make_function ~name:"ceil" ~types:[ Number ] ~run

let ends_with =
  let run = function
    | [ `String subject; `String suffix ] ->
      let suffix_length = String.length suffix in
      let result =
        try
          let subject_last = Str.last_chars subject suffix_length in
          subject_last = suffix
        with Invalid_argument _ ->
          false
      in
      `Bool result
    | _ ->
      assert false
  in
  make_function ~name:"ends_with" ~types:[ String; String ] ~run

let floor =
  let run = function
    | [ `Int i ] -> `Int i
    | [ `Float f ] -> `Float (floor f)
    | _ -> assert false
  in
  make_function ~name:"floor" ~types:[ Number ] ~run

let join =
  let run = function
    | [ `String glue; `List values ] ->
      let string_values =
        List.map
          (function
            | `String s -> s
            | _ -> assert false)
          values
      in
      let result = String.concat glue string_values in
      `String result
    | _ ->
      assert false
  in
  make_function ~name:"join" ~types:[ String; Array String ] ~run

let keys =
  let run = function
    | [ `Assoc assoc ] ->
      let result = List.map (fun (key, _) -> `String key) assoc in
      `List result
    | _ ->
      assert false
  in
  make_function ~name:"keys" ~types:[ Object ] ~run

let length =
  let run = function
    | [ `String s ] -> `Int (String.length s)
    | [ `Assoc elts ] -> `Int (List.length elts)
    | [ `List elts ] -> `Int (List.length elts)
    | _ -> assert false
  in
  make_function
    ~name:"length"
    ~types:[ Or (Or (String, Array Any), Object) ]
    ~run

let map =
  let run = function
    | [ `JMESPathExpr f; `List elts ] ->
      `List (List.map f elts)
    | _ ->
      assert false
  in
  make_function ~name:"map" ~types:[ Expression; Array Any ] ~run

let max =
  let run = function
    | [ `List elts ] ->
      List.fold_left
        (fun acc elt ->
           if acc = `Null then elt
           else max_int_string acc elt)
        `Null
        elts
    | _ ->
      assert false
  in
  make_function ~name:"max" ~types:[ Or (Array String, Array Number) ] ~run

let max_by =
  let run = function
    | [ `List elts; `JMESPathExpr eval ] ->
      let result =
        List.fold_left
          (fun acc elt ->
             if acc = `Null then eval elt
             else max_int_string acc (eval elt))
          `Null
          elts
      in
      result
    | _ ->
      assert false
  in
  make_function ~name:"max_by" ~types:[ Array Any; Expression ] ~run

let merge =
  let module MergeMap = Map.Make(String) in
  let run = function
    | objects ->
      let result_map =
        List.fold_left
          (fun acc elt ->
             match elt with
             | `Assoc assoc ->
               List.fold_left (fun m (k, v) -> MergeMap.add k v m) acc assoc
             | _ ->
               assert false (* by typing *))
          MergeMap.empty
          objects
      in
      let result_list =
        MergeMap.fold
          (fun k v acc -> (k, v) :: acc)
          result_map
          []
      in
      `Assoc result_list
  in
  make_function ~name:"merge" ~types:[ Variadic Object ] ~run

let min =
  let run = function
    | [ `List elts ] ->
      let result =
        List.fold_left
          (fun acc elt ->
             if acc = `Null then elt
             else min_int_string acc elt)
          `Null
          elts
      in
      result
    | _ ->
      assert false
  in
  make_function ~name:"min" ~types:[ Or(Array Number, Array String) ] ~run

let min_by =
  let run = function
    | [ `List elts; `JMESPathExpr eval ] ->
      let result =
        List.fold_left
          (fun acc elt ->
             if acc = `Null then eval elt
             else min_int_string acc (eval elt))
          `Null
          elts
      in
      result
    | _ ->
      assert false
  in
  make_function ~name:"min_by" ~types:[ Array Any; Expression; ] ~run

let not_null =
  let run parameters =
    let result =
      try List.find (fun elt -> elt <> `Null) parameters
      with Not_found -> `Null
    in
    coerce_to_json result
  in
  make_function ~name:"not_null" ~types:[ Variadic Any ] ~run

let reverse =
  let run = function
    | [ `String s ] ->
      let length = String.length s in
      let result = Bytes.make length '\x00' in
      String.iteri (fun i c -> Bytes.set result (length - i - 1) c) s;
      `String (Bytes.to_string result)
    | [ `List l ] ->
      `List (List.rev l)
    | _ ->
      assert false
  in
  make_function ~name:"reverse" ~types:[ Or (Array Any, String) ] ~run

let sort =
  let run = function
    | [ `List l ] ->
      let result = List.sort compare l in
      `List result
    | _ ->
      assert false
  in
  make_function ~name:"sort" ~types:[ Or (Array Number, Array String) ] ~run

let sort_by =
  let run = function
    | [ `List l; `JMESPathExpr eval ] ->
      let result =
        List.sort
          (fun left right -> compare (eval left) (eval right))
          l
      in
      `List result
    | _ ->
      assert false
  in
  make_function ~name:"sort_by" ~types:[ Array Any; Expression ] ~run

let starts_with =
  let run = function
    | [ `String subject; `String suffix ] ->
      let suffix_length = String.length suffix in
      let result =
        try
          let subject_start = String.sub subject 0 suffix_length in
          subject_start = suffix
        with Invalid_argument _ ->
          false
      in
      `Bool result
    | _ ->
      assert false
  in
  make_function ~name:"starts_with" ~types:[ String; String ] ~run

let sum =
  let run = function
    | [ `List l ] ->
      let result =
        List.fold_left
          (fun acc elt ->
             match elt with
             | `Int i -> acc +. (float_of_int i)
             | `Float f -> acc +. f
             | _ -> assert false)
          0. l
      in
      `Float result
    | _ -> assert false
  in
  make_function ~name:"sum" ~types:[ Array Number ] ~run

let to_array =
  let run = function
    | [ `List l ] -> `List l
    | [ json ] -> `List [ coerce_to_json json ]
    | _ -> assert false
  in
  make_function ~name:"to_array" ~types:[ Any ] ~run

let to_string =
  let run = function
    | [ `String s ] ->
      `String s
    | [ json ] ->
      let json = coerce_to_json json in
      let string_value = Yojson.Basic.to_string json in
      `String string_value
    | _ ->
      assert false
  in
  make_function ~name:"to_string" ~types:[ Any ] ~run

let to_number =
  let run = function
    | [ `Int i ] ->
      `Int i
    | [ `Float f ] ->
      `Float f
    | [ `String s ] ->
      let result =
        try float_of_string s
        with Failure _ -> assert false
      in
      `Float result
    | _ ->
      `Null
  in
  make_function ~name:"to_number" ~types:[ Any ] ~run

let type_ =
  let run = function
    | [ `List _ ] -> `String "array"
    | [ `Assoc _ ] -> `String "assoc"
    | [ `Int _ | `Float _] -> `String "number"
    | [ `String _ ] -> `String "string"
    | [ `Bool _ ] -> `String "boolean"
    | [ `Null ] -> `String "null"
    | _ -> assert false
  in
  make_function ~name:"type" ~types:[ Any ] ~run

let values =
  let run = function
    | [ `Assoc assoc ] ->
      let result = List.map snd assoc in
      `List result
    | _ ->
      assert false
  in
  make_function ~name:"values" ~types:[ Object ] ~run


(* Access *)

let get_function = FunctionRegister.get_function

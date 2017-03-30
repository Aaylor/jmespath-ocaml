
let not_null json =
  match json with
  | `Null -> false
  | _ -> true

let truth_value json =
  match json with
  | `List [] | `Assoc [] | `String "" | `Bool false | `Null -> false
  | _ -> true

type 'a polymorphic_operator = { op: 'a. 'a -> 'a -> bool }

let compare_number { op } left right =
  match left, right with
  | `Float f, `Int i -> `Bool (op f (float_of_int i))
  | `Int i, `Float f -> `Bool (op (float_of_int i) f)
  | `Float f1, `Float f2 -> `Bool (op f1 f2)
  | `Int i1, `Int i2 -> `Bool(op i1 i2)
  | _ -> `Null

let rec json_equal left right =
  match left, right with
  | `String s1, `String s2 ->
    String.equal s1 s2
  | `Int i1, `Int i2 ->
    i1 = i2
  | `Float f1, `Float f2 ->
    f1 = f2
  | `Int i, `Float f
  | `Float f, `Int i ->
    (float_of_int i) = f
  | `Bool b1, `Bool b2 ->
    b1 = b2
  | `Null, `Null ->
    true
  | `List l1, `List l2 ->
    begin
      try List.for_all2 json_equal l1 l2
      with Invalid_argument _ -> false
    end
  | `Assoc a1, `Assoc a2 ->
    let a1_sorted = List.sort (fun (s, _) (s', _) -> String.compare s s') a1 in
    let a2_sorted = List.sort (fun (s, _) (s', _) -> String.compare s s') a2 in
    begin
      try
        List.for_all2
          (fun (key1, value1) (key2, value2) ->
             String.equal key1 key2 && json_equal value1 value2)
          a1_sorted
          a2_sorted
      with Invalid_argument _ -> false
    end
  | _ ->
    false

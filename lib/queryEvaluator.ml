
open QueryAST

(* Utility Function *)

let map_filter ~map ~filter list =
  List.fold_right
    (fun elt acc ->
       let new_elt = map elt in
       if filter new_elt then new_elt :: acc
       else acc)
    list
    []

let slice ~start ~stop ~step json_list =
  if step = 0 then failwith "slice: step is 0";
  let array_length = List.length json_list in
  let make_default_value ~down ~up value =
    match value with
    | None -> if step > 0 then down else up
    | Some s -> if s < 0 then array_length + s else s
  in
  let json_list, start, stop, step =
    let start' = make_default_value ~down:0 ~up:array_length start in
    let stop' = make_default_value ~down:array_length ~up:0 stop in
    if step > 0 then
      (* if step is positive, work with the list as is *)
      json_list, start', stop', step
    else
      (* otherwise, we have to reverse the list, swap the start and stop
         indexes, and make the step positive *)
      List.rev json_list, stop', start', (abs step)
  in
  let rec aux i json_list acc =
    match json_list with
    | [] ->
      `List (List.rev acc)
    | x :: xs ->
      if i >= start && i < stop && i mod step = 0
      then aux (succ i) xs
          (x :: acc)
      else aux (succ i) xs acc
  in
  aux 0 json_list []


(* Evaluator *)

type evaluator_state =
  { projectify: bool;
    json: Yojson.Basic.json }

let projectify_state json =
  { projectify = true;
    json }

let unprojectify_state json =
  { projectify = false;
    json }

let rec eval_query query state =
  match query with
  | Any ->
    eval_any state
  | CurrentNode ->
    state
  | ExprIdentifier (Identifier id) ->
    eval_identifier id state
  | ExprMultiSelectList selection ->
    eval_multi_select_list selection state
  | ExprMultiSelectHash selection ->
    eval_multi_select_hash selection state
  | Not expr ->
    let { json } = eval_query expr state in
    let b = Util.truth_value json in
    unprojectify_state (`Bool (not b))
  | SubExpression (expr, sub_expr) ->
    let state' = eval_query expr state in
    eval_sub_expression sub_expr state'
  | BinaryExpression (binop, left, right) ->
    eval_binary_expression binop left right state
  | ComparatorExpression (comparator, left, right) ->
    eval_comparator_expression comparator left right state
  | IndexExpression (eo, index) ->
    let state' =
      match eo with
      | None -> state
      | Some expr -> eval_query expr state
    in
    eval_index_expression index state'
  | Pipe (left, right) ->
    let state' = eval_query left state in
    let right_state = { state' with projectify = false } in
    eval_query right right_state
  | FunctionCall (name, parameters) ->
    eval_function_call name parameters state
  | RawString str ->
    unprojectify_state (`String str)
  | RawJson json ->
    unprojectify_state json

and eval_any state =
  let result =
    match state.json with
    | `Assoc elements ->
      let result = map_filter ~map:snd ~filter:Util.not_null elements in
      `List result
    | _ ->
      `Null
  in
  { projectify = true;
    json = result }

and eval_identifier id state =
  let result =
    match state.json with
    | `List l when state.projectify ->
      let map elt =
        match elt with
        | `Assoc _ -> Yojson.Basic.Util.member id elt
        | _ -> `Null
      in
      let result = map_filter ~map ~filter:Util.not_null l in
      `List result
    | `Assoc _ ->
      Yojson.Basic.Util.member id state.json
    | _ ->
      `Null
  in
  { state with json = result }

and eval_multi_select_list selection state =
  let result =
    List.map
      (fun expression ->
         let { json } = eval_query expression state in
         json)
      selection
  in
  projectify_state (`List result)

and eval_multi_select_hash selection state =
  let result =
    List.map
      (fun (Identifier key, expression) ->
         let { json } = eval_query expression state in
         key, json)
      selection
  in
  projectify_state (`Assoc result)

and eval_binary_expression binop left right state =
  match binop with
  | Or ->
    let result_left = eval_query left state in
    if Util.truth_value result_left.json
    then result_left
    else
      let result_right = eval_query right state in
      if Util.truth_value result_right.json
      then result_right
      else unprojectify_state `Null
  | And ->
    let result_left = eval_query left state in
    if Util.truth_value result_left.json
    then eval_query right state
    else result_left

and eval_comparator_expression comparator left right state =
  let { json = json_left } = eval_query left state in
  let { json = json_right } = eval_query right state in
  let result =
    match comparator with
    | LessThan ->
      Util.compare_number Util.{ op = ( < ) } json_left json_right
    | LessOrEqual ->
      Util.compare_number Util.{ op = ( <= ) } json_left json_right
    | GreaterThan ->
      Util.compare_number Util.{ op = ( > ) } json_left json_right
    | GreaterOrEqual ->
      Util.compare_number Util.{ op = ( >= ) } json_left json_right
    | Equal -> `Bool (Util.json_equal json_left json_right)
    | NotEqual -> `Bool (not (Util.json_equal json_left json_right))
  in
  unprojectify_state result

and eval_sub_expression sub_expr state =
  match sub_expr with
  | SubExprAny ->
    eval_any state
  | SubExprIdentifier (Identifier id) ->
    eval_identifier id state
  | SubExprFunctionCall (name, parameters) ->
    eval_function_call name parameters state
  | SubExprMultiSelectList selection ->
    eval_multi_select_list selection state
  | SubExprMultiSelectHash selection ->
    eval_multi_select_hash selection state

and eval_index_expression index state =
  let eval_index_aux index list_json =
    match index with
    | IndexAny ->
      projectify_state (`List list_json)
    | IndexEmpty ->
      let flattened_list =
        List.fold_right
          (fun elt acc ->
             match elt with
             | `List l -> l @ acc
             | json -> json :: acc)
          list_json
          []
      in
      projectify_state (`List flattened_list)
    | Index index ->
      let array_length = List.length list_json in
      let final_index = if index < 0 then array_length + index else index in
      if final_index > array_length || final_index < 0
      then unprojectify_state `Null
      else unprojectify_state (List.nth list_json final_index)
    | IndexConditional condition ->
      let result_list =
        List.fold_left
          (fun acc json_elt ->
             let state = unprojectify_state json_elt in
             let { json = conditional_json } = eval_query condition state in
             if Util.truth_value conditional_json
             then json_elt :: acc
             else acc)
          []
          list_json
      in
      projectify_state (`List (List.rev result_list))
    | IndexSlice { start; stop; step } ->
      projectify_state (slice ~start ~stop ~step list_json)
  in
  match state.json with
  | `List l -> eval_index_aux index l
  | _ -> unprojectify_state `Null

and eval_function_call name parameters state =
  let parameters =
    List.map
      (fun e ->
         match e with
         | FunParamExpr e ->
           let { json } = eval_query e state in
           (json :> QueryFunctions.jmespath_parameter)
         | FunParamExprTyp e ->
           let eval_expr_typ json =
             let state = unprojectify_state json in
             let { json } = eval_query e state in
             json
           in
           `JMESPathExpr eval_expr_typ)
      parameters
  in
  let fn = QueryFunctions.get_function name in
  let json = fn parameters in
  { state with json }

let eval query json =
  let { json } = eval_query query (unprojectify_state json) in
  json

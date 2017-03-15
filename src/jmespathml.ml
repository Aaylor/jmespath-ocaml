
let usage =
  Format.sprintf
    "%s [json_file] <query>"
    Sys.executable_name

let exit_error msg =
  Format.eprintf "%s@." msg;
  exit 1

let parse_parameters () =
  match Array.length Sys.argv with
  | 2 -> `Stdin, Sys.argv.(1)
  | 3 -> `File Sys.argv.(1), Sys.argv.(2)
  | _ -> exit_error usage

let do_eval channel query =
  let json = Yojson.Basic.from_channel channel in
  let result = JMESPath.eval_string query json in
  Format.printf "%s@." (Yojson.Basic.to_string result)

let () =
  let input, query = parse_parameters () in
  let channel, clean =
    match input with
    | `Stdin ->
      stdin, (fun () -> ())
    | `File name ->
      try
        let channel = open_in name in
        channel, (fun () -> close_in channel)
      with Sys_error _ ->
        exit_error (Format.sprintf "Error: `%s' not found." name)
  in
  try
    do_eval channel query;
    clean ()
  with e ->
    raise e


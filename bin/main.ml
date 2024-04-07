let read_file filename =
  try Ok (In_channel.with_open_text filename In_channel.input_all)
  with Sys_error msg -> Error msg

let run source =
  let open Melox.Scanner in
  let open Melox.Parser in
  match scan ~drop_eof:true source with
  | Ok tokens -> (
      match parse tokens with
      | Ok ast_node -> Melox.Ast.pp ast_node |> print_endline
      | Error UnexpectedToken ->
          print_endline "Error [parser]: Unexpected token."
      | Error UnexpectedEof ->
          print_endline "Error [parser]: Unexpected end of file.")
  | Error e -> scanner_error_message e |> print_endline

let run_repl () =
  let rec loop () =
    print_string "> ";
    try
      read_line () |> run;
      loop ()
    with End_of_file ->
      print_endline "| Exiting...";
      ()
  in
  loop ()

let run_file filename =
  if Sys.file_exists filename then (
    try
      match read_file filename with
      | Ok source -> run source
      | Error msg ->
          Printf.printf
            "Error: failed to read file %s. Encountered the following internal \
             exception: %s\n"
            filename msg;
          exit 72
    with Sys_error msg ->
      Printf.printf "Internal error: %s\n" msg;
      exit 70)
  else (
    Printf.printf "Error: file %s does not exist.\n" filename;
    exit 72)

let () =
  match Sys.argv with
  | [| _ |] -> run_repl ()
  | [| _; filename |] -> run_file filename
  | _ ->
      print_endline "Usage: melox [script]";
      exit 64

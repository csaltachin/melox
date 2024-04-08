let read_file filename =
  try Ok (In_channel.with_open_text filename In_channel.input_all)
  with Sys_error msg -> Error msg

type exit_status =
  | ExitReplOk
  | RunOk
  | ScanError
  | ParseError
  | RuntimeError
  | FileNotFound
  | FileReadFailed
  | BadUsage
  | InternalException

let code_of_status status =
  match status with
  | ExitReplOk -> 0
  | RunOk -> 0
  | ScanError -> 65
  | ParseError -> 65
  | RuntimeError -> 70
  | FileNotFound -> 72
  | FileReadFailed -> 72
  | BadUsage -> 64
  | InternalException -> 70

let run source : exit_status =
  let open Melox.Scanner in
  (* TODO: rewrite this with let* (Result.bind) *)
  match scan ~drop_eof:true source with
  | Ok tokens -> (
      let open Melox.Parser in
      match parse tokens with
      | Ok ast_node -> (
          let open Melox.Interpreter in
          match interpret ast_node with
          | Ok value ->
              let _ = Melox.Object.pp_obj value |> print_endline in
              RunOk
          | Error DivisionByZero ->
              let _ = "Error [runtime]: Division by zero." |> print_endline in
              RuntimeError
          | Error (TypeError { expected; actual }) ->
              let _ =
                Printf.sprintf
                  "Error [runtime]: Type error. Expected %s, found %s." expected
                  actual
                |> print_endline
              in
              RuntimeError)
      | Error UnexpectedToken ->
          let _ = print_endline "Error [parser]: Unexpected token." in
          ParseError
      | Error UnexpectedEof ->
          let _ = print_endline "Error [parser]: Unexpected end of file." in
          ParseError)
  | Error e ->
      let _ = scanner_error_message e |> print_endline in
      ScanError

let run_repl () =
  let rec loop () =
    print_string "> ";
    try
      let _ = () |> read_line |> run in
      loop ()
    with End_of_file ->
      print_endline "| Exiting...";
      ExitReplOk
  in
  loop ()

let run_file filename =
  if Sys.file_exists filename then (
    try
      match read_file filename with
      | Ok source -> source |> run |> code_of_status |> exit
      | Error msg ->
          Printf.printf
            "Error: failed to read file %s. Encountered the following internal \
             exception: %s\n"
            filename msg;
          FileReadFailed
    with Sys_error msg ->
      Printf.printf "Internal exception: %s\n" msg;
      InternalException)
  else (
    Printf.printf "Error: file %s does not exist.\n" filename;
    FileNotFound)

let () =
  let exit_status =
    match Sys.argv with
    | [| _ |] -> run_repl ()
    | [| _; filename |] -> run_file filename
    | _ ->
        print_endline "Usage: melox [script]";
        BadUsage
  in
  exit_status |> code_of_status |> exit

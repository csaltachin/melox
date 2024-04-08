(** Try to read file [filename], and wrap the file contents (or any internal
    exception) in a result. It is assumed that [filename] is a valid file, i.e.
    that [Sys.file_exists filename] is true. *)
let read_file filename =
  try Ok (In_channel.with_open_text filename In_channel.input_all)
  with Sys_error msg -> Error msg

(** Type of exit status for the main melox executable. *)
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

(** Given an exit status, return the exit code that the executable should exit
    on. *)
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

(** Run the source code [source]. Return an exit status, depending on whether
    execution was successful, a syntax error was encountered, or a runtime error
    was raised during execution. *)
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

(** Run the REPL shell, reading lines and executing them until EOF is read, and
    returning [ExitReplOk]. *)
let run_repl () : exit_status =
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

(** Try to read file [filename] as Lox source code and execute it. Returns an
    exit status depending on whether the file read was successful and/or the
    resulting status of the code execution. *)
let run_file filename : exit_status =
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

(* Main command-line entry point for the melox executable. *)
let () =
  let status =
    match Sys.argv with
    | [| _ |] -> run_repl ()
    | [| _; filename |] -> run_file filename
    | _ ->
        print_endline "Usage: melox [script]";
        BadUsage
  in
  status |> code_of_status |> exit

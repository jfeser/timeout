open! Core
module Json = Yojson.Safe
open Timeout.Limited_process

let print_stats result =
  Out_channel.newline stdout;
  ( match result.status with
  | `Success -> printf "Child exited with return code 0.\n"
  | `Exit_or_signal (`Exit_non_zero code) ->
      printf "Child exited with return code %d.\n" code
  | `Exit_or_signal (`Signal s) ->
      printf "Child received signal %s.\n" (Signal.to_string s)
  | `Killed_memory -> printf "Child killed for violating memory limit.\n"
  | `Killed_runtime -> printf "Child killed for violating runtime limit.\n" );
  if Int64.(result.peak_memory <> min_value) then
    printf "Peak memory: %s Mb\n"
      (Int64.( / ) result.peak_memory (Int64.of_int 1000000) |> Int64.to_string);
  printf "Runtime: %s" (Time_ns.Span.to_short_string result.runtime)

let main memory_limit time_limit machine_readable silence_child verbose command
    () =
  match command with
  | Some (prog :: args) ->
      let mem_limit = Option.map ~f:(fun mb -> mb * 1000000) memory_limit in
      let time_limit = Option.map ~f:Time_ns.Span.of_sec time_limit in
      let output =
        if silence_child then `None
        else if machine_readable then `Saved
        else `Standard
      in
      let result = run ~output ?mem_limit ?time_limit (`Program (prog, args)) in
      if machine_readable then
        let json = [%yojson_of: result] result in
        Yojson.Safe.pretty_to_channel Out_channel.stdout json
      else if verbose then print_stats result
  | None | Some [] -> failwith "Error: No command specified."

let () =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-m" ~aliases:[ "--memory" ] (optional int)
         ~doc:" process memory limit (Mb) (default: unlimited)"
    +> flag "-t" ~aliases:[ "--time" ] (optional float)
         ~doc:" process time limit (sec) (default: unlimited)"
    +> flag "--machine-readable" no_arg
         ~doc:" produce a summary in machine readable format"
    +> flag "-q" ~aliases:[ "--quiet" ] no_arg
         ~doc:" silence all output from the child process"
    +> flag "-v" ~aliases:[ "--verbose" ] no_arg ~doc:" enable verbose output"
    +> flag "--" escape
         ~doc:" use the remaining arguments as the command to run"
  in
  let command =
    Command.basic_spec ~summary:"Run a command with time and memory limits."
      spec main
  in
  Command.run command

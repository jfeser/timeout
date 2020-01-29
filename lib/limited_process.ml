open Core

type span = Time_ns.Span.t

let yojson_of_span s = `Float (Time_ns.Span.to_sec s)

type int64 = Int64.t

let yojson_of_int64 x = `Intlit (Int64.to_string x)

type opt_file = string option

let yojson_of_opt_file = function
  | None -> `String ""
  | Some fn -> `String (In_channel.with_file fn ~f:In_channel.input_all)

type result = {
  runtime : span;
  peak_memory : int64;
  status :
    [ `Success
    | `Killed_memory
    | `Killed_runtime
    | `Exit_or_signal of (Unix.Exit_or_signal.error[@yojson.opaque]) ];
  stdout : opt_file;
  stderr : opt_file;
  user_time : span;
  sys_time : span;
}
[@@deriving yojson_of]

let ticks_per_second = 1000

let sleep_time = Float.( / ) 1.0 (Float.of_int ticks_per_second)

let start_time = ref Time_ns.epoch

let end_time = ref Time_ns.epoch

module Stubs = Bindings.Stubs (Bindings_stubs)

let getrusage =
  Foreign.foreign "getrusage"
    Ctypes.(int @-> ptr Stubs.Rusage.t @-> returning int)

let getrusage kind =
  let open Ctypes in
  let arg = match kind with `Self -> 0 | `Children -> -1 in
  let rusage_ptr = allocate_n ~count:1 Stubs.Rusage.t in
  let ret = getrusage arg rusage_ptr in
  let to_span tv =
    let open Ctypes in
    Time_ns.Span.(
      of_int_sec (getf tv Stubs.Timeval.tv_sec)
      + of_int_us (getf tv Stubs.Timeval.tv_usec |> Signed.Long.to_int))
  in
  if ret = 0 then
    Some
      (object
         method maxrss =
           Ctypes.(
             getf !@rusage_ptr Stubs.Rusage.ru_maxrss |> Signed.Long.to_int64)

         method utime =
           Ctypes.(getf !@rusage_ptr Stubs.Rusage.ru_utime) |> to_span

         method stime =
           Ctypes.(getf !@rusage_ptr Stubs.Rusage.ru_stime) |> to_span
      end)
  else None

let check_limits _pid _memory time =
  Option.value_map time ~default:`Ok ~f:(fun limit ->
      if Time_ns.(Span.(diff (now ()) !start_time <= limit)) then `Ok
      else `Runtime)

let get_result status =
  let rusage = Option.value_exn (getrusage `Children) in
  {
    status;
    stdout = None;
    stderr = None;
    runtime = Time_ns.diff !end_time !start_time;
    user_time = rusage#utime;
    sys_time = rusage#stime;
    peak_memory = rusage#maxrss;
  }

let rec wait_for_child memory_limit time_limit pid =
  end_time := Time_ns.now ();
  match Unix.wait_nohang (`Pid pid) with
  (* If the child process has exited, check return value and return result. *)
  | Some (_, result) -> (
      match result with
      | Ok () -> get_result `Success
      | Error err -> get_result (`Exit_or_signal err) )
  (* If not, check whether child should be killed, otherwise sleep. *)
  | None -> (
      match check_limits pid memory_limit time_limit with
      | `Ok ->
          let remaining_sleep = ref sleep_time in
          while Float.(!remaining_sleep > 0.0) do
            remaining_sleep := Unix.nanosleep sleep_time
          done;
          wait_for_child memory_limit time_limit pid
      | `Memory ->
          Process.kill ~is_child:true ~wait_for:(Time.Span.of_sec 10.0) pid;
          get_result `Killed_memory
      | `Runtime ->
          Process.kill ~is_child:true ~wait_for:(Time.Span.of_sec 10.0) pid;
          get_result `Killed_runtime )

let run ?(output = `Standard) ?mem_limit ?time_limit runnable =
  (* Generate temporary files for the child's output. *)
  let stdout_fn, stdout_fd = Unix.mkstemp "/tmp/stdout" in
  let stderr_fn, stderr_fd = Unix.mkstemp "/tmp/stderr" in
  start_time := Time_ns.now ();
  match Unix.fork () with
  | `In_the_child -> (
      ( match output with
      | `None ->
          let null = Unix.openfile ~mode:[ Unix.O_WRONLY ] "/dev/null" in
          Unix.dup2 ~src:null ~dst:Unix.stdout;
          Unix.dup2 ~src:null ~dst:Unix.stderr
      | `Saved ->
          Unix.dup2 ~src:stdout_fd ~dst:Unix.stdout;
          Unix.dup2 ~src:stderr_fd ~dst:Unix.stderr
      | `Standard -> () );
      match runnable with
      | `Program (program, args) ->
          never_returns
            (Unix.exec ~prog:program ~argv:(program :: args) ~use_path:true ())
      | `Closure func ->
          func ();
          exit 0 )
  | `In_the_parent child_pid ->
      let result = wait_for_child mem_limit time_limit child_pid in
      let result =
        match output with
        | `Saved ->
            { result with stdout = Some stdout_fn; stderr = Some stderr_fn }
        | _ -> result
      in
      start_time := Time_ns.epoch;
      end_time := Time_ns.epoch;
      result

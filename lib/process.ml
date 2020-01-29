open! Core

(** Process handling from Core_extended v0.11 *)

let waitpid_nohang pid =
  match Unix.wait_nohang (`Pid pid) with
  | None -> None
  | Some (v, res) ->
      assert (Pid.(v = pid));
      Some res

(** wait for a given pid to exit;
      returns true when the process exits and false if the process is still runing
      after waiting for [span]
  *)
let wait_for_exit ~is_child span pid =
  let end_time = Time.add (Time.now ()) span in
  let exited () =
    if is_child then
      match waitpid_nohang pid with None -> true | Some _ -> false
    else
      (* This is the equivalent of calling the C kill with 0 (test whether a
         process exists) *)
      match Signal.send (Signal.of_system_int 0) (`Pid pid) with
      | `Ok -> true
      | `No_such_process -> false
  in
  let rec loop () =
    if Time.( > ) (Time.now ()) end_time then false
      (* We need to explicitely waitpid the child otherwise we are sending signals
       to a zombie*)
    else if not (exited ()) then true
    else (
      Time.pause (sec 0.1);
      loop () )
  in
  loop ()

(** kills a process by sending [signal]; waiting for [wait_for] and then
    sending a [sigkill].
    You need to set is_child to true when killing child processes or run waitpid
    on them in another.
    @raises Failure if the target program hangs for more that [wait_for] after
    receiving the [sigkill].
    caveat: [is_child:false] (the default) is racy: it can both send signals to wrong
    processes and it can also fail to notice that the target died.
*)
let kill ?(is_child = false) ?(wait_for = sec 2.0) ?(signal = Signal.term) pid =
  Signal.send_exn signal (`Pid pid);
  if not (wait_for_exit ~is_child wait_for pid) then (
    ( match Signal.send Signal.kill (`Pid pid) with
    | `No_such_process ->
        if is_child then
          failwith
            "Process.kill got `No_such_process even though the process was a \
             child we never waited for"
    | `Ok -> () );
    if not (wait_for_exit ~is_child wait_for pid) then
      failwithf "Process.kill failed to kill %i%s" (Pid.to_int pid)
        ( if is_child then ""
        else " (or the process wasn't collected by its parent)" )
        () )

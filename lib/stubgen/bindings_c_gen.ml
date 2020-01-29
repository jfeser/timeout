open! Core

let c_headers = {|
#include <sys/time.h>
#include <sys/resource.h>
|}

let main () =
  Out_channel.with_file "bindings_stubs_gen.c" ~f:(fun ch ->
      let fmt = Format.formatter_of_out_channel ch in
      Format.fprintf fmt "%s@\n" c_headers;
      Cstubs_structs.write_c fmt (module Bindings.Stubs);
      Format.pp_print_flush fmt ())

let () = main ()

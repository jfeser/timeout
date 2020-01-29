module Stubs (S : Cstubs_structs.TYPE) = struct
  module Timeval = struct
    type timeval

    type t = timeval Ctypes.structure

    let t : t S.typ = S.structure "timeval"

    let tv_sec = S.(field t "tv_sec" int)

    let tv_usec = S.(field t "tv_usec" long)

    let () = S.seal t
  end

  module Rusage = struct
    type rusage

    type t = rusage Ctypes.structure

    let t : t S.typ = S.structure "rusage"

    let ru_maxrss = S.(field t "ru_maxrss" long)

    let ru_utime = S.(field t "ru_utime" Timeval.t)

    let ru_stime = S.(field t "ru_stime" Timeval.t)

    let () = S.seal t
  end
end

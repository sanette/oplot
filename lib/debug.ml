let debug = true

let print s =
  Printf.ksprintf (fun s -> if debug then print_endline s) s

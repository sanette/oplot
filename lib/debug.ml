open Tsdl

exception Sdl_error of string

let map_opt f o = match o with None -> None | Some x -> Some (f x)

let debug =
  match Sys.getenv_opt "OPLOT_DEBUG" |> map_opt String.lowercase_ascii with
  | Some "true" | Some "1" -> true
  | _ -> false

let print s = Printf.ksprintf (fun s -> if debug then print_endline s) s
let pr s = Printf.ksprintf print_endline s

let go : 'a Tsdl.Sdl.result -> 'a = function
  | Error _ -> raise (Sdl_error ("SDL ERROR: " ^ Sdl.get_error ()))
  | Ok r -> r

let timeit ?name ?(n = 1) f =
  let t0 = Unix.gettimeofday () in
  let res = ref None in
  for _ = 1 to n do
    res := Some (f ())
  done;
  let t = Unix.gettimeofday () -. t0 in
  let name = match name with None -> "" | Some s -> "[" ^ s ^ "]: " in
  if n = 1 then pr "%stotal time: %fs\n" name t
  else pr "%stotal time for n=%i : %fs  (average:%gs)\n" name n t (t /. float n);
  match !res with
  | Some v -> v
  | _ -> raise (Invalid_argument "timeit: n must be positive")

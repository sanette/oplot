open Tsdl

exception Sdl_error of string

let debug = true
let print s = Printf.ksprintf (fun s -> if debug then print_endline s) s

let go : 'a Tsdl.Sdl.result -> 'a = function
  | Error _ -> raise (Sdl_error ("SDL ERROR: " ^ Sdl.get_error ()))
  | Ok r -> r

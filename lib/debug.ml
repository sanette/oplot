open Tsdl

exception Sdl_error of string

let map_opt f o = match o with None -> None | Some x -> Some (f x)

let debug =
  match Sys.getenv_opt "OPLOT_DEBUG" |> map_opt String.lowercase_ascii with
  | Some "true" | Some "1" -> true
  | _ -> false

let print s = Printf.ksprintf (fun s -> if debug then print_endline s) s

let go : 'a Tsdl.Sdl.result -> 'a = function
  | Error _ -> raise (Sdl_error ("SDL ERROR: " ^ Sdl.get_error ()))
  | Ok r -> r

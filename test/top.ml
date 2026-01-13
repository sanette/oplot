#use "topfind"

#thread;;

Printf.printf "Sys.interactive = %b\n" !Sys.interactive;;

Printf.printf "Backend = %s\n"
  (match Sys.backend_type with
  | Native -> "native"
  | Bytecode -> "bytecode"
  | Other s -> "other: " ^ s)
;;

#require "oplot"

let () =
  let open Oplot.Plt in
  let p = plot sin (-2.) 20. in
  let a = axis 0. 0. in

  (* The opam Continuous Integration does not allow to write into the home
     directory *)
  let output = Filename.temp_file "oplot" ".fig" in
  display ~dev:fig ~output [ Color red; p; Color black; a ];
  assert (Sys.file_exists output)

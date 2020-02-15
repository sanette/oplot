(* User-defined animation *)

#use "topfind";;
#require "oplot";;

open Oplot.Plt;;

let v = view 0. (-1.) 20. 1.;;
let t = text "Hello world" 10. 0. ~size:50;;

let f t1 t2 x = sin ((1. +. (sin t1)) *. (x -. 10.) -. t2);;

let anim v dev = let p = plot (f (float (elapsed ()) /. 1000.)
			         (float (elapsed ()) /. 1237.)) 0. 20. in
  object_plot p (Some v) ~dev:dev;;

display [ v ; Color red ; t; Color cyan ; User anim ] ~dev:gl;;

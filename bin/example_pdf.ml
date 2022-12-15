(* Basic Oplot example *)



open Oplot.Plt

let p = plot sin (-2.) 20.
let a = axis 0. 0.;;

display ~dev:pdf [ Color red; p; Color black; a ];;

quit ()

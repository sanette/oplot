(* Basic Oplot example *)
#use "topfind";;
#thread;;
#require "oplot";;

open Oplot.Plt;;

let p = plot sin (-2.) 20.
let a = axis 0. 0.;;

display [ Color red; p; Color black; a ];;

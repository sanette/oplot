(* Oplot example: plot the Airy function, from the GSL library *)
#use "topfind"

#thread

#require "oplot"

#require "gsl"

open Oplot.Plt

let airyai x = Gsl.Sf.airy_Ai x Gsl.Fun.SIMPLE
let p = plot airyai (-15.) 5.
let a = axis 0. 0.;;

display [ Color red; p; Color black; a ]

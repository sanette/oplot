(* GSL Gamma function, adaptative plot, and LaTeX formula *)

#use "topfind";;
#require "gsl";;
#require "oplot";;

module Math = struct
  Gsl.Error.init ()
  let gamma x = 
    try Gsl.Sf.gamma x
    with 
    | Gsl.Error.Gsl_exn _ -> nan
    | e -> raise e
end

open Oplot.Plt;;

let p1 = view (-2.5) (-10.) (4.) (10.);;
let p2 = color 1. 0. 0.;;
let p3 = adapt_plot Math.gamma (-2.5) (4.);;
let p4 = color 0.74902 0.74902 0.74902;;
let p5 = axis (0.) (0.);;
let p6 = color 0. 0. 0.;;
let p7 = latex "The $\\Gamma$ function" ~size:24 ~align:LEFT (1.) (5.);;
let p8 = latex "$\\displaystyle\\Gamma(x) = \\int_0^{+\\infty} t^{x-1} e^{-t} dt$" ~size:20 ~align:LEFT (0.8) (-5.);;
let sh = [ p1; p2; p3; p4; p5; p6; p7; p8 ];;
display sh;;

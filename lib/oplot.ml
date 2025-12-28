Debug.print "* Loading oplot";;

(* Simple mathematical plotter library for `ocaml` with fast graphics (opengl),
   LaTeX display, and high quality vector output (xfig, postscript or PDF)

    Copyright (c) 2006-2025 VU NGOC San *)

(* This program was initially developped by the author at the University of
   Grenoble a long time ago. *)

(* This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details. *)

include Oplot_intf
module Points = Points
module Debug = Debug
module type GRAPHICS = Make_graphics.GRAPHICS

(* The initial modules are grouped and re-split into two main modules: Plt for
   all usual functions and Internal for specific needs like goplot. TODO split
   this from stratch. *)

module Make (G : GRAPHICS) : S = struct
  module Main = Make_core.Make (G)
  include Main
  module Internal = Main
end

module Plt = Make (Make_graphics.Dummy)

(*
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

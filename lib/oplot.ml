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

module Points = Points

module type GRAPHICS = Make_graphics.GRAPHICS

module Core = Core.Make (Make_graphics.Dummy)

(* The initial modules are grouped and re-split into two main modules: Plt for
   all usual functions and Internal for specific needs like goplot. TODO split
   this from stratch. *)

(* module type PltS = Make_plt.S *)

module Plt = struct
  include Core
  module Internal = Core
end

(*
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

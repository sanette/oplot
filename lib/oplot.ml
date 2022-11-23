Debug.print "* Loading oplot"

(* Simple mathematical plotter library for `ocaml` with fast graphics (opengl),
   LaTeX display, and high quality vector output (xfig, postscript or PDF)

    copyright (c) 2006-2022 VU NGOC San *)

(* This program was initially developped by the author at the University of
   Grenoble. *)

(* This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details. *)

module Points = Points

(* The initial modules are grouped and re-split into two main modules: Plt for
   all usual functions and Internal for specific needs like goplot *)

module Plt = struct
  include Oplotdef
  include Renderinit
  include Sysinit
  include Oplotmain
end

module Internal = struct
  include Oplotmain
  include Oplotdef
  include Renderinit
  include Sysinit
end

(*
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

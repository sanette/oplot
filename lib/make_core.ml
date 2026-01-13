(* Constructing Oplot's main module *)

module Make (Graphics : Make_graphics.GRAPHICS) = struct
  include Common
  include Oplotdef
  include Renderinit
  include Sysinit
  module Oplotmain = Oplotmain.Make (Graphics)
  include Oplotmain
  module Isocurve = Isocurve
  include Addons
end

(*
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

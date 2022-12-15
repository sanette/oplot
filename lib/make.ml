module Make (Graphics : Make_graphics.GRAPHICS) = struct
  include Oplotdef
  include Renderinit
  include Sysinit
  module Oplotmain = Oplotmain.Make (Graphics)
  include Oplotmain
end

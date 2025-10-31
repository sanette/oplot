module Make (Graphics : Make_graphics.GRAPHICS) = struct
  include Common
  include Oplotdef
  include Renderinit
  include Sysinit
  module Oplotmain = Oplotmain.Make (Graphics)
  include Oplotmain
end

(* Make the Graphics module optional *)

module type GRAPHICS = sig
  type color = int
  type event = Button_down | Button_up | Key_pressed | Mouse_motion | Poll

  type status = {
    mouse_x : int;  (** X coordinate of the mouse *)
    mouse_y : int;  (** Y coordinate of the mouse *)
    button : bool;  (** true if a mouse button is pressed *)
    keypressed : bool;  (** true if a key has been pressed *)
    key : char;  (** the character for the key pressed *)
  }

  val open_graph : string -> unit
  val close_graph : unit -> unit
  val synchronize : unit -> unit
  val rgb : int -> int -> int -> color
  val set_line_width : int -> unit
  val set_color : color -> unit
  val plots : (int * int) array -> unit
  val draw_poly_line : (int * int) array -> unit
  val fill_poly : (int * int) array -> unit
  val draw_segments : (int * int * int * int) array -> unit
  val set_text_size : int -> unit
  val set_font : string -> unit
  val moveto : int -> int -> unit
  val draw_string : string -> unit
  val read_key : unit -> char
  val clear_graph : unit -> unit
  val size_x : unit -> int
  val size_y : unit -> int
  val set_window_title : string -> unit
  val auto_synchronize : bool -> unit
  val wait_next_event : event list -> status
  val text_size : string -> int * int
end

module Dummy = struct
  let noway s = raise (Oplotdef.Not_implemented ("Graphics." ^ s))

  type color = int
  type event = Button_down | Button_up | Key_pressed | Mouse_motion | Poll

  type status = {
    mouse_x : int;  (** X coordinate of the mouse *)
    mouse_y : int;  (** Y coordinate of the mouse *)
    button : bool;  (** true if a mouse button is pressed *)
    keypressed : bool;  (** true if a key has been pressed *)
    key : char;  (** the character for the key pressed *)
  }

  let open_graph _ = noway "open_graph"
  let close_graph () = noway "close_graph"
  let synchronize () = noway "synchronize"
  let rgb _ _ _ = noway "rgb"
  let set_line_width _ = noway "set_line_width"
  let set_color _ = noway "set_color"
  let plots _ = noway "plots"
  let draw_poly_line _ = noway "draw_poly_line"
  let fill_poly _ = noway "fill_poly"
  let draw_segments _ = noway "draw_segments"
  let set_text_size _ = noway "set_text_size"
  let set_font _ = noway "set_font"
  let moveto _ _ = noway "moveto"
  let draw_string _ = noway "draw_string"
  let read_key () = noway "read_key"
  let clear_graph () = noway "clear_graph"
  let size_x () = noway "size_x"
  let size_y () = noway "size_y"
  let set_window_title _ = noway "set_window_title"
  let auto_synchronize _ = noway "auto_synchronize"
  let wait_next_event _ = noway "wait_next_event"
  let text_size _ = noway "text_size"
end

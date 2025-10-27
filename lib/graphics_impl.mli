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

module type S = sig
  (** {1 Oplot internals}

    Oplot internal functions are useful for creating user interfaces. *)

  open Common
  val reset_time : ?t0:int -> unit -> unit
  val has_anim : plot_object -> bool
  val gllist_empty : unit -> gllist
  val get_view : view ref -> plot_object
  type gl_handler = SDL | GLUT | GTK
  val set_default_gl : gl_handler -> unit
  val init : unit -> unit
  val get_mouse_x : unit -> int
  val get_mouse_y : unit -> int
  val set_mouse_x : int -> unit
  val set_mouse_y : int -> unit
  val get_window_height : unit -> int
  val get_frame_length : unit -> int
  val set_frame_length : int -> unit
  val gtk_mainloop : plot_object -> int
  val gl_init : ?show:bool -> unit -> unit
  val gl_resize : unit -> unit
  val scale : float -> float
  val iscale : int -> int
  val point_of_pixel : int * int -> view option -> float * float
  val set_line_width : ?dev:plot_device -> float -> unit
  val set_point_size : ?dev:plot_device -> float -> unit
  val get_light : unit -> bool
  val toggle_light : unit -> unit
  val force_refresh : unit -> unit
  val gl_mouse_motion : int -> int -> unit
  val interrupt : unit -> unit
  val latex_to_sdl : string -> int -> Tsdl.Sdl.surface
  exception Shell_error of (int * string)
  val oplot_dir : string
  val home_dir : string
  val first_time : unit -> bool
  val has_latex : bool
  val has_gs : bool
  val pngalpha : unit -> bool
  val has_fig2dev : bool
end

module type S = sig
  open Points
  open Common

  val black : color
  val white : color
  val red : color
  val green : color
  val blue : color
  val yellow : color
  val cyan : color
  val magenta : color


  (** {2 Defining plot objects}

      'Defining' means computing the coordinates of the points and lines to
      display, but not actually displaying them. We call a "sheet" a list of
      objects to be displayed. *)

  (** {3 2D objects}

      Helper functions for creating 2D plot objects *)

  val point : float * float -> point
  (** A single point (x,y). *)

  val axis : float -> float -> plot_object
  (** [axis x0 y0] creates an [Axis] object with two axis crossing at the point
      ([x0],[y0]). *)

  val parametric_plot :
    (float -> float) ->
    (float -> float) ->
    ?pas:float ->
    ?adapt:bool ->
    float ->
    float ->
    plot_object
  (** [parametric_plot fx fy t0 t1] computes a parametric curve given by the
      points ([fx](t), [fy](t)), for t varyingn from [t0] to [t1]. If [adapt] is
      true, the step will adapt to the arc-length of the curve. *)

  val point_plot_f :
    (float -> float) -> ?pas:float -> float -> float -> plot_object
  (** [point_plot_f f x0 x1] computes a subset of the graph of the function [f]
      obtained by isolated points with a fixed horizontal step. *)

  val line_plot_f :
    (float -> float) -> ?pas:float -> float -> float -> plot_object
  (** Similar to {!point_plot_f} but the points are joined by line segments. *)

  val plot : (float -> float) -> ?pas:float -> float -> float -> plot_object
  (** Alias for {!line_plot_f}. *)

  val adapt_plot :
    (float -> float) -> ?pas:float -> float -> float -> plot_object
  (** Similar to {!line_plot_f}, but the plot will be dynamically cropped to the
      current {!type-Common.view} object. It returns an {!Common.Adapt} object. *)

  val anim_plot :
    (float -> float -> float) ->
    ?pas:float ->
    ?t0:float ->
    ?t1:float ->
    float ->
    float ->
    plot_object
  (** If [f] is a function two parameters [t] and [x], then [anim_plot f x0 x1]
      will generate an animation of the graph of the functions [f t] for [t]
      evolving with real time. The resulting object is of type {!Common.User}. *)

  val dot_plot :
    ?dot:(float -> float -> plot_object) ->
    ?view:plot_object ->
    (float * float) list ->
    plot_object list
  (** [dot_plot ~dot list] draws a dot at each position [(x,y)] in the given
      [list]. Each dot is plotted using the [dot] function. By default, if
      [~dot] is not specified, a single pixel is drawn. Another possibility is
      to use the{!diamond} function. *)

  val diamond : ?size:float -> float -> float -> plot_object
  (** Draw a small diamond (lozange) at the given [x y] position. *)

  val box : float -> float -> float -> float -> plot_object
  (** [box x0 y0 x1 y1] draws a filled box given by the diagonal points [x0,y0]
      and [x1,y1]. *)

  val text :
    string -> ?size:int -> ?align:align -> float -> float -> plot_object
  (** [text s x y] draws the string [s] at the position ([x],[y]). *)

  val move_text : text -> point -> unit
  (** Move text at position given by [point]. *)

  val latex :
    string -> ?size:int -> ?align:align -> float -> float -> plot_object
  (** Similar to {!val-text} but the rendered text is the result of LaTeX
      compilation of the given string. *)

  val view : float -> float -> float -> float -> plot_object
  (** [view x0 y0 x1 y1] creates a {!Common.View} object indicating the bounding box
      for subsequent drawings. *)

  (** {3 3D objects}

      Helper functions for creating 3D plot objects *)

  val point3 : float * float * float -> point3
  (** A single 3D point (x,y,z). *)

  val surf3d_plot :
    (float -> float -> float) ->
    (float -> float -> float) ->
    (float -> float -> float) ->
    ?width:int ->
    ?height:int ->
    ?wire:bool ->
    float ->
    float ->
    float ->
    float ->
    plot_object
  (** [surf3d_plot fx fy fz u0 v0 u1 v1] computes the parametric surface spanned
      by the map (u,v)->(fx(u,v), fy(u,v), fz(u,v)) when (u,v) varies in the
      range [\[u0,u1\] ✕ \[v0,v1\]]. *)

  val grid_plot :
    (float -> float -> float) ->
    ?wire:bool ->
    ?width:int ->
    ?height:int ->
    float ->
    float ->
    float ->
    float ->
    plot_object
  (** [grid_plot f x0 y0 x1 y1] computes the graph of the function [f] of two
      variables x y, when (x,y) varies in the range [\[x0,x1\] ✕ \[y0,y1\]]. *)

  (** {3 Other objects} *)

  val color : float -> float -> float -> plot_object
  (** Specifies the RGB color for subsequent drawings. *)

  val freeze : int -> plot_object
  (** [freeze t] creates a {!Common.Freeze} for [t] ms. *)

  val pause : int -> plot_object
  (** [pause t] creates a {!Common.Pause} for [t] ms. *)

  val rotate : float -> float -> float -> float -> float -> plot_object
  (** [rotate x y z theta t] rotates the 3D scene with angular velocity [theta]
      (in radians) aroung the axis ([x],[y],[z]), during time [t]. *)

  (** {2 Displaying the plot objects}

      Various devices can be used to render the plots. *)

  val display :
    ?dev:user_device ->
    ?fscreen:bool ->
    ?output:string ->
    plot_object list ->
    unit
  (** Initialize the graphics device and display the plot objects *)

  (** {3 Available devices} *)

  val x11 : user_device
  (** Software rendering in a separate window. *)

  val gl : user_device
  (** Hardware (opengl) rendering in a separate window. This is the default. A
      few keys are active while the window is open. Press [h] for help. *)

  val fig : user_device
  (** Render to an xfig file. *)

  val xfig : user_device
  (** Open in the [xfig] program, if present. *)

  val eps : user_device
  (** Render to an EPS (encapsulated postscript) file. *)

  val pdf : user_device
  (** Render to a PDF (portable document format) file. *)

  val gv : user_device
  (** Render to EPS and open with a postscript viewer like [gv]. *)

  val bmp : user_device
  (** Render to a BMP image. Deprecated. It will actually render a PNG image. *)

  val png : user_device
  (** Render to a PNG image. *)

  val img : user_device
  (** Open in an image viewer. *)

  (** {3 Rendering parameters} *)

  val resize_window : int -> int -> unit

  val get_gl_scale : unit -> float
  (** The GL_SCALE is used to accomodate for HI-DPI screens. A "normal" screen
      of DPI about 110 should have GL_SCALE=1. A HI-DPI screen can typically
      have a scale of 2 or 2.5; on linux the GL_SCALE is detected at startup. It
      can be modified by the user. *)

  val set_gl_scale : float -> unit

  val quit : ?dev:plot_device -> unit -> unit
  (** Close the current rendering window and clear the temporary directory. *)

  val get_tmp_dir : unit -> string

  (** {3 Drawing specific objects}

      These functions should not be used interactively, because they necessitate
      the graphics window to be already opened by {!display}; but they can be
      interesting when programming a {!Common.User} object. *)

  val set_color : ?dev:plot_device -> color -> unit

  val object_plot :
    ?addcounter:bool -> dev:plot_device -> plot_object -> view option -> unit
  (** Draw a single object, but not a Sheet. The device must have been
      initialized before.

      If [addcounter] is true (default) the object will be considered as a new
      element of the currently displayed sheet; otherwise, it will be considered
      as being part of the currently displayed object: this only affects the
      {!Common.Pause} mechanism. *)

  val elapsed : unit -> int
  (** Time elapsed from the openning of the opengl window. *)

  val user_flush : plot_device -> unit
  (** Synchronize graphics output by swapping back and front buffers. Hence two
      consecutive calls will result in flicker. See {!copy_back_buffer}. *)

  val copy_back_buffer : unit -> unit
  (** Copy backbuffer to front buffer. If I use this intensively I can really
      hear my graphics card... *)

  (* val draw_points : points -> ?dev:plot_device -> ?dep:int -> view option -> unit *)
end

(* Main interface file for Oplot *)

module type S = sig
  open Points
  (** Main Oplot functions

      This module contains all functions for defining and rendering plots. *)

  type plot_device = GRAPHICS | GL | FIG
  type view = point * point
  type view3 = point3 * point3
  type points = point list
  type axis
  type image
  type latex
  type text
  type imatrix = int array array
  type fmatrix = float array array
  type grid = fmatrix * view3 * bool
  type surf3d = fmatrix * fmatrix * fmatrix * view3 * bool
  type curve3d = point3 list * view3
  type move3d
  type gllist
  type color = { r : float; g : float; b : float }
  type align = CENTER | LEFT | RIGHT

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

  type plot_object =
    | Points of points  (** A list of points. *)
    | Lines of points list
        (** The points of each sublist are joined by a line segment. *)
    | Poly of points  (** Closed polygonal line. *)
    | View of view option  (** Indicate the x-y range to display. *)
    | Axis of axis  (** Axis with divisions. *)
    | Color of color  (** Indicate the color to draw subsequent objects. *)
    | Text of text  (** Position a text at some (x,y) coordinate. *)
    | Matrix of imatrix
        (** Checkboard-like matrix view with 0-255 greyscale. *)
    | Grid of grid * gllist  (** 3D mountain-like representation of a matrix. *)
    | Surf3d of surf3d * gllist  (** 3D parametric surface. *)
    | Curve3d of curve3d * gllist  (** 3D points joined by line segments. *)
    | Move3d of move3d  (** Animate the 3D object by a uniform rotation. *)
    | Pause of int
        (** Display the current state of the sheet, and wait before displaying
            next object, but don't stop animation in the opengl window. This
            only works for interactive displays using the {!GL} device. *)
    | Freeze of int
        (** Display the current state of the sheet and suspend all display for
            the given time. *)
    | Clear of color  (** Clear graphics. *)
    | Adapt of
        (view option * plot_object option) ref * (view option -> plot_object)
        (** Any object that needs to adapt itself to the current View. *)
    | User of (view -> plot_device -> unit)
        (** Execute any user-defined program. *)
    | Anim of (float -> plot_object)
        (** Repeatedly execute any user-defined program. *)
    | Sheet of plot_object list  (** Group plot objects. *)

  (** {3 2D objects}

      Helper functions for creating 2D plot objects *)

  val point : float * float -> point
  (** A single point (x,y). *)

  val axis : float -> float -> plot_object
  (** [axis x0 y0] creates an [Axis] object with two axis crossing at the point
      ([x0],[y0]).

      Example:

      {@ocaml env=EXAMPLE[
        # let a = axis 0. 0.;;
        # display [ a ];;
      ]} *)

  val parametric_plot :
    (float -> float) ->
    (float -> float) ->
    ?step:float ->
    ?adapt:bool ->
    float ->
    float ->
    plot_object
  (** [parametric_plot fx fy t0 t1] computes a parametric curve given by the
      points ([fx](t), [fy](t)), for t varying from [t0] to [t1]. If [adapt] is
      true, the step will adapt to the arc-length of the curve. *)

  val point_plot_f :
    (float -> float) -> ?step:float -> float -> float -> plot_object
  (** [point_plot_f f x0 x1] computes a subset of the graph of the function [f]
      obtained by isolated points with a fixed horizontal step. *)

  val line_plot_f :
    (float -> float) -> ?step:float -> float -> float -> plot_object
  (** Similar to {!point_plot_f} but the points are joined by line segments. *)

  val plot : (float -> float) -> ?step:float -> float -> float -> plot_object
  (** Alias for {!line_plot_f}. *)

  val adapt_plot :
    (float -> float) -> ?step:float -> float -> float -> plot_object
  (** Similar to {!line_plot_f}, but the plot will be dynamically cropped to the
      current {!type-view} object. It returns an {!Adapt} object. *)

  val implicit_curve :
    ?pixel_size:int * int ->
    ?grid_size:int * int ->
    ?sub_size:int * int ->
    ?depth:int ->
    ?steps:int ->
    ?better:int ->
    (float ->float -> float) -> view -> plot_object
  (** [implicit_curve f (p0, p1)] returns a {!Lines} object that draws (an
      approximation) of the level set [f x y = 0] inside the box delimited by
      the diagonal points [p0,p1].

      If the result is not satisfactory, or if you want a faster computation,
      you can play with the optional parameters; their meanings are explained in
      {!Isocurve.compute_level}.

     *)

  module Isocurve : sig
    (** Additional utilities for drawing and inspecting curves defined by an
        implicit equation. *)

    type info = {
      grid_size : int * int; (** Size of the initial sampling array. See {!compute_level}. *)

      grid : plot_object; (** Plottig this object will draw the initial grid. This
                              field is empty if [debug=false] in the call of
                              {!compute_level}. *)

      boxes : plot_object; (** Plotting this object will draw the cells where
                               subsampling has been performed. *)

      steps : int; (** See {!compute_level}. *)
      depth : int; (** See {!compute_level}. *)

      poles : int; (** Number of sign changes of [f] that have not been considered
                       as zeros, but rather as poles (where the function diverges or
                       is highly discontinuous.)*)

      message : Buffer.t (** Reading this string willl give you some debug
                             information. *)
    }

    val compute_level : ?debug:bool ->
      ?pixel_size:int * int ->
      ?grid_size:int * int ->
      ?sub_size:int * int ->
      ?steps:int ->
      ?better:int -> ?depth:int -> (point -> float) -> point * point -> plot_object * info
    (** [compute f (p0, p1)] returns a {!Lines} object that draws (an
          approximation) of the level set [f p = 0] inside the box delimited by
          the diagonal points [p0,p1].

         This is essentialy the same as {!Plt.implicit_curve} but returns both a
         [plot_object] and an [info] value. Use this version only if you need to
         obtain debug information.

        The meaning of the optional parameters is as follows.

        - [pixel_size] is a hint to size in pixels of the box where the curve
            will be drawn; it is used to detect the resolution at which the
            computations should be made. The default is [(500,500)].
        - [grid_size] is size of the initial sampling of the function [f]. For
            instance, if [grid_size=(7,5)] the (x,y) region where the implicit curve
            is sought is divised into 7x5 cells; hence there are 8=7+1 sampling
            points in the horizontal (x) direction, and 6=5+1 sampling points in the
            vertical direction. By default [grid_size] is automatically detected by
            looking at the function oscillations, starting from an initial size of
            (34,34). Specifying this parameter skips this detection, resulting in
            less evaluations of the function.

            Note however that sub-sampling will be applied (if [sub_size] is not
            [(1,1)]) in the cells where the curvature of the levelset is high.

        - [sub_size] is the size of the sub-grid recursively used for a better
            approximation of the curve in cells where the curvature seems too
            high. The default is [(2,2)].
        - [depth] is the maximum number of recursive subdivisions of cells. By
            default it is computed in such a way that the smallest sub-cell is not
            smaller than the required pixel resolution. You may reduce it for
            fastest computation. [depth=4] is typically a good choice.
        - [steps] is the maximum number of steps used by the Newton method to
            determine the position of the curve on the cells boundaries. Currently
            the default is 4.
        - [better] is a quick way to obtain a more precise curve without setting
          any other parameter. You can first try [better=1] , then [better=2],
          etc. Using a negative number will degrade the precision of the curve. Of
          course the default is 0.


        In the image below we plot the initial grid (in green) and the recursive
        subgrids (in cyan). We see that the curve looks nice and smooth while
        the initial grid was not a priori fine enough.

         {%html:<img src="isocurve_debug.png" class="oplot" alt="isocurve example">%}

        (Image obtained by the [heart.ml] example.)

    *)

    val print_info : info -> unit

  end


  val anim_plot :
    (float -> float -> float) ->
    ?step:float ->
    ?t0:float ->
    ?t1:float ->
    float ->
    float ->
    plot_object
  (** If [f] is a function two parameters [t] and [x], then [anim_plot f x0 x1]
      will generate an animation of the graph of the functions [f t] for [t]
      evolving with real time. The resulting object is of type {!Anim}. *)

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
  (** [view x0 y0 x1 y1] creates a {!View} object indicating the bounding box
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
      range [[u0,u1] ✕ [v0,v1]]. *)

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
      variables x y, when (x,y) varies in the range [[x0,x1] ✕ [y0,y1]]. *)

  (** {3 Other objects} *)

  val color : float -> float -> float -> plot_object
  (** Specify the RGB color for subsequent drawings. *)

  val line_width : float -> plot_object
  (** Set line width for subsequent drawings. *)

  val freeze : int -> plot_object
  (** [freeze t] creates a {!Freeze} for [t] ms. *)

  val pause : int -> plot_object
  (** [pause t] creates a {!Pause} for [t] ms. *)

  val rotate : float -> float -> float -> float -> float -> plot_object
  (** [rotate x y z theta t] rotates the 3D scene with angular velocity [theta]
      (in radians) aroung the axis ([x],[y],[z]), during time [t]. *)

  val repeat : plot_object
  (* Inserting this object in a sheet will force contiuous looping over the
       sheet, as if there was some animation. Useful for animating User
       objects. *)

  (** {3 Retrieving data from plot objects} *)

  val get_points2 : plot_object -> points
  (** Obtain the list of 2D points, when relevant. *)

  (** {2 Displaying the plot objects}

      Various devices can be used to render the plots. *)

  type user_device

  val display :
    ?dev:user_device ->
    ?fscreen:bool ->
    ?output:string ->
    plot_object list ->
    unit
  (** Initialize the graphics device and display the plot objects *)

  (** {3 Available devices} *)

  val graphics : user_device
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
      interesting when programming a {!User} object. *)

  val set_color : ?dev:plot_device -> color -> unit

  val object_plot :
    ?addcounter:bool -> dev:plot_device -> plot_object -> view option -> unit
  (** Draw a single object, but not a Sheet. The device must have been
      initialized before.

      If [addcounter] is true (default) the object will be considered as a new
      element of the currently displayed sheet; otherwise, it will be considered
      as being part of the currently displayed object: this only affects the
      {!Pause} mechanism. *)

  val elapsed : unit -> int
  (** Time elapsed from the openning of the opengl window. *)

  val user_flush : plot_device -> unit
  (** Synchronize graphics output by swapping back and front buffers. Hence two
      consecutive calls will result in flicker. *)

  (* val draw_points : points -> ?dev:plot_device -> ?dep:int -> view option -> unit *)

  (** {2 Oplot internals} *)

  module Internal : sig
    (** Oplot internal functions are useful for creating user interfaces. *)

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
end

module type Intf = sig
  (** {1 Example}

      [Oplot] can be used in the toplevel. First load the library with

      {[
        #use "topfind"

        #thread

        #require "oplot"
      ]}

      You may open the {!Oplot.Plt} module for easy access to all plot
      functions.

      {[
        open Oplot.Plt
      ]}

      Draw the graph of the [sine] function with

      {[
        let p = plot sin (-2.) 20.
        let a = axis 0. 0.;;

        display [ Color red; p; Color black; a ]
      ]}

      This will open a window with the graphics, which should look like this:

      {%html:<img src="example.png" class="oplot" alt="oplot example">%}

      Press [F] for fullscreen toggle, [CTRL-S] for saving the image, and [ESC]
      or [Q] to close the window. Press [h] to see the list of active keys.

      Of course you can play with it:

      {[
        let rec sh i =
          if i == 0 then []
          else
            let p =
              line_plot_f
                (fun x -> sin (x +. (float_of_int i /. 50.)))
                0. 20. ~step:0.05
            in
            let c =
              color (float_of_int i /. 50.) (1. -. (float_of_int i /. 50.)) 0.
            in
            c :: p :: sh (i - 1)
        ;;

        display (sh 50)
      ]}

      {%html:<img src="example2.png" class="oplot" alt="oplot example">%} *)

  (** {1 Main Oplot functions} *)

  module Points = Points
  (** Types of points (2D or 3D). *)

  module Plt : S
  (** This module contains all plotting functions. *)

  (** {1 Adding a new graphics backend}

      These signatures allow you to add another backend. An example is to use
      the old [Graphics] library, see
      {{:https://github.com/sanette/oplot-graphics}oplot-graphics}. *)

  module type S = S
  module type GRAPHICS = Make_graphics.GRAPHICS

  module Make : GRAPHICS -> S
  (** Use this functor to create a new [Plt] module for adding a drawing
      backend. *)
end

(*
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

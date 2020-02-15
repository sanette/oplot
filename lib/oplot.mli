(** Simple mathematical plotter library for [ocaml] with fast graphics (opengl),
    LaTeX display, and high quality vector output (xfig, postscript or PDF)

        {%html:<img src="gamma.png" class="oplot" alt="oplot example">%}

    
    @version 0.5

    @author Vu Ngoc San
    
*)

(** {1 Example} 
    
    [Oplot] can be used in the toplevel. First load the library with
    
    {[
      #use "topfind";;
      #require "oplot";;
    ]}
    
    You may open the {!Oplot.Plt} module for easy access to all plot 
     functions.
    
    {[
      open Oplot.Plt;;
    ]}
    
    Draw the graph of the [sine] function with
    {[
      let p = plot sin (-2.) 20.
      let a = axis 0. 0. ;;
      
      display [Color red ; p ; Color black ; a];;
    ]}
    
    This will open a window with the graphics, which should look like this:
    
        {%html:<img src="example.png" class="oplot" alt="oplot example">%}

    Press [F] for fullscreen toggle, [CTRL-S] for saving the image, 
    and [ESC] or [Q] to close the window.
    
    Of course you can play with it:
    {[
      let rec sh i = if i==0 then [] else 
          let p = line_plot_f (fun x->sin (x+.(float_of_int i)/.50.)) 0. 20. ~pas:0.05 in 
          let c = color ((float_of_int i)/.50.) (1.-.((float_of_int i)/.50.)) 0. in 
      c::(p::(sh (i-1)));;
      
      display (sh 50) ;;
    ]}
        {%html:<img src="example2.png" class="oplot" alt="oplot example">%}
*)

(** Types of points *)
module Points : sig
  module Point2 : sig
    type t = {x: float; y: float}
  end
  module Point3 : sig
    type t = {x: float; y: float; z: float}
  end
  type point = Point2.t
  type point3 = Point3.t
end


(** {1 Main Oplot functions}

    This module contains all plotting functions.  *)

(** Main Oplot functions

    This module contains all functions for defining and rendering plots. *)
module Plt : sig

  open Points

  type plot_device = X11 | GL | FIG 

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
  type curve3d = (point3 list) * view3
  type move3d

  type gllist

  type color =  { r:float ; g:float ; b:float }
  val black : color
  val white : color
  val red : color
  val green : color
  val blue : color
  val yellow : color
  val cyan : color
  val magenta : color

  type align = CENTER | LEFT | RIGHT

  type text_flag = Normal | Latex

  type gl_handler = SDL | GLUT | GTK

  (** {2 Defining plot objects}

      'Defining' means computing the coordinates of the points and lines to
      display, but not actually displaying them.  *)

  type plot_object =
    | Points of points
    (** A list of points *)

    | Lines of points list
    (** The points of each sublist are joined by a line segment *)

    | Poly of points
    (** Closed polygonal line *)
  
    | View of view option
    (** Indicate the x-y range to display *)
  
    | Axis of axis
    (** Axis with divisions *)
  
    | Color of color
    (** Indicate the color to draw subsequent objects *)
  
    | Text of text
    (** Position a text at some (x,y) coordinate *)
  
    | Matrix of imatrix * (view option)
    (** Checkboard-like matrix view with 0-255 greyscale*)                  
  
    | Grid of grid * gllist
    (** 3D mountain-like representation of a matrix *)
              
    | Surf3d of surf3d * gllist
    (** 3D parametric surface *)
                
    | Curve3d of curve3d * gllist
    (** 3D points joined by line segments *)
  
    | Move3d of move3d
    (** Animate the 3D object by a uniform rotation *)
  
    | Pause of int
    (** Wait before displaying next object, but don't stop animation in the
       opengl window *)
  
    | Freeze of int
    (** Suspend all display for the given time *)
  
    | Clear of color
    (** Clear graphics *)
  
    | Adapt of ((view option) * (plot_object option)) ref *
               (view option -> plot_object)
    (** Any object that need to adapt itself to the current View *)
  
    | User of (view -> plot_device -> unit)
    (** Execute any user-defined program *)
  
    | Sheet of plot_object list
    (** Group plot objects *)


  (** {3 2D objects} 

      Helper functions for creating 2D plot objects *)
          
  val point : float * float -> point
  (** A single point (x,y). *)
    
  val axis : float -> float -> plot_object
  (** [axis x0 y0] creates an [Axis] object with two axis crossing at the point
     ([x0],[y0]). *)

  val parametric_plot :
    (float -> float) -> (float -> float) ->
    ?pas:float -> ?adapt:bool -> float -> float -> plot_object
  (** [parametric_plot fx fy t0 t1] computes a parametric curve given by the
     points ([fx](t), [fy](t)), for t varyingn from [t0] to [t1]. If [adapt] is
     true, the step will adapt to the arc-length of the curve.  *)
  
  val point_plot_f :
    (float -> float) -> ?pas:float -> float -> float -> plot_object
  (** [point_plot_f f x0 x1] computes a subset of the graph of the function [f]
     obtained by isolated points with a fixed horizontal step. *)
  
  val line_plot_f :
    (float -> float) -> ?pas:float -> float -> float -> plot_object
  (** Similar to {!point_plot_f} but the points are joined by line segments. *)

  val plot :
    (float -> float) -> ?pas:float -> float -> float -> plot_object
  (** Alias for {!line_plot_f}. *)

  val adapt_plot :
    (float -> float) -> ?pas:float -> float -> float -> plot_object
  (** Similar to {!line_plot_f}, but the plot will be dynamically cropped to the
      current {!view} object. It returns an {!Adapt} object. *)

  val anim_plot : (float -> float -> float) ->
    ?pas:float -> ?t0:float -> ?t1:float -> float -> float -> plot_object
  (** If [f] is a function two parameters [t] and [x], then [anim_plot f x0 x1]
     will generate an animation of the graph of the functions [f t] for [t]
     evolving with real time. The resulting object is of type {!User}. *)
  
  val text :
    string -> ?size:int -> ?align:align -> float -> float -> plot_object
  (** [text s x y] draws the string [s] at the position ([x],[y]). *)
  
  val latex:
    string -> ?size:int -> ?align:align -> float -> float -> plot_object
  (** Similar to {!text} but the rendered text is the result of LaTeX
     compilation of the given string. *)

  val view: float -> float -> float -> float -> plot_object
  (** [view x0 y0 x1 y1] creates a {!View} object indicating the bounding box
     for subsequent drawings. *)
  
  (** {3 3D objects}

      Helper functions for creating 3D plot objects *)
  
  val point3 : float * float * float -> point3
  (** A single 3D point (x,y,z). *)
  
  val surf3d_plot : (float -> float -> float) ->
    (float -> float -> float) ->
    (float -> float -> float) ->
    ?width:int ->
    ?height:int -> ?wire:bool -> float -> float -> float -> float -> plot_object
  (** [surf3d_plot fx fy fz u0 v0 u1 v1] computes the parametric surface spanned
     by the map (u,v)->(fx(u,v), fy(u,v), fz(u,v)) when (u,v) varies in the
     range [[u0,u1] ✕ [v0,v1]]. *)
  
  val grid_plot : (float -> float -> float) ->
    ?wire:bool ->
    ?width:int -> ?height:int -> float -> float -> float -> float -> plot_object
  (** [grid_plot f x0 y0 x1 y1] computes the graph of the function [f] of two
     variables x y, when (x,y) varies in the range [[x0,x1] ✕ [y0,y1]]. *)

  (** {3 Other objects} *)
    
  val color: float -> float -> float -> plot_object
  (** Specifies the RGB color for subsequent drawings. *)

  val freeze : int -> plot_object
    (** [freeze t] creates a {!Freeze} for [t] ms. *)
  
  val pause : int -> plot_object
  (** [pause t] creates a {!Pause} for [t] ms. *)

  val rotate : float -> float -> float -> float -> float -> plot_object
  (** [rotate x y z theta t] rotates the 3D scene with angular velocity [theta]
      (in radians) aroung the axis ([x],[y],[z]), during time [t].  *)
   

  (** {2 Displaying the plot objects}

      Various devices can be used to render the plots.

  *)

  type user_device

  val display : ?dev:user_device ->
    ?fscreen:bool -> ?output:string -> plot_object list -> unit
  (** Display the plot objects *)

  val object_plot :
    ?addcounter:bool -> dev:plot_device -> plot_object -> view option -> unit
  (** Draw a single object, but not a Sheet *)
    
  val elapsed : unit -> int
  (** Time elapsed from the openning of the opengl window *)
    
  (** {3 Available devices} *)
    
  val x11 : user_device
  (** Software rendering in a separate window *)

  val gl : user_device
  (** Hardware (opengl) rendering in a separate window. This is the default *)

  val fig : user_device
  (** Render to an xfig file *)

  val xfig : user_device
  (** Open in the [xfig] program, if present *)

  val eps : user_device
  (** Render to an EPS (encapsulated postscript) file *)

  val pdf : user_device
  (** Render to a PDF (portable document format) file *)

  val gv : user_device
  (** Render to EPS and open with a postscript viewer like [gv] *)

  val bmp : user_device
  (** Render to a BMP image *)

  (* val png : user_device
   * (\** Render to a PNG image *\) *)

  val img : user_device
  (** Open in an image viewer *)

  (** {3 Rendering parameters} *)

  val resize_window : int -> int -> unit

  (** The GL_SCALE is used to accomodate for HI-DPI screens. A "normal" screen
      of DPI about 110 should have GL_SCALE=1. A HI-DPI screen can typically have
      a scale of 2 or 2.5; on linux the GL_SCALE is detected at startup. It can
      be modified by the user. *)
  val get_gl_scale : unit -> float
  val set_gl_scale : float -> unit

  val quit : ?dev:plot_device -> unit -> unit
  (** Close the current rendering window and clear the temporary directory *)

  val get_tmp_dir : unit -> string

end



(********************************************************)
(* Below is the more complete interface, used by goplot *)

(** {1 Oplot internals}

Oplot internal functions are useful for creating user interfaces.  *)

module Internal : sig

  open Plt
  (* type plot_device = Plt.plot_device
   * 
   * open Points
   *     
   * type view = point * point
   * type view3 = point3 * point3
   * type points = point list
   * type axis
   * type image
   * type latex
   * type text
   * type imatrix = int array array
   * type fmatrix = float array array
   * type grid = fmatrix * view3 * bool
   * type surf3d = fmatrix * fmatrix * fmatrix * view3 * bool
   * type curve3d = (point3 list) * view3
   * type move3d
   *        
   * type gllist
   *   
   * type color =  { r:float ; g:float ; b:float }
   * val black : color
   * val white : color
   * val red : color
   * val green : color
   * val blue : color
   * val yellow : color
   * val cyan : color
   * val magenta : color
   * 
   * type align = CENTER | LEFT | RIGHT
   * type text_flag = Normal | Latex
   * 
   * type plot_object =
   * | Points of points 
   * | Lines of points list
   * | Poly of points
   * | View of view option
   * | Axis of axis
   * | Color of color
   * | Text of text
   * | Matrix of imatrix * (view option)
   * | Grid of grid * gllist
   * | Surf3d of surf3d * gllist
   * | Curve3d of curve3d * gllist
   * | Move3d of move3d
   * | Pause of int
   * | Freeze of int
   * | Clear of color
   * | Adapt of ((view option) * (plot_object option)) ref * (view option -> plot_object)
   * | User of (view -> plot_device -> unit) 
   * | Sheet of plot_object list
   * 
   * val axis : float -> float -> plot_object
   * val parametric_plot :
   *   (float -> float) -> (float -> float) ->
   *   ?pas:float -> ?adapt:bool -> float -> float -> plot_object
   * val point_plot_f :
   *   (float -> float) -> ?pas:float -> float -> float -> plot_object
   * val line_plot_f :
   *   (float -> float) -> ?pas:float -> float -> float -> plot_object
   * val plot :
   *   (float -> float) -> ?pas:float -> float -> float -> plot_object
   * val adapt_plot :
   *   (float -> float) -> ?pas:float -> float -> float -> plot_object
   * val text :
   *   string -> ?size:int -> ?align:align -> float -> float -> plot_object
   * val latex:
   *   string -> ?size:int -> ?align:align -> float -> float -> plot_object
   * val surf3d_plot : (float -> float -> float) ->
   *   (float -> float -> float) ->
   *   (float -> float -> float) ->
   *   ?width:int ->
   *   ?height:int -> ?wire:bool -> float -> float -> float -> float -> plot_object
   * 
   * val grid_plot : (float -> float -> float) ->
   *   ?wire:bool ->
   *   ?width:int -> ?height:int -> float -> float -> float -> float -> plot_object
   * val view: float -> float -> float -> float -> plot_object
   * val color: float -> float -> float -> plot_object
   * val freeze : int -> plot_object
   * val pause : int -> plot_object
   * val rotate : float -> float -> float -> float -> float -> plot_object *)

  (** {2 Plot objects} *)
      
  val has_anim : plot_object -> bool
  val gllist_empty : unit -> gllist

  (* type user_device
   * val x11 : user_device
   * val gl : user_device
   * val fig : user_device
   * val xfig : user_device
   * val eps : user_device
   * val pdf : user_device
   * val gv : user_device
   * val bmp : user_device
   * val png : user_device
   * val img : user_device
   *   
   * type gl_handler = SDL | GLUT | GTK *)

  (** {2 Opengl rendering and interface} *)
    
  val set_default_gl : gl_handler -> unit
  
  val init : unit -> unit
  (** Set various rendering values to their defaults. *)
    
  val get_mouse_x : unit -> int
  val get_mouse_y : unit -> int
  val set_mouse_x : int -> unit
  val set_mouse_y : int -> unit
  val get_window_height : unit -> int
  (* val resize_window : int -> int -> unit *)

  val get_frame_length : unit -> int
  (** Get the target frame duration in ms. *)

  val set_frame_length : int -> unit
  
  val gtk_mainloop : plot_object -> int
  val gl_init : ?show:bool -> unit -> unit
  val gl_resize : unit -> unit

  (* (\** The GL_SCALE is used to accomodate for HI-DPI screens. A "normal" screen
   *    of DPI about 110 should have GL_SCALE=1. A HI-DPI screen can typically have
   *    a scale of 2 or 2.5; on linux the GL_SCALE is detected at startup. It can
   *    be modified by the user. *\)
   * val get_gl_scale : unit -> float
   * val set_gl_scale : float -> unit *)

  val scale : float -> float
  (** multiply by the GL_SCALE *)
    
  val iscale : int -> int
  (** multiply by the GL_SCALE and round *)

  val set_line_width : ?dev:plot_device -> float -> unit
  val set_point_size : ?dev:plot_device -> float -> unit
  val force_refresh : unit -> unit
  val gl_mouse_motion : int -> int -> unit
  val interrupt : unit -> unit
  (* val quit : ?dev:Def.plot_device -> unit -> unit *)
  
  (* val display : ?dev:Dev.user_device ->
   *   ?fscreen:bool -> ?output:string -> Def.plot_object list -> unit *)

  (** {2 Low-level rendering} *)
        
  (* val anim_plot : (float -> float -> float) ->
   *   ?pas:float -> ?t0:float -> ?t1:float -> float -> float -> Def.plot_object *)

  val latex_to_sdl : string -> int -> Sdlvideo.surface

  (** {2 Shell interaction with other programs} *)
                                        
  exception Shell_error of (int * string)

  val oplot_dir : string
  (** The directory shared by oplot and goplot for various assets *)
    
  val has_latex : bool
  (** Whether LaTeX is detected on the computer *)
  
  val has_gs : bool
  (** Whether [gs] ghostscript is detected on the computer *)

  val pngalpha : unit -> bool
  (** Whether [gs] supports the [pngalpha] device *)
  
  val has_fig2dev : bool
  (** Whether [fig2dev] is detected on the computer *)
  
  val first_time : unit -> bool
  (** Whether the [$HOME/.oplot] directory already exists *)
    
end


(* 
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

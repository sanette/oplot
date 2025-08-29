(* Interface for Common definitions of types for Oplot. San Vu Ngoc. *)

(* We have to make this complete signature public to allow the Make functor to
   generate extensions to oplot (like oplot-graphics) using the same Common
   interface. (If we hide some types by making them private, then the exported
   interface will be different -- and hence incompatible -- with the real
   interface used when compiling the Main module.) *)

(* However, can we make some types private later, in the Plt/Internal
   modules? *)

open Points

type plot_device = X11 | GL | FIG
type view = point * point
type view3 = point3 * point3
type points = point list
type align = CENTER | LEFT | RIGHT
type text_flag = Normal | Latex
type text_image = ([ `rgba ], [ `ubyte ]) GlPix.t * int * int

(* (image + dimensions de la partie intéressante) *)
type text = {
  mutable pos : point;
  text : string;
  size : int;
  align : align;
  flag : text_flag;
  mutable pix : text_image option;
}

type axis = {
  center : point;
  mutable view : view option;
  mutable ticks : (points * text list) option;
  mutable window_size : int * int;
}

type image
type latex
type imatrix = int array array
type fmatrix = float array array
type grid = fmatrix * view3 * bool
type surf3d = fmatrix * fmatrix * fmatrix * view3 * bool
type curve3d = point3 list * view3

type motion3d =
  | Translate of point3
  | Rotate of Geom.quaternion
  | Zoom of ((float -> float) * float option)

type mrange = { mutable min : float; mutable max : float }
type move3d = { move : motion3d; time : mrange; mutable init_time : int option }
type gllist = GlList.t option ref
type color = { r : float; g : float; b : float }

type plot_object =
  | Points of points  (** A list of points. *)
  | Lines of points list
      (** The points of each sublist are joined by a line segment. *)
  | Poly of points  (** Closed polygonal line. *)
  | View of view option  (** Indicate the x-y range to display. *)
  | Axis of axis  (** Axis with divisions. *)
  | Color of color  (** Indicate the color to draw subsequent objects. *)
  | Text of text  (** Position a text at some (x,y) coordinate. *)
  | Matrix of imatrix  (** Checkboard-like matrix view with 0-255 greyscale. *)
  | Grid of grid * gllist  (** 3D mountain-like representation of a matrix. *)
  | Surf3d of surf3d * gllist  (** 3D parametric surface. *)
  | Curve3d of curve3d * gllist  (** 3D points joined by line segments. *)
  | Move3d of move3d  (** Animate the 3D object by a uniform rotation. *)
  | Pause of int
      (** Display the current state of the sheet, and wait before displaying
          next object, but don't stop animation in the opengl window. This only
          works for interactive displays using the {!GL} device. *)
  | Freeze of int
      (** Display the current state of the sheet and suspend all display for the
          given time. *)
  | Clear of color  (** Clear graphics. *)
  | Adapt of
      (view option * plot_object option) ref * (view option -> plot_object)
      (** Any object that needs to adapt itself to the current View. *)
  | User of (view -> plot_device -> unit)
      (** Execute any user-defined program. *)
  | Sheet of plot_object list  (** Group plot objects. *)

(* float entre 0 et 1 *)
type colormap = color -> float -> color
type coord = X | Y

type user_device =
  | X11_d
  | GL_d
  | FIG_d
  | XFIG_d
  | EPS_d
  | PDF_d
  | GV_d
  | BMP_d
  | PNG_d
  | IMG_d

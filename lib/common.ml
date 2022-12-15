(* Common definitions of types for Oplot. San Vu Ngoc. *)

open Points

(* sorties graphiques de base implémentées par le programme *)
type plot_device = X11 | GL | FIG
type mrange = { mutable min : float; mutable max : float }
type points = point list
type view = point * point
(* View (p1,p2) représente les coins bg et hd de la portion a afficher *)
(* à remplacer par des "ranges" optionnels ? *)

type view3 = point3 * point3
(* coordonnées des diagonales du cube *)
(* gérer aussi la couleur comme les view ou attachée à chaque objet ? *)

type color = { r : float; g : float; b : float }
type text_image = ([ `luminance_alpha ], [ `ubyte ]) GlPix.t * int * int
(* (image + dimensions de la partie intéressante) *)

type align = CENTER | LEFT | RIGHT
type text_flag = Normal | Latex

type text = {
  mutable pos : point;
  text : string;
  size : int;
  align : align;
  flag : text_flag;
  mutable pix : text_image option;
}
(* le champ optionnel modifiable "pix" permet de stocker l'image une fois
   calculée.
*)

type axis = {
  center : point;
  mutable view : view option;
  mutable ticks : (points * text list) option;
  mutable window_size : int * int;
}

type imatrix = int array array
type fmatrix = float array array
type grid = fmatrix * view3 * bool (* bool=wire *)
type surf3d = fmatrix * fmatrix * fmatrix * view3 * bool
(* une feuille (sheet)décrit une suite d'ordres graphiques *)
(* un objet plot décrit un arbre de feuilles (sheet et non leaf, héhé) *)

(* 3D: utiliser GlArray.vertex dans la display list pour optimiser l'affichage ? *)

type curve3d = point3 list * view3
type gllist = GlList.t option ref

type motion3d =
  | Translate of point3
  | Rotate of Geom.quaternion
  | Zoom of ((float -> float) * float option)

type move3d = { move : motion3d; time : mrange; mutable init_time : int option }
type image = ([ `rgba ], [ `ubyte ]) GlPix.t * int * int

type latex = {
  lpos : point;
  ltext : string;
  lsize : int;
  lalign : align;
  mutable lpix : image option;
}
(* comme text sauf le format de l'image plus gros: rgba (les couleurs sont
   données par l'en-tête latex (à changer ?)*)

type plot_object =
  | Points of points
  | Lines of points list
  | Poly of points
  | View of view option
  | Axis of axis
  | Color of color
  | Text of text
  | Matrix of imatrix (* utiliser une texture à la place *)
  | Grid of grid * gllist
  | Surf3d of surf3d * gllist
  | Curve3d of curve3d * gllist
  | Move3d of move3d
  | Pause of int
  | Freeze of int
  | Clear of color
  | Adapt of
      (view option * plot_object option) ref * (view option -> plot_object)
    (* f doit être une fonction qui donne quelque chose même si view option = None. Ca peut servir à initialiser la view avec maxview *)
  | User of (view -> plot_device -> unit)
  (* =une fonction utilisateur *)
  (* | Anim of (float -> plot_object)*)
  | Sheet of plot_object list

(* faire un type plot adaptatif qui recalcule les points en fonction de la
   résolution de la fenêtre, et d'adapte aux grandes dérivées. *)
(* inclure les transitions dans le mécanisme de pause ? cf zoom *)

(* float entre 0 et 1 *)
type colormap = color -> float -> color
type coord = X | Y

(* sorties graphiques utilisables par l'utilisateur *)
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

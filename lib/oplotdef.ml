(*  on met ici ce qui est indépendant de la boucle de rendu *)
(* essentiellement les types et les constructeurs *)

open Points

exception Not_implemented of string
exception Empty_list
exception View_expected
exception Fig_Too_Many_Colors
exception Division_by_zero
exception Should_Not_Happen

(* sorties graphiques implémentées par le programme *)
type plot_device = X11 | GL | FIG

let window_width = ref 640

(* taille sans les marges en pixels. Sera multiplié par gl_scale. *)
let window_height = ref 480
let fwindow_width = ref (float !window_width)
let fwindow_height = ref (float !window_height)

(* pour le moment les marges ne sont actives que sur la sortie opengl *)
let left_margin = ref 10 (* en pixels *)
let right_margin = ref 10
let top_margin = ref 10
let bottom_margin = ref 10

(* nbre max de points déterminé automatiquement dans une courbe paramétrée *)
let maxpoints = 3000

type mrange = { mutable min : float; mutable max : float }
type points = point list
type view = point * point
(* View (p1,p2) représente les coins bg et hd de la portion a afficher *)
(* à remplacer par des "ranges" optionnels ? *)

type view3 = point3 * point3
(* coordonnées des diagonales du cube *)

let current_view3d : view3 option ref = ref None
(* attention pas réentrant...*)

let initialize_view3 v =
  match !current_view3d with
  | None ->
      current_view3d := Some v;
      v
  | Some v3 -> v3

let reset_view3 () = current_view3d := None

let get_view3 () =
  match !current_view3d with
  | Some v3 -> v3
  | None -> failwith "There should be a view2 here !"
(* inutile ? *)

(* gérer aussi la couleur comme les view ou attachée à chaque objet ? *)
type color = { r : float; g : float; b : float }

let black = { r = 0.; g = 0.; b = 0. }
let white = { r = 1.; g = 1.; b = 1. }
let red = { r = 1.; g = 0.; b = 0. }
let green = { r = 0.; g = 1.; b = 0. }
let blue = { r = 0.; g = 0.; b = 1. }
let yellow = { r = 1.; g = 1.; b = 0. }
let cyan = { r = 0.; g = 1.; b = 1. }
let magenta = { r = 1.; g = 0.; b = 1. }

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

let move_text text pos = text.pos <- pos

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

let gllist_empty () : gllist = ref None

type motion3d =
  | Translate of point3
  | Rotate of Geom.quaternion
  | Zoom of ((float -> float) * float option)

type move3d = { move : motion3d; time : mrange; mutable init_time : int option }

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

(* animation ? *)
let rec has_anim sh =
  match sh with
  | Sheet [] -> false
  | User _ -> true
  | Move3d _ -> true
  | Sheet (po :: ssh) -> has_anim po || has_anim (Sheet ssh)
  | _ -> false

(* pauses ? *)
let rec has_pause sh =
  match sh with
  | Sheet [] -> false
  | Pause _ | Freeze _ -> true
  | Sheet (po :: ssh) -> has_pause po || has_pause (Sheet ssh)
  | _ -> false

(******************************************************)
(************** partie création des objets ************)
(******************************************************)

let point (x0, y0) = { Point2.x = x0; y = y0 }
(* c'est semble-t'il la facon la plus pratique pour l'utilisateur: on peut
   écrire point (-1.,-1.)  alors qu'avec point x0 x0 = ETC.  on devrait mettre
   plus de parenthèses: point (-1.) (-1.) . Pour la programmation a priori je ne
   l'utilise pas *)

let point3 (x0, y0, z0) = { Point3.x = x0; y = y0; z = z0 }

(*** interactif: crée un objet Text (string, size, position) ***)
let text s ?(size = 12) ?(align = CENTER) x0 y0 =
  Text
    { pos = point (x0, y0); text = s; size; align; flag = Normal; pix = None }

(*** le même avec le flag Latex ***)
let latex s ?(size = 18) ?(align = CENTER) x0 y0 =
  Text { pos = point (x0, y0); text = s; size; align; flag = Latex; pix = None }

(*** interactif: crée un objet View ***)
let view x0 y0 x1 y1 = View (Some (point (x0, y0), point (x1, y1)))
let get_view view_ref = User (fun v _ -> view_ref := v)

let view2of3 (p1, p2) =
  Some (point (p1.Point3.x, p1.Point3.y), point (p2.Point3.x, p2.Point3.y))

(* Retourne la liste des points à tracer, avec un pas optionnel (par défaut le
   pas est ajusté pour !window_width points). Le dernier argument () est
   nécessaire pour permettre à "pas" d'être optionnel.  Un élément de cette
   liste est du type (x0,y0): *)

(* rem: la liste commence par l'abscisse la plus grande x1 *)
let point_list f x0 x1 ?(pas = (x1 -. x0) /. !fwindow_width) () =
  let rec loop l x =
    if x > x1 then l (* modifier si on veut x1 < x0 !! *)
    else loop ({ Point2.x; y = f x } :: l) (x +. pas)
  in
  loop [] x0

(* attribue un type (Points ou Lines) à une liste de points *)
let points_of_list pl = Points pl
let lines_of_list pl = Lines [ pl ]
let poly_of_list pl = Poly pl (*utile ?*)

(*** interactif: crée un objet Points à partir d'une fonction ***)
(* on pourrait faire:

   let point_plot_f f x0 x1 ?(pas=(x1 -. x0) /. !fwindow_width) () = ETC...

   mais ca oblige l'utilisateur a mettre le () à la fin... un peu penible, quoi
*)
let point_plot_f f ?pas x0 x1 =
  let p =
    match pas with None -> (x1 -. x0) /. !fwindow_width | Some pp -> pp
  in
  points_of_list (point_list f x0 x1 ~pas:p ())

(*** interactif: crée un objet Lines à partir d'une fonction ***)
let line_plot_f f ?pas x0 x1 =
  let p =
    match pas with None -> (x1 -. x0) /. !fwindow_width | Some pp -> pp
  in
  lines_of_list (point_list f x0 x1 ~pas:p ())

let plot = line_plot_f

(* est-ce que p est à l'intérieur de la view ? *)
let inside { Point2.x; y } ({ Point2.x = x1; y = y1 }, { Point2.x = x2; y = y2 })
    =
  (x -. x1) *. (x2 -. x1) >= 0.
  && (x -. x2) *. (x1 -. x2) >= 0.
  && (y -. y1) *. (y2 -. y1) >= 0.
  && (y -. y2) *. (y1 -. y2) >= 0.

(* point d'intersection du segment [q1,q2] avec la boite de la view p1,p2, dans
   le cas unique *)
let intersect q1 q2 (p1, p2) =
  (* debug : *)
  (* let print p = Printf.printf "x=%f; y=%f\n" p.x p.y in
     print q1; print q2; print_newline (); *)
  Geom.inter_box (q1, q2) (p1, p2)

(* let crop o v = *)
(*   let list = match o with  *)
(*     | Lines [pl] -> pl *)
(*     | _ -> failwith "Lines expected" in *)
(*   let rec loop l1 prev acc lacc =  *)
(*     match l1 with *)
(*       | [] -> if acc = [] then lacc else acc::lacc *)
(*       | p::rest -> if inside p v then loop rest (p::acc) lacc *)
(*  else (match acc with *)
(*   | [] -> loop rest [] lacc *)
(*   | l -> loop rest [] (acc::lacc)) in *)
(*     Lines (loop list [] []) *)

(* sépare une liste de points en plusieurs sous-listes pour ne pas tracer ce qui
   sort de la view. Ici on prend en compte les segments à cheval sur les
   frontière. Rem: la liste se retrouve "à l'endroit"... *)
let lines_crop lines v =
  let rec loop l1 prev acc lacc =
    match l1 with
    | [] -> if acc = [] then lacc else acc :: lacc
    | p :: rest -> begin
        match (inside p v, acc) with
        | true, [] (* on rentre *) ->
            let new_acc =
              match prev with
              | None -> [ p ]
              | Some p' ->
                  let p'' = try intersect p p' v with _ -> p' in
                  [ p; p'' ]
            in
            loop rest (Some p) new_acc lacc
        | false, [] (* on reste dehors *) -> loop rest (Some p) [] lacc
        | true, _ (* on reste dedans *) -> loop rest (Some p) (p :: acc) lacc
        | false, _ (* on sort *) -> begin
            match prev with
            | Some p' ->
                let p'' = try intersect p p' v with _ -> p' in
                loop rest (Some p) [] ((p'' :: acc) :: lacc)
            | None -> failwith "point expected in line_crop!"
          end
      end
  in
  loop lines None [] []

(* crée un objet Adapt pour un graphe de fonction *)
let adapt_plot f ?pas x0 x1 =
  let list = point_list f ?pas x0 x1 () in
  (* adapter aussi le pas ? *)
  Adapt
    ( ref (None, None),
      fun view ->
        match view with
        | None -> lines_of_list list
        | Some v -> Lines (lines_crop list v) )

(* ici t0 peut être plus grand que t1 si on veut ! *)
let parametric_list fx fy t0 t1 pas =
  let rec loop list t =
    if abs_float (t +. pas -. t0) > abs_float (t1 -. t0) then
      { Point2.x = fx t1; y = fy t1 } :: list
    else loop ({ Point2.x = fx t; y = fy t } :: list) (t +. pas)
  in
  loop [] t0

let parametric_plot fx fy ?pas ?(adapt = true) t0 t1 =
  let p =
    match pas with
    | None ->
        (t1 -. t0)
        /. min
             (Geom.curve_length fx fy t0 t1 *. !fwindow_width)
             (float maxpoints)
    | Some pp -> pp
  in
  let list = parametric_list fx fy t0 t1 p in
  if adapt then
    Adapt
      ( ref (None, None),
        fun view ->
          match view with
          | None -> lines_of_list list
          | Some v -> Lines (lines_crop list v) )
  else lines_of_list list

(* fonction en escalier à partir d'une liste (liste à l'envers *)
let list_plot l x0 x1 =
  let n = List.length l in
  let dx = (x1 -. x0) /. float n in
  let rec loop list plist i =
    match list with
    | [] -> plist
    | y :: ll ->
        loop ll
          ({ Point2.x = x0 +. (float (i + 1) *. dx); y }
          :: { Point2.x = x0 +. (float i *. dx); y }
          :: plist)
          (i + 1)
  in
  Lines [ loop l [] 0 ]

(* auxilliaires pour dot_plot *)
let just_a_dot x y = points_of_list [ point (x, y) ]

let diamond ?(size = 0.01) x y =
  let epsilon = size /. 2. in
  Poly
    [
      point (x -. epsilon, y);
      point (x, y +. epsilon);
      point (x +. epsilon, y);
      point (x, y -. epsilon);
      point (x -. epsilon, y);
    ]

let box x0 y0 x1 y1 =
  Poly
    [
      { x = x0; y = y0 };
      { x = x1; y = y0 };
      { x = x1; y = y1 };
      { x = x0; y = y1 };
    ]

let find_view l =
  let rec loop xmax xmin ymax ymin l =
    match l with
    | (x, y) :: ll ->
        loop (max xmax x) (min xmin x) (max ymax y) (min ymin y) ll
    | [] -> view xmin ymin xmax ymax
  in
  match l with
  | [] -> failwith "List should not be empty"
  | (x, y) :: ll -> loop x y x y ll

(* ensemble de points (x,y), avec la fonction qui trace un point en argument *)
(* attention retourne une liste de plot_objects *)
let dot_plot ?(dot = just_a_dot) ?view l =
  let rec loop list sh =
    match list with (x, y) :: ll -> loop ll (dot x y :: sh) | [] -> sh
  in
  let v = match view with Some vv -> vv | None -> find_view l in
  v :: loop l []

let label_dot_plot ?(dot = just_a_dot) ?(offset = 0.005) ?view l =
  let rec loop list sh =
    match list with
    | (x, y, label) :: ll ->
        let text = text label (x +. (2. *. offset)) y ~align:LEFT in
        loop ll (dot x y :: text :: sh)
    | [] -> sh
  in
  let v =
    match view with
    | Some vv -> vv
    | None ->
        let coord_list = List.rev_map (fun (x, y, _) -> (x, y)) l in
        find_view coord_list
  in
  v :: loop l []

let matrix_plot_f f ?(width = !window_width / 4) ?(height = !window_height / 4)
    x0 y0 x1 y1 =
  let fm = Array.make_matrix height width 0. in
  let dx = (x1 -. x0) /. float width and dy = (y1 -. y0) /. float height in
  let zmin = ref (f (x0 +. (dx /. 2.)) (y0 +. (dy /. 2.))) in
  let zmax = ref !zmin in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let z =
        f (x0 +. ((float j +. 0.5) *. dx)) (y0 +. ((float i +. 0.5) *. dy))
      in
      fm.(i).(j) <- z;
      if z < !zmin then zmin := z;
      if z > !zmax then zmax := z
    done
  done;
  let dz = if !zmax = !zmin then 1. else !zmax -. !zmin in
  let m = Array.make_matrix height width 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      m.(i).(j) <- int_of_float (255. *. (fm.(i).(j) -. !zmin) /. dz)
    done
  done;
  Matrix m

(* variante... *)
let grid_plot f ?(wire = true) ?(width = !window_width / 20)
    ?(height = !window_height / 20) x0 y0 x1 y1 =
  let fm = Array.make_matrix (height + 1) (width + 1) 0. in
  let dx = (x1 -. x0) /. float width and dy = (y1 -. y0) /. float height in
  let zmin = ref (f x0 y0) in
  let zmax = ref !zmin in
  for i = 0 to height do
    for j = 0 to width do
      let z = f (x0 +. (float j *. dx)) (y0 +. (float i *. dy)) in
      fm.(i).(j) <- z;
      if z < !zmin then zmin := z;
      if z > !zmax then zmax := z
    done
  done;
  Grid ((fm, (point3 (x0, y0, !zmin), point3 (x1, y1, !zmax)), wire), ref None)

let surf3d_plot fx fy fz ?(width = 40) ?(height = 40) ?(wire = true) u0 v0 u1 v1
    =
  (* on rajoute des bords pour calculer les normales *)
  (* les données "normales" sont les indices 1 à width+1, 1 à height+1 *)
  let mx = Array.make_matrix (height + 3) (width + 3) 0.
  and my = Array.make_matrix (height + 3) (width + 3) 0.
  and mz = Array.make_matrix (height + 3) (width + 3) 0. in
  let du = (u1 -. u0) /. float width and dv = (v1 -. v0) /. float height in
  let zmin = ref (fz u0 v0)
  and xmin = ref (fx u0 v0)
  and ymin = ref (fy u0 v0) in
  let zmax = ref !zmin and xmax = ref !xmin and ymax = ref !ymin in
  for i = -1 to height + 1 do
    let mxi = mx.(i + 1) and myi = my.(i + 1) and mzi = mz.(i + 1) in
    for j = -1 to width + 1 do
      let x = fx (u0 +. (float j *. du)) (v0 +. (float i *. dv))
      and y = fy (u0 +. (float j *. du)) (v0 +. (float i *. dv))
      and z = fz (u0 +. (float j *. du)) (v0 +. (float i *. dv)) in
      mxi.(j + 1) <- x;
      myi.(j + 1) <- y;
      mzi.(j + 1) <- z;
      if z < !zmin then zmin := z;
      if z > !zmax then zmax := z;
      if x < !xmin then xmin := x;
      if x > !xmax then xmax := x;
      if y < !ymin then ymin := y;
      if y > !ymax then ymax := y
    done
  done;
  Surf3d
    ( ( mx,
        my,
        mz,
        (point3 (!xmin, !ymin, !zmin), point3 (!xmax, !ymax, !zmax)),
        wire ),
      ref None )

(*** interactif: crée un objet Axis ***)
let axis x0 y0 =
  Axis
    {
      center = point (x0, y0);
      view = None;
      ticks = None;
      window_size = (!window_width, !window_height);
    }

(*** interactif: crée un objet Color (pas très utile, on peut le faire
     à la main ! ***)
let color r g b = Color { r; g; b }
let pause t = Pause t
let freeze t = Freeze t

let translate v t =
  Move3d { move = Translate v; time = { min = 0.; max = t }; init_time = None }

let rotate x y z th t =
  let quat = Geom.q_rotation x y z th in
  Move3d { move = Rotate quat; time = { min = 0.; max = t }; init_time = None }

let zoom z z0 t0 t1 =
  Move3d
    { move = Zoom (z, z0); time = { min = t0; max = t1 }; init_time = None }

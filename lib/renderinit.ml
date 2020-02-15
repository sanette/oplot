let () = Debug.print "* Loading renderinit"


(* initialisations liées aux sorties graphiques et à la boucle de rendu *)

open Oplotdef

let window_open = ref false
let interrupt_request = ref false
let fullscreen = ref false
let multisampling = ref true
let gl_scale = ref 2.

(* sorties graphiques utilisables par l'utilisateur *)
type user_device = X11_d | GL_d | FIG_d | XFIG_d | EPS_d | PDF_d
     | GV_d | BMP_d | PNG_d | IMG_d 
(* user aliases *)
let x11 = X11_d 
and gl = GL_d 
and fig = FIG_d
and xfig = XFIG_d
and eps = EPS_d
and pdf = PDF_d
and gv = GV_d
and bmp = BMP_d
and png = PNG_d
and img = IMG_d

let default_device = ref GL
let default_user_device = ref GL_d

(*** interactif: choix de la sortie graphique ***)
let set_device devname =
  default_user_device := devname;
  default_device := List.assoc devname
      [ (X11_d , X11) ; 
        (GL_d , GL) ; 
        (FIG_d , FIG) ; 
        (XFIG_d , FIG) ; 
        (EPS_d , FIG) ; 
        (GV_d , FIG) ;
        (BMP_d , GL) ;
        (PNG_d , GL) ;
        (IMG_d , GL) ]

(* gestionnaires opengl autorisés *)
type gl_handler = SDL | GLUT | GTK
let default_gl = ref SDL
let set_default_gl x = default_gl := x

(* w et h comprennent les marges *)
let resize_window w h = 
  let (w,h) = (max (w - !left_margin - !right_margin) 5,
               max (h - !top_margin - !bottom_margin) 5) in
  window_width:=w; fwindow_width:= float w;
  window_height:=h; fwindow_height:= float h

(* pour tourner les objets 3D indépendemment des objets 2D: *)
(* position en représentation quaternion *)
let default_position3d = (1., 0., 0., 0.)
let position3d = ref default_position3d

let default_zoom3d = 1.;; (* coeff multiplicatif *)
let zoom3d = ref default_zoom3d

let mouse_x = ref 0
let mouse_y = ref 0

let frame_length = ref (1000 / 29)  (* 29 FPS *)
let get_frame_length () = !frame_length
let set_frame_length x = frame_length := x
    
let start_time = ref 0;; 
(* utilisé pour réguler l'affichage dans les boucles principales *)

let initial_time = ref 0;; 

(* pour geler le temps pendant les freezes *)
let time_delay = ref 0

let counter = ref 0;; 
(* numero de l'objet courant. On utilise deux numéros pour un objet
   axis. Ca permet de séparer le texte des axes *)

let get_depth dep : int = match dep with
  | None -> max 0 (900 - !counter)
  | Some d -> d


let wait = Pause 0;; (* s'utilise pour attendre la frappe d'une touche *)
(* revoir le fonctionnement des pauses ! *)
let resume_pause = ref false;; (* used to escape from paused *)
let pause_time : int option ref = ref None
let do_not_draw = ref false;; (* used to stop drawing at some point *)
(* use an exception instead ? *)

let pause_pass = ref 0;; (* how many objects to bypass before pausing *)
(* redondantes variables ? *)

let pause_init () = 
  pause_time := None;
  resume_pause := false;
  do_not_draw := false;
  pause_pass := 0

let default_color = blue
let default_bg_color = ref white
let clear = Clear !default_bg_color;; (*inutilisé ?*)

(* la plupart du temps le système graphique se souvient de la dernière couleur
   utilisée, mais parfois on a besoin de la modifier momentanément, et donc de
   s'en souvenir *)
let current_color = ref blue

(* fournit le triplet correspondant *)
let float_of_color c = ( c.r , c.g, c.b )

(* fournit un triplet d'entiers rvb de 0 à 255 *)
let int_of_color c = (int_of_float ( c.r *. 255. ) ),
  (int_of_float ( c.g *. 255. ) ),
  (int_of_float ( c.b *. 255. ) )

let rgb_of_color c = let (r,g,b) = int_of_color c in Graphics.rgb r g b


(* faire un tableau array pour les 5?? couleurs *)
let fig_colors = Array.make 543 0
let init_fig_colors () = 
  Array.fill fig_colors 0 543 0;
  let rec loop i l = match l with
      [] -> ()
    | c::ll -> fig_colors.(i) <- c; loop (i+1) ll in
    loop 0 
      [  0x000000  ; (* black *)
       0x0000ff  ; (* blue *)
       0x00ff00  ; (* green *)
       0x00ffff  ; (* cyan *)
       0xff0000  ; (* red *)
       0xff00ff  ; (* magenta *)
       0xffff00  ; (* yellow *)
       0xffffff  ; (* white *)
       0x000090  ;
       0x0000b0  ;
       0x0000d0  ;
       0x87ceff  ;
       0x009000  ;
       0x00b000  ;
       0x00d000  ;
       0x009090  ;
       0x00b0b0  ;
       0x00d0d0  ;
       0x900000  ;
       0xb00000  ;
       0xd00000  ;
       0x900090  ;
       0xb000b0  ;
       0xd000d0  ;
       0x803000  ;
       0xa04000  ;
       0xc06000  ;
       0xff8080  ;
       0xffa0a0  ;
       0xffc0c0  ;
       0xffe0e0  ;
       0xffd700  ]

(* trouve l'indice de la première occurence d'un element dans un tableau . Sinon
   retourne -1 *)
let mem array (elem : int) = let l = Array.length array in
let rec loop i = if i = l then -1 
  else if array.(i) = elem then i else loop (i+1) in
  loop 0

let fig_of_color c = mem fig_colors (rgb_of_color c)

let pr = Printf.printf
           
(* try to obtain the monitor's DPI on linux systems. Does not work with multiple
   monitors *)
let get_dpi () =
  try
    let proc = Unix.open_process_in
        "xdpyinfo | grep resolution | awk '{print $2}'" in
    let res = input_line proc in
    match Unix.close_process_in proc with
    | Unix.WEXITED 0 ->
      let i = String.index res 'x' in
      let dpi =int_of_string (String.sub res 0 i) in
      pr "Detected DPI=%u\n" dpi;
      Some dpi
    | _ -> pr "Cannot get monitor's DPI from [%s]. Using 110." res;
      None
  with
  | _ -> pr "Cannot get monitor's DPI from xdpyinfo. Using 110.";
    None

let init () =
  let default_dpi = 110 in
  let dpi = match get_dpi () with
    | Some x -> x
    | None -> default_dpi in
  let s = if dpi <= 110 then 1. else (float dpi /. 110.) in
  pr "Using SCALE=%f\n" s;
  gl_scale := s;
  window_open := false;
  fullscreen := false;
  multisampling := true;
  interrupt_request := false;
  default_device := GL;
  default_user_device := GL_d;
  default_gl := SDL;
  position3d := default_position3d;
  zoom3d := default_zoom3d;
  mouse_x := 0;
  mouse_y := 0;
  frame_length := (1000 / 29);
  start_time := 0; 
  initial_time := 0; 
  time_delay := 0;
  counter := 0; 
  default_bg_color := white;
  current_color := blue;
  pause_init ();
  reset_view3 ();; (* pas néc ? cf disp *)

let get_mouse_x () = !mouse_x
let get_mouse_y () = !mouse_y
let set_mouse_x x = mouse_x := x
let set_mouse_y y = mouse_y := y
    
let get_window_height () = !Oplotdef.window_height

Debug.print "* Loading oplotmain";;

(*** à faire: ca serait bien d'utiliser la bibliothèque gl2ps
       pour convertir directement en ps
       http://www.geuz.org/gl2ps/
       mais il faut écrire l'interface ocaml!
  ***)

(* utiliser pdflatex ? (cf le module ~/.inkscape/extensions/textext.py)
*)

module Make (Graphics : Make_graphics.GRAPHICS) = struct
  open Tsdl
  module Gl = Gl_legacy
  module Gl3 = Tgl3.Gl
  open Common
  open Points
  open Point2
  open Oplotdef
  open Sysinit
  open Renderinit

  let go = Debug.go
  let do_option o f = match o with Some x -> f x | None -> ()
  let default o v = match o with Some x -> x | None -> v
  let force_refresh = ref false
  let xfig_scale = 45.
  (* Un point xfig vaut 1/80 inch. Mais attention "When exporting to EPS,
   PostScript or any bitmap format (e.g. GIF), the line thickness is reduced to
   1/160 inch (0.159mm) to "lighten" the look." *)

  (* (bx0,by0, bx1,by1) : coordonnées logiques (et non physiques) de la fenêtre *)
  let bounding_box dev =
    match dev with
    | GRAPHICS -> (1., 1., !fwindow_width, !fwindow_height)
    (* ici logique=physique, à un décalage de 1 près *)
    | GL -> (0., 0., 1., 1.)
    (* sous opengl le redimensionnement de la fenêtre est pris en compte
     séparément par le viewport *)
    | FIG ->
        ( 0.,
          xfig_scale *. 157.2 *. !fwindow_height /. 600. /. !gl_scale,
          xfig_scale *. 210. *. !fwindow_width /. 800. /. !gl_scale,
          0. )
  (* about a5 for 800x600 *)

  (*  ( 0. , 45. *. 200. , 45. *. 266.7 , 0. );; *)
  (* in xfig, origin is upper left *)
  (* corriger le ratio ! *)

  (*** interactif: renvoie le nombre de millisecondes écoulées depuis la dernière
     initialisation de sdl. Donc réinitialisé au lancement de sdl_init ***)
  let time () = Int32.to_int (Sdl.get_ticks ()) - !time_delay
  let elapsed () = time () - !initial_time
  let reset_time ?(t0 = 0) () = initial_time := time () - t0

  (**********************************************************)
  (**************** inits opengl ****************************)
  (**********************************************************)

  let scale x = x *. !gl_scale
  let round x = int_of_float (x +. 0.5)
  let iscale i = round (float i *. !gl_scale)
  let dpi_scale = ref 1.

  let scale_window =
    let scaled = ref false in
    fun () ->
      if not !scaled then begin
        window_width := iscale !window_width;
        window_height := iscale !window_height;
        fwindow_width := float !window_width;
        fwindow_height := float !window_height;
        scaled := true
      end
      else Debug.print "Already scaled."

  (* initialisation d'une fenêtre opengl par SDL *)
  let win = ref None
  let glcontext = ref None

  let window_os_size () =
    ( round (float (!window_width + !left_margin + !right_margin) /. !dpi_scale),
      round (float (!window_height + !top_margin + !bottom_margin) /. !dpi_scale)
    )

  (* The following is a workaround for the weird bug on Mac OS 13.0.1 with cocoa
   video driver which prevents windows to close in an interactive toplevel
   session. For some reason the window will close if we initialise a subsystem
   that was not already initialised, here joystick. *)
  let sdl_destroy_window win =
    Sdl.destroy_window win;
    if !Sys.interactive && Sdl.get_current_video_driver () = Some "cocoa" then begin
      Debug.print "cocoa workaround";
      Sdl.delay 100l;
      (go @@ Sdl.(init Init.joystick));
      Sdl.(quit_sub_system Init.joystick)
    end

  let sdl_get_dpi_scale () =
    match
      Sdl.create_window "Oplot - SDL Window" ~w:64 ~h:64
        Sdl.Window.(opengl + allow_highdpi + hidden)
    with
    | Error (`Msg e) ->
        Debug.print "Cannot open test window: %s" e;
        1.
    | Ok win ->
        let w, h = Sdl.get_window_size win in
        (* size in OS pixels *)
        let rw, rh = Sdl.gl_get_drawable_size win in
        sdl_destroy_window win;
        (* size in hardware pixels *)
        if (rw, rh) <> (w, h) then begin
          let dpi_xscale = float rw /. float w in
          let dpi_yscale = float rh /. float h in
          Debug.print "This display imposes a hard scaling of (%f,%f)."
            dpi_xscale dpi_yscale;
          min dpi_xscale dpi_yscale
        end
        else 1.

  let gl_clear_color c = Gl.clear_color c.r c.g c.b 1.0
  let gl_draw_color c = Gl.color3f c.r c.g c.b

  let sdl_init ~show () =
    let crucial () =
      if Sdl.Init.test (Sdl.was_init None) Sdl.Init.video then
        Debug.print "Using existing SDL context."
      else begin
        Debug.print "Raising up SDL...";
        Sdl.init Sdl.Init.(timer + video + events) |> go;
        win := None;
        at_exit (fun () ->
            Debug.print "Quitting SDL";
            Sdl.quit ());
        (* if !multisampling then Sdlgl.set_attr [ Sdlgl.MULTISAMPLEBUFFERS 1; *)
        (*             Sdlgl.MULTISAMPLESAMPLES 4]; *)
        match Sdl.get_display_dpi 0 with
        | Ok (x, _, _) ->
            Debug.print "DPI detected by SDL: %f" x;
            gl_scale := max 1. (x /. 110.)
        | Error (`Msg m) -> Debug.print "Cannot get DPI from SDL: %s" m
      end;
      if !win = None then begin
        scale_window ();
        dpi_scale := sdl_get_dpi_scale ();
        let w, h = window_os_size () in
        match
          Sdl.create_window "Oplot - SDL Window" ~w ~h
            Sdl.Window.(opengl + resizable + allow_highdpi)
        with
        | Error (`Msg e) ->
            Sdl.log "Create window error: %s" e;
            raise (Debug.Sdl_error e)
        | Ok wn ->
            win := Some wn;
            if !dpi_scale <> 1. then
              let rw, rh = Sdl.gl_get_drawable_size wn in
              (* size in hardware pixels *)
              resize_window rw rh
      end;
      if not show then do_option !win Sdl.hide_window;
      Sdlttf.init () |> go;
      glcontext :=
        match !win with
        | Some w -> Some (go @@ Sdl.gl_create_context w)
        | None -> None
    in
    (try crucial ()
     with Debug.Sdl_error _ -> (
       Debug.print "Hum... Trying again";
       Sdl.quit ();
       multisampling := false;
       Unix.sleep 1;
       try crucial ()
       with Debug.Sdl_error e ->
         Debug.print "Sdl error %s" e;
         do_option !glcontext Sdl.gl_delete_context;
         Sdl.quit ();
         exit 1));

    (* GlClear.color (float_of_color !default_bg_color);
     GlClear.clear [ `depth ];
     Sdlgl.swap_buffers (); *)
    (* List.iter (fun e -> Sdl.set_event_state e Sdl.enable) *)
    (*   Sdl.Event.[key_down; mouse_button_down; window_event; sys_wm_event]; *)
    window_open := true;
    Debug.print "sdl_init OK"

  let gtk_init () = ()
  (* pour le moment, c'est fait avant par gtk *)

  (* transforme un vecteur de pixels en vecteur opengl *)
  let draw_of_pixel (dx, dy) (bx0, by0, bx1, by1) =
    ( float dx *. (bx1 -. bx0) /. !fwindow_width,
      float dy *. (by1 -. by0) /. !fwindow_height )

  (* doit être appelé après resize_window pour mettre les variables globales à
   jour *)
  let gl_resize () =
    Gl.viewport 0 0
      (!window_width + !left_margin + !right_margin)
      (!window_height + !top_margin + !bottom_margin);
    Gl.matrix_mode Gl.projection;
    Gl.load_identity ();
    let bb = bounding_box GL in
    let dxl, dyb = draw_of_pixel (!left_margin, !bottom_margin) bb
    and dxr, dyt = draw_of_pixel (!right_margin, !top_margin) bb in
    Gl.ortho (-.dxl) (1. +. dxr) (-.dyb) (1. +. dyt) (-2.) 2.;
    Gl.matrix_mode Gl.modelview

  let gl_rotated2d angle =
    let bb = bounding_box GL in
    let dxl, dyb = draw_of_pixel (!left_margin, !bottom_margin) bb
    and dxr, dyt = draw_of_pixel (!right_margin, !top_margin) bb in
    let x = (1. +. dxl +. dxr) /. 2. in
    let y = (1. +. dyb +. dyt) /. 2. in
    Gl.translatef x y 0.;
    Gl.rotated angle 0. 0. 1.;
    Gl.translatef (-.x) (-.y) 0.

  let gl_init ?(show = true) () =
    (match !default_gl with
    | GLUT -> Iglut.init ()
    | SDL -> sdl_init ~show ()
    | GTK -> gtk_init ());
    Debug.print "GL inits...";
    gl_clear_color !default_bg_color;
    Gl3.draw_buffer Gl3.back;
    Gl3.read_buffer Gl3.back;
    Gl3.clear Gl3.depth;
    Gl3.pixel_storei Gl3.unpack_alignment 1;

    (*  if !multisampling then Gl.enable `multisample else *)

    (* Gl.enable `line_smooth;*)
    Gl3.disable Gl3.polygon_smooth;
    (* sinon on voit les triangulations *)
    Gl3.hint Gl3.line_smooth Gl3.fastest;
    Gl3.enable Gl3.blend;

    (* Gl.enable `point_smooth; *)
    (* Peut causer d'énormes ralentissements sur certaines implémentations !! *)
    Gl3.blend_func Gl3.src_alpha Gl3.one_minus_src_alpha;
    Gl3.line_width !gl_scale;
    Gl3.point_size !gl_scale;
    Gl3.enable Gl3.polygon_offset_fill;
    Gl3.polygon_offset 1. 1.;
    gl_resize ();

    (*  GlMat.frustum ~x:(-1. , 1.) ~y:(-. 600. /. 800. , 600. /. 800.) ~z:(2. , 6.); *)

    (* l'aspect est réglé sur la taille par défaut 800x600 *)
    (* GluMat.perspective ~fovy:60. ~aspect:(800./.600.) ~z:(100.,-.150.); *)
    (* ??? *)
    (* en cas de frustum:  GlMat.translate ~z:(-4.) (); *)
    Gl.push_matrix ();
    (* on sauve la position initiale *)
    Gl.flush ();
    Debug.print "gl_init OK"

  let toggle_fullscreen () =
    do_option !win (fun w ->
        match
          Sdl.set_window_fullscreen w
            (if not !fullscreen then Sdl.Window.fullscreen_desktop
             else Sdl.Window.windowed)
        with
        | Error (`Msg e) -> Sdl.log "Fullscreen error: %s" e
        | Ok () -> fullscreen := not !fullscreen)

  let close ?(dev = !default_device) () =
    match dev with
    | GRAPHICS -> Graphics.close_graph ()
    | GL -> (
        window_open := false;
        if !fullscreen then toggle_fullscreen ();
        let close () =
          do_option !glcontext Sdl.gl_delete_context;
          glcontext := None;
          do_option !win (fun w ->
              Debug.print "Destroying window";
              sdl_destroy_window w;
              Sdl.(flush_events Event.first_event Event.last_event));
          win := None
        in
        try close ()
        with Debug.Sdl_error e ->
          Debug.print "%s. Hum... trying again." e;
          close ())
    | FIG -> raise (Not_implemented "FIG close")
  (* flush buffer *)

  let quit ?(dev = !default_device) () =
    try
      close ~dev ();
      remove_tmp_dir ()
    with e ->
      Debug.print "Warning: quit wasn't clean.";
      if Debug.debug then raise e

  (* Set this to true to force re-creating all gllists *)
  let reset_gllist = ref false

  (**********************************************************)
  (************ fin inits opengl ****************************)
  (**********************************************************)

  (**********************************************************)
  (******** module feedback **(à transformer en vrai module *)
  (**********************************************************)

  (*** à améliorer en suivant l'exemple de Feedback.html ***)
  (*** http://www.opengl.org/resources/code/samples/mjktips/Feedback.html ***)
  (*** en particulier, urgent: trier les primitives en fonction de la profondeur
   !, et puis faire les dégradés... ***)

  let text_token = 1.
  let insert_token x = Gl.Feedback.pass_through x

  exception Feedback_Buffer_Overflow

  (* faire un  gl_init (); avant, et un close() ~dev:GL; après...  *)
  (* draw_proc est une procédure qui lance les tracés opengl. *)
  (* retourne le buffer de feedback et le nombre d'éléments. *)
  let feedback_render draw_proc =
    (* on augmente progressivement la taille du buffer en 2^i si nécessaire...
     (pas très fin, évidemment) *)
    let rec loop i =
      reset_gllist := true;
      (* gllists are not rendered in feedback mode. *)
      gl_init ();
      let r = Gl.Feedback.setup (1 lsl i) Gl.Feedback.GL_3D_COLOR in
      ignore (Gl.render_mode Gl.FEEDBACK);
      gl_draw_color default_color;
      Debug.print "draw in feedback mode...";
      draw_proc ();
      Debug.print "done";
      let num = Gl.render_mode Gl.RENDER in
      if num < 0 then
        if i < 31 then
          (* r can be freed. automatic? *)
          loop (i + 1)
        else raise Feedback_Buffer_Overflow
      else begin
        Debug.print "Created feedback buffer of size: %d for %d objects."
          (1 lsl i) num;
        (r, num)
      end
    in
    loop 16

  (*on commence avec une taille de 2^16=65536*)

  let get_vertex r pos =
    let open Bigarray.Array1 in
    let coord = sub r pos 3 in
    let colour = sub r (pos + 3) 4 in
    (coord, colour)

  let point_of_vertex (coord, _colour) =
    let open Bigarray.Array1 in
    { x = get coord 0; y = get coord 1 }

  let depth_of_vertex (coord, _colour) =
    let open Bigarray.Array1 in
    get coord 2

  let color_of_vertex (_coord, colour) =
    let open Bigarray.Array1 in
    let a = get colour 3 in
    if a <> 1. then begin
      prerr_endline (Printf.sprintf "Feedback Alpha:%f\n" a);
      flush stderr
    end;
    { r = get colour 0; g = get colour 1; b = get colour 2 }

  (* inutile *)
  let feedback_print r n =
    for i = 0 to n - 1 do
      Printf.printf "%d: %f\n" i (Bigarray.Array1.get r i)
    done

  let () = Debug.print "Initialise feedback constants"

  let feedback_view () =
    view (float !left_margin) (float !bottom_margin)
      (float (!window_width + !right_margin))
      (float (!window_height + !top_margin))

  let gl_vertex_size = 7

  (* fournit un plot object list [Color c ; Points pl] avec des points
   consécutifs (donc de la même couleur) nmax est 1+l'indice max du
   feedback buffer. Doit commencer par gl_point_token.  Rem: fournit
   la liste à l'envers ! On donne aussi la profondeur moyenne.
*)
  let feedback_parse_point r n0 nmax =
    let rec loop n c0 pl depsum nombre =
      if
        n >= nmax
        || Gl.Feedback.tokenf (Bigarray.Array1.get r n) <> Gl.Feedback.POINT
      then (pl, depsum, nombre, n)
      else
        let v = get_vertex r (n + 1) in
        let c = color_of_vertex v in
        if c <> c0 then (pl, depsum, nombre, n)
        else
          let p = point_of_vertex v and d = depth_of_vertex v in
          loop (n + 1 + gl_vertex_size) c0 (p :: pl) (depsum +. d) (nombre + 1)
    in
    let c0 = color_of_vertex (get_vertex r (n0 + 1)) in
    let pl, depsum, nombre, n = loop n0 c0 [] 0. 0 in
    ([ Color c0; Points pl ], depsum /. float nombre, n)

  (* fournit un plot object list [Color c ; Lines pl] avec des points
   consécutifs (donc de la même couleur) nmax est 1+l'indice max du
   feedback buffer. Doit commencer par gl_line_reset_token (??) (non
   revérifié !).  Rem1: fournit la liste à l'envers !  Rem2: on ignore
   si le point final d'un segment a une couleur différente du point
   initial... car de toutes façons xfig ne gère pas les dégradés.
*)
  let feedback_parse_line r n0 nmax =
    let rec loop n p0 c0 d0 pl depsum nombre =
      if
        n >= nmax
        || Gl.Feedback.tokenf (Bigarray.Array1.get r n) <> Gl.Feedback.LINE
      then (pl, depsum, nombre, n)
      else
        let v1, v2 =
          (get_vertex r (n + 1), get_vertex r (n + 1 + gl_vertex_size))
        in
        if
          color_of_vertex v1 <> c0 (* change de couleur *)
          || point_of_vertex v1 <> p0 (* ligne non continue *)
          || depth_of_vertex v1 <> d0
          (* change de profondeur  *)
        then (pl, depsum, nombre, n)
        else
          let p2 = point_of_vertex v2 and d = depth_of_vertex v2 in
          loop
            (n + 1 + (2 * gl_vertex_size))
            p2 c0 d0 (p2 :: pl) (depsum +. d) (nombre + 1)
    in
    let v1, v2 =
      (get_vertex r (n0 + 1), get_vertex r (n0 + 1 + gl_vertex_size))
    in
    let c1 = color_of_vertex v1
    and d = depth_of_vertex v1 +. depth_of_vertex v2
    and p2 = point_of_vertex v2 in
    let pl, depsum, nombre, n =
      loop
        (n0 + 1 + (2 * gl_vertex_size))
        p2 c1 d
        [ p2; point_of_vertex v1 ]
        d 2
    in
    ([ Color c1; Lines [ pl ] ], depsum /. float nombre, n)

  (* fournit un plot object list [Color c ; Poly pl] avec des points
   consécutifs (donc de la même couleur) nmax est 1+l'indice max du
   feedback buffer. Doit commencer par gl_polygon_token (non
   revérifié!).  Rem1: fournit la liste à l'envers ! Rem2: on ne prend
   pas en compte la différence de couleurs à chaque sommet. Rem3: en
   general opengl sépare tous les polygones en triangles !
*)
  (* a faire: fournir la couleur moyenne *)
  let feedback_parse_poly r n0 nmax =
    let rec loop n nfin pl depsum nombre =
      if n > nfin || n >= nmax then (pl, depsum, nombre, n)
      else
        let v = get_vertex r n in
        let p = point_of_vertex v and d = depth_of_vertex v in
        loop (n + gl_vertex_size) nfin (p :: pl) (depsum +. d) (nombre + 1)
    in
    let num = int_of_float (Bigarray.Array1.get r (n0 + 1)) in
    let v0 = get_vertex r (n0 + 2) in
    let c0 = color_of_vertex v0 in
    let pl, depsum, nombre, n =
      loop (n0 + 2) (n0 + 1 + (num * gl_vertex_size)) [] 0. 0
    in
    let poly_offset =
      0.0001
      (* !!!!!!!!!! à normaliser ? *)
    in
    ([ Color c0; Poly pl ], poly_offset +. (depsum /. float nombre), n)

  let feedback_parse_pass r n0 =
    let token = Bigarray.Array1.get r (n0 + 1) in
    if token = text_token then raise (Not_implemented "text token")
    else raise (Not_implemented "unknown pass-through")

  let depth_compare ((_, d1) : plot_object list * float) (_, d2) =
    if d1 > d2 then -1 else if d1 < d2 then 1 else 0

  (* renvoie une liste d'objets, directement visualisable avec display
*)
  let feedback_parse r nmax =
    let rec loop n pl =
      if n >= nmax then pl
      else
        let x = Bigarray.Array1.get r n in
        let open Gl.Feedback in
        let pl', dep, n' =
          match tokenf x with
          | POINT -> feedback_parse_point r n nmax
          | LINE | LINE_RESET -> feedback_parse_line r n nmax
          | POLYGON -> feedback_parse_poly r n nmax
          | PASS_THROUGH -> feedback_parse_pass r n
          | _ -> raise (Not_implemented "unknown token")
        in
        loop n' ((pl', dep) :: pl)
    in
    let liste = List.sort depth_compare (loop 0 []) in
    feedback_view () :: List.flatten (List.map (fun (pl, _) -> pl) liste)

  (**********************************************************)
  (*********** fin module feedback **************************)
  (**********************************************************)

  (* divers pour 3D *)
  (* on utilise ici des triplets car c'est ce qu'utilise lablgl... ? *)

  let ( +| ) (x0, y0, z0) (x1, y1, z1) = (x0 +. x1, y0 +. y1, z0 +. z1)
  let ( -| ) (x0, y0, z0) (x1, y1, z1) = (x0 -. x1, y0 -. y1, z0 -. z1)
  let ( *| ) s (x, y, z) = (s *. x, s *. y, s *. z)
  let pscal (x0, y0, z0) (x1, y1, z1) = (x0 *. x1) +. (y0 *. y1) +. (z0 *. z1)
  let norm r = 1. /. sqrt (pscal r r) *| r

  let pvect (x0, y0, z0) (x1, y1, z1) =
    ( (z0 *. y1) -. (z1 *. y0),
      (x0 *. z1) -. (x1 *. z0),
      (y0 *. x1) -. (y1 *. x0) )

  let unit_normal a b c = norm (pvect (c -| b) (a -| b))

  (******************)

  let light_on = ref true
  let get_light () = !light_on

  let toggle_light () =
    light_on := not !light_on;
    reset_gllist := true

  let switch_light bool =
    match bool with
    | true ->
        Gl.enable Gl.lighting;
        Gl.lightfv Gl.light0 Gl.position [| 1.; -1.; 1.; 0.5 |];
        Gl.lightfv Gl.light0 Gl.specular [| 0.; 0.; 0.; 1. |];
        Gl.lightfv Gl.light0 Gl.diffuse [| 0.2; 0.2; 0.2; 0.8 |];
        Gl.light_modelf Gl.light_model_two_side 1.0;
        Gl.enable Gl.light0;
        Gl.materialf Gl.front Gl.shininess 30.;
        Gl.materialfv Gl.front Gl.emission [| 0.2; 0.2; 0.2; 1. |];
        Gl.materialf Gl.back Gl.shininess 10.;
        Gl.materialfv Gl.back Gl.emission [| 0.1; 0.1; 0.1; 1. |];
        Gl.enable Gl.color_material_enum;
        Gl.color_material Gl.front_and_back Gl.specular;
        Gl.color_material Gl.front_and_back Gl.ambient_and_diffuse
    | false ->
        Gl.disable Gl.lighting;
        Gl.disable Gl.color_material_enum

  (**********  pour entrer dans le mode oplot 3D ************)
  let enter3d ({ Point3.x = x1; y = y1; _ }, { Point3.x = x2; y = y2; _ }) =
    Gl.push_matrix ();
    Gl.matrix_mode Gl.projection;
    Gl.push_matrix ();
    Gl.load_identity ();
    Gl.ortho x1 x2 y1 y2 (-100.) 100.;
    (* à remplacer par les valeurs zmin zmax, ajustées pour permettre la rotation *)
    Gl.matrix_mode Gl.modelview;
    Gl.load_identity ();
    Gl.translatef 0. 0. (-50.);
    (*perspective : bien ?
    GlMat.frustum ~x:(-1. , 1.) ~y:(-. 600. /. 800. , 600. /. 800.) ~z:(2. , 6.);
    Gl.translate ~z:(-4.) (); *)
    switch_light !light_on;

    (* si on met dans une displaylist la lumière n'est calculée qu'une fois. *)

    (*****************)
    let zoom = !zoom3d in
    Gl.scalef zoom zoom zoom;
    (* inutile et lent ? *)
    let rot = Geom.q_matrix_ba !position3d in
    Gl.mult_matrixf rot;
    Gl.enable Gl.depth_test

  let leave3d () =
    Gl.disable Gl.depth_test;
    Gl.matrix_mode Gl.projection;
    switch_light false;
    Gl.pop_matrix ();
    Gl.matrix_mode Gl.modelview;
    Gl.pop_matrix ()

  (*********** partie tracé *************)

  (* il doit y avoir un moyen d'éviter ça, mais je ne sais pas faire
   "p.bla" où bla vaut x,y ou z *)
  let getx p = p.x
  let gety p = p.y

  (* applique la fonction f aux coordonnées c de la liste de points pl,
   à l'envers: -- on doit pouvoir faire plus simple *)
  let mymap f pl c =
    let myget = match c with X -> getx | Y -> gety in
    match pl with
    | [] -> raise Empty_list
    | p :: ppl ->
        let xlist l = List.rev_map myget l in
        List.fold_left f (myget p) (xlist ppl)

  let fmin x y : float = if y < x then y else x
  let fmax x y : float = if y > x then y else x

  (* determine les extrêma du tracé  (inutilisé) *)
  let xmin pl = mymap fmin pl X
  let xmax pl = mymap fmax pl X
  let ymin pl = mymap fmin pl Y
  let ymax pl = mymap fmax pl Y

  (* on a trois échelles:
   * celle, mathématique, de la fonction; on les appelle "point".
   * celle, logique, des procédures de traçage; "draw"
   * celle, physique, des pixels de la fenêtre. "pixel"
   *)

  (* convertir la liste de points en une liste de coordonnées flottantes ( x , y )
   de façon que la fenêtre "view" soit adaptée à la "bounding box". Autrement
   dit, maths->logique *)
  (* Attention la liste devient inversée (pour plus d'efficacité) !! *)
  let rescale_list pl v (bx0, by0, bx1, by1) =
    (* changer le nom *)
    match v with
    | None ->
        if pl <> [] then raise View_expected
        else begin
          Debug.print "Warning: no view provided for rescale_list";
          []
        end
    | Some ({ x = x0; y = y0 }, { x = x1; y = y1 }) ->
        let xmin, xfactor =
          if x1 = x0 then (-0.5, bx1 -. bx0)
          else (x0, (bx1 -. bx0) /. (x1 -. x0))
        in
        let ymin, yfactor =
          if y1 = y0 then (-0.5, by1 -. by0)
          else (y0, (by1 -. by0) /. (y1 -. y0))
        in
        (* on met une largeur de 1 et on centre au cas où la largeur du dessin est
         nulle *)
        let dr_of_point p =
          let myi x = bx0 +. ((x -. xmin) *. xfactor) in
          let myj y = by0 +. ((y -. ymin) *. yfactor) in
          (myi (getx p), myj (gety p))
        in
        List.rev_map dr_of_point pl

  (* juste pour un point: *)
  let draw_of_point p v (bx0, by0, bx1, by1) =
    List.hd (rescale_list [ p ] v (bx0, by0, bx1, by1))

  (* cas 3d, uniquement pour openGL *)
  let rescale_3dpoint (x, y, z) (x0, y0, z0) (x1, y1, z1) =
    ( (x -. x0) *. 2. /. (x1 -. x0),
      (y -. y0) *. (12. /. 8.) /. (y1 -. y0),
      -2. +. ((z -. z0) *. 4. /. (z1 -. z0)) )

  let rescale_3dlist pl = List.rev_map rescale_3dpoint pl

  (* inversement, convertit des dim en coords "draw" adaptées à
   la "bounding box" en une taille adaptée à la "view" -inutilisé pour
   le moment *)
  (* le faire en coord absolues ? *)
  let point_of_draw (dx, dy) (bx0, by0, bx1, by1) = function
    | None -> raise View_expected
    | Some ({ x = x0; y = y0 }, { x = x1; y = y1 }) ->
        ((x1 -. x0) *. dx /. (bx1 -. bx0), (y1 -. y0) *. dy /. (by1 -. by0))

  (* le deuxième arg est un view *)
  let point_of_pixel (dx, dy) = function
    | None -> raise View_expected
    | Some ({ x = x0; y = y0 }, { x = x1; y = y1 }) ->
        ( float dx *. (x1 -. x0) /. !fwindow_width,
          float dy *. (y1 -. y0) /. !fwindow_height )

  (*   point_of_draw (draw_of_pixel (dx,dy) (bx0,by0,bx1,by1)) v (bx0,by0,bx1,by1);; *)

  (* détermine la "view" optimale pour un objet. Utilisé seulement en début de
   tracé, lorsqu'aucune "view" n'a déjà été imposée.  Attention on ajoute
   parfois +/- 1 lorsque l'objet a une de ses dimensions égales à zéro ! *)
  let rec maxview po =
    match po with
    | Points pl | Poly pl ->
        if pl = [] then None
        else
          let x0, y0, x1, y1 =
            (mymap fmin pl X, mymap fmin pl Y, mymap fmax pl X, mymap fmax pl Y)
          in
          let x1 = if x1 = x0 then x0 +. 1. else x1 in
          let y1 = if y1 = y0 then y0 +. 1. else y1 in
          Some (point (x0, y0), point (x1, y1))
    | Lines pll -> maxview (Points (List.flatten pll))
    | View v -> v
    | Axis { center = { x = x0; y = y0 }; _ } ->
        Some (point (x0 -. 1., y0 -. 1.), point (x0 +. 1., y0 +. 1.))
    | Text t ->
        let x, y = (t.pos.x, t.pos.y) in
        Some (point (x -. 1., y -. 1.), point (x +. 1., y +. 1.))
    | Matrix _ -> None
    | Grid ((_, v3, _), _) -> view2of3 v3
    | Surf3d ((_, _, _, v3, _), _) -> view2of3 v3
    | Adapt (_, f) -> maxview (f None)
    | User _ | Anim _ ->
        None
        (* Some (point(-.1., -.1.), point(1.,1.)) *)
        (* mieux que rien ... *)
    | _ -> None

  (*********** partie device dependent ******************)
  (*************** convertir GL(3D) en FIG utilisant le feedback ********)
  (* gldraw_func est la fonction qui lance les ordres opengl,
   et plot_func est la fonction object_plot *)
  (* vérifier le fonctionnement des view. refaire en utilisant depth de xfig ? *)
  let gl2fig gldraw_func plot_func =
    let was_init = Sdl.Init.test (Sdl.was_init None) Sdl.Init.video in
    (* gl_init (); *)
    (*enter3d v3;*)
    (*print_endline "enter3D OK";*)
    let r, num = feedback_render gldraw_func in
    Debug.print "feedback_render OK";
    let parsed = feedback_parse r num in
    (* Raw.free_static r; *)
    let fb_view, fb_list = (maxview (List.hd parsed), List.tl parsed) in
    List.iter (fun o -> plot_func ~dev:FIG o fb_view) fb_list;
    (* leave3d (); *)
    if not was_init then close () ~dev:GL

  (**************)

  let copy_back_buffer () =
    Gl3.draw_buffer Gl3.front;
    Gl3.read_buffer Gl3.back;
    Gl.copy_pixels 0 0 !window_width !window_height Gl.color;
    Gl3.draw_buffer Gl3.back;
    Gl3.read_buffer Gl3.back

  let copy_to_back_buffer () =
    Gl3.draw_buffer Gl3.back;
    Gl3.read_buffer Gl3.front;
    Gl.copy_pixels 0 0 !window_width !window_height Gl.color;
    Gl3.read_buffer Gl3.back

  let buffer_enum i =
    (* FIXME this is wrong, it cannot be an argument of draw_buffer, see https://registry.khronos.org/OpenGL-Refpages/gl2.1/xhtml/glDrawBuffer.xml *)
    assert (i >= 0 && i < 16);
    Gl3.draw_buffer0 + i

  let copy_buffer i =
    (* wrong, see above *)
    Gl3.draw_buffer (buffer_enum i);
    Gl3.read_buffer Gl3.back;
    Gl.copy_pixels 0 0 !window_width !window_height Gl.color;
    Gl3.draw_buffer Gl3.back;
    Gl3.read_buffer Gl3.back

  let recall_buffer i =
    (* wrong, see above *)
    Gl3.read_buffer (buffer_enum i);
    Gl3.draw_buffer Gl3.back;
    Gl.copy_pixels 0 0 !window_width !window_height Gl.color;
    Gl3.draw_buffer Gl3.back;
    Gl3.read_buffer Gl3.back

  let user_flush = function
    | GRAPHICS -> Graphics.synchronize ()
    | GL -> (
        match !default_gl with
        | GLUT -> Iglut.swapbuffers ()
        | SDL -> do_option !win Sdl.gl_swap_window
        | GTK -> () (* ??? *))
    | FIG -> ()

  (***********************************)
  (* gestion des fontes ttf avec sdl *)
  (*  (pas si facile !)                 *)

  let sdl_get_pixel_not_used surface x y =
    (* in principle one should check with MUST_LOCK if the surface is RLE encoded
     (otherwise there is no need to lock, see
     https://wiki.libsdl.org/SDL_LockSurface). But that's ok, if not, call to
     Sdl.lock_surface does almost nothing. *)
    let pitch = Sdl.get_surface_pitch surface in
    let format_enum = Sdl.get_surface_format_enum surface in
    if format_enum <> Sdl.Pixel.format_argb8888 then begin
      Sdl.log "sdl_get_pixel: surface has wrong format";
      exit 1
    end;
    go (Sdl.lock_surface surface);
    let pixels = Sdl.get_surface_pixels surface Bigarray.int8_unsigned in
    (* Warning! the pitch can be larger than w * 4 !*)
    let i0 = (y * pitch) + (4 * x) in
    let open Bigarray in
    let b = Array1.get pixels i0 in
    let g = Array1.get pixels (i0 + 1) in
    let r = Array1.get pixels (i0 + 2) in
    let a = Array1.get pixels (i0 + 3) in
    Sdl.unlock_surface surface;
    ((r, g, b), a)

  (*  let r = GlPix.read ~x:0 ~y:0 ~width:!window_width ~height:!window_height ~format:`rgba ~kind:`ubyte;; *)

  (* produit une surface sdl a partir du code LaTeX. Utilise latex,
   dvips et gs... *)
  (* à faire: détecter l'encodage... *)
  let latex_to_sdl message size =
    let current_dir = Sys.getcwd () in
    Sys.chdir !tmp_dir;
    let latex_channel = open_out latex_tmp in
    output_string latex_channel
      "\\documentclass{article}\n\
       \\usepackage{color}\n\
       \\usepackage[utf8]{inputenc}\n\
       \\usepackage[active,tightpage]{preview}\n\
       \\begin{document}\n\
       \\begin{preview}\n";
    output_string latex_channel message;
    output_string latex_channel "\\end{preview}\n\\end{document}\n";
    close_out latex_channel;
    let base_name = Filename.chop_suffix latex_tmp ".tex" in
    shell "latex '\\nonstopmode\\input{%s}'" latex_tmp;
    shell "dvips -D 600 %s.dvi -o %s.ps" base_name base_name;
    shell
      "gs -sDEVICE=pngalpha -dTextAlphaBits=4 -r%d -dGraphicsAlphaBits=4 \
       -dSafer -q -dNOPAUSE -sOutputFile=%s.png %s.ps -c quit"
      (* si on n'a pas pngalpha:*)
      (* shell "gs -sDEVICE=ppmraw -dTextAlphaBits=4 -r%d -dGraphicsAlphaBits=4 -dSafer -q -dNOPAUSE -sOutputFile=%s.ppm %s.ps -c quit"  *)
      (* ensuite faire quleque chose comme  *)
      (* convert -transparent white  oplot-tmp.ppm aaa.png *)
      (size * 6)
      base_name base_name;
    let image =
      Tsdl_image.Image.load (Printf.sprintf "%s.png" base_name) |> go
    in
    Sys.chdir current_dir;
    if Sdl.get_surface_format_enum image = Sdl.Pixel.format_argb8888 then image
    else Sdl.convert_surface_format image Sdl.Pixel.format_argb8888 |> go

  (*supprimer les fichiers tmp *)
  (* Pour faire un fichier eps, puis eventuellement svg ou fig en remplacant le texte par du vectoriel :
   os.system('dvips -q -f -E -D 600 -y 5000 -o ' + ps_file + ' ' + dvi_file)
   #os.system('cd ' + base_dir)
   os.system('pstoedit -f plot-svg -dt -ssp ' + ps_file + ' ' + svg_file + '> ' + out_file)
   svg_open(self, svg_file)
   Ca vient du fichier /usr/share/inkscape/extensions/eqtexsvg.py

   ->-> creer un objet Postscript, qui sera transformé en image pour opengl
   (on peut aussi creer un objet Image)
*)

  (* renvoie une image contenant le message affiché avec la
   fonte FreeSans ou avec LaTeX, ainsi que les dimensions du texte *)
  (* would be cool to have instead a vectorial drawing of the glyphs... *)
  let text_image message size flag =
    match flag with
    | Normal ->
        if size <> !current_font_size then (
          current_font := Sdlttf.open_font !font_path size |> go;
          current_font_size := size);
        (* avoid opening the same font every time *)
        let s =
          Sdlttf.render_utf8_blended !current_font message
            (sdl_color (opaque white))
          (* we use white here and then modulate *)
          |> go
        in
        Sdl.convert_surface_format s Sdl.Pixel.format_abgr8888 |> go
    | Latex -> latex_to_sdl message size

  (* l'image est rescalée pour que la taille soit indépendante de tout
   (donc une fonte 12 points affiche toujours 12 pixels) *)
  (* je ne sais pas si c'est le mieux *)

  (* affiche une image en position x0 y0 et mode opengl "mode" *)
  let draw_image ?(mode = Gl.modulate) image x0 y0 =
    Gl3.tex_parameteri Gl3.texture_2d Gl3.texture_mag_filter Gl3.nearest;
    Gl3.tex_parameteri Gl3.texture_2d Gl3.texture_min_filter Gl3.nearest;

    (* Gl3.pixel_storei Gl3.unpack_row_length 0; *)
    go (Sdl.lock_surface image);
    let pixels = Sdl.get_surface_pixels image Bigarray.int8_unsigned in
    let w, h = Sdl.get_surface_size image in
    (* https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexImage2D.xhtml *)
    Gl3.tex_image2d Gl3.texture_2d 0 Gl3.rgba w h 0 Gl3.rgba Gl3.unsigned_byte
      (`Data pixels);
    Sdl.unlock_surface image;

    (* utilisation de image comme texture *)
    Gl.enable Gl.texture_2d;

    (* Gl3.enable Gl3.blend; *)
    (* on trace un rectangle de couleur sur lequel la texture va se
     fixer *)
    let rx, ry = (float w /. !fwindow_width, float h /. !fwindow_height) in
    (* ratios à adapter à bounding box : draw of pixel !window_width
     !window_height. 601 au lieu de 600 pour un petit pb d'arrondi (?) que je
     ne comprends pas trop *)
    Gl.tex_envi Gl.texture_env Gl.texture_env_mode mode;
    Gl.gl_begin Gl.quads;
    Gl.tex_coord2d 0.0 0.0;
    Gl.vertex2d x0 (y0 +. ry);
    Gl.tex_coord2d 0.0 1.0;
    Gl.vertex2d x0 y0;
    Gl.tex_coord2d 1.0 1.0;
    Gl.vertex2d (x0 +. rx) y0;
    Gl.tex_coord2d 1.0 0.0;
    Gl.vertex2d (x0 +. rx) (y0 +. ry);
    Gl.gl_end ();
    Gl.disable Gl.texture_2d
  (* Gl3.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;; *)

  (***********************************)
  (* screenshot *)
  (***********************************)

  let sdl_screenshot ?(output = png_output) () =
    Gl.finish ();
    (* let t0 = time () in *)
    let w = !window_width + !left_margin + !right_margin in
    let h = !window_height + !top_margin + !bottom_margin in

    let ba_gl =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 4)
    in
    Gl3.read_pixels 0 0 w h Gl3.rgba Gl3.unsigned_byte (`Data ba_gl);

    let ba_sdl =
      Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (w * h * 4)
    in

    (* Because of bottom-left origin of openGL pixels, we have to flip
     vertically. One could use glClipControl(GL_UPPER_LEFT, GL_ZERO_TO_ONE)
     instead, but this would require openGL 4.x *)
    let pitch = 4 * w in
    for y = 0 to h - 1 do
      let src = Bigarray.Array1.sub ba_gl (y * pitch) pitch in
      let dst = Bigarray.Array1.sub ba_sdl ((h - 1 - y) * pitch) pitch in
      Bigarray.Array1.blit src dst
    done;

    let s =
      Sdl.create_rgb_surface_from ba_sdl ~w ~h ~depth:32 ~pitch:(w * 4)
        0x000000ffl (* Rmask *)
        0x0000ff00l (* Gmask *)
        0x00ff0000l (* Bmask *)
        0xff000000l (* Amask *)
      |> go
    in

    match Tsdl_image.Image.save_png s output with
    | 0 -> print_endline (Printf.sprintf "Screenshot saved to [%s]." output)
    | i -> Sdl.log "Error %i when saving screenshot to: %s" i output

  (********************)
  (* tracé des objets *)
  (********************)

  let set_line_width ?(dev = !default_device) w =
    match dev with
    | GRAPHICS -> Graphics.set_line_width (int_of_float w)
    | GL -> Gl3.line_width w
    | FIG -> Debug.print "Not implemented: fig set_line_width"

  let set_point_size ?(dev = !default_device) w =
    match dev with
    | GRAPHICS -> raise (Not_implemented "GRAPHICS set_point_size")
    | GL -> Gl3.point_size w
    | FIG -> raise (Not_implemented "fig set_line_size")

  let set_color ?(dev = !default_device) c =
    (match dev with
    | GRAPHICS ->
        let r, g, b = int_of_color c in
        Graphics.set_color (Graphics.rgb r g b)
    | GL -> gl_draw_color c
    | FIG ->
        if fig_of_color c = -1 then (
          let r = rgb_of_color c in
          Printf.fprintf !xfig_head_channel "0 %d #%.6x\n" !fig_color_counter r;
          fig_colors.(!fig_color_counter) <- r;
          incr fig_color_counter));
    (* need to be careful xfig has only 512 user colors and in fact probably much
     less. maybe 255-32 colors *)
    (* enregistrer dans un tableau puis faire un réduction du nombre de couleurs
     par interpolation ?? ou remplacer par svg ? *)
    current_color := c

  let linear_cmap color1 color2 x =
    let f x u1 u2 = (x *. u2) +. ((1. -. x) *. u1) in
    {
      r = f x color1.r color2.r;
      g = f x color1.g color2.g;
      b = f x color1.b color2.b;
    }

  (* du noir à la 'color '*)
  let from_black_cmap = linear_cmap black
  let from_white_cmap = linear_cmap white

  (* de la 'color' au blanc *)
  let to_white_cmap color = linear_cmap color white
  let to_black_cmap color = linear_cmap color black

  let draw_points pl ?(dev = !default_device) ?dep ?(pixel_size = 2) view =
    (* xfig reduces line width by a factor of two to "lighten" the look. For
     isolated points, we scale it back to have adjacent points slightly
     overlapping (instead of being clearly disjoint). *)
    let ps = rescale_list pl view (bounding_box dev) in
    match dev with
    | GRAPHICS ->
        Graphics.plots
          (Array.of_list
             (List.rev_map (fun (x, y) -> (int_of_float x, int_of_float y)) ps))
    | GL ->
        Gl.gl_begin Gl.points;
        List.iter (fun (x, y) -> Gl.vertex2f x y) ps;
        Gl.gl_end ()
    | FIG ->
        (* we crop points outside the view *)
        let ps =
          match view with
          | Some v ->
              rescale_list
                (lines_crop pl v |> List.flatten)
                view (bounding_box dev)
          | None ->
              Debug.print "draw_points needs a view";
              ps
        in
        let depth = get_depth dep and co = fig_of_color !current_color in
        List.iter
          (fun (x, y) ->
            Printf.fprintf !xfig_main_channel
              "2 1 0 %u %d %d %d -1 -1 0.000 0 0 -1 0 0 1\n" pixel_size co co
              depth;
            Printf.fprintf !xfig_main_channel "\t%d %d\n" (int_of_float x)
              (int_of_float y))
          ps

  let draw_lines pl ~dev ?dep view =
    let ps = rescale_list pl view (bounding_box dev) in
    match dev with
    | GRAPHICS ->
        Graphics.draw_poly_line
          (Array.of_list
             (List.rev_map (fun (x, y) -> (int_of_float x, int_of_float y)) ps))
    | GL ->
        Gl.gl_begin Gl.line_strip;
        List.iter (fun (x, y) -> Gl.vertex2f x y) ps;
        Gl.gl_end ()
    | FIG ->
        let depth = get_depth dep and co = fig_of_color !current_color in
        Printf.fprintf !xfig_main_channel
          "2 1 0 %u %d %d %d -1 -1 0.000 0 0 -1 0 0 %d\n" (* iscale *) 1 co co
          depth (List.length ps);
        List.iter
          (fun (x, y) ->
            Printf.fprintf !xfig_main_channel "\t%d %d\n" (int_of_float x)
              (int_of_float y))
          ps

  let draw_lines pl ?(dev = !default_device) ?dep view =
    if dev = FIG then (
      (* latex n'aime pas les dimensions trop grandes*)
      let pls =
        match view with
        | None -> raise View_expected
        | Some v -> lines_crop pl v
      in
      if Debug.debug && List.length pls > 1 then
        print_endline "Cropping overflowing Lines for FIG rendering";
      List.iter (fun pl -> draw_lines pl ~dev ?dep view) pls)
    else draw_lines pl ~dev ?dep view

  let draw_poly pl ?(dev = !default_device) ?border_color ?dep view =
    let ps = rescale_list pl view (bounding_box dev) in
    match dev with
    | GRAPHICS ->
        Graphics.fill_poly
          (Array.of_list
             (List.rev_map (fun (x, y) -> (int_of_float x, int_of_float y)) ps))
    | GL ->
        Gl.gl_begin Gl.polygon;
        List.iter (fun (x, y) -> Gl.vertex2f x y) ps;
        Gl.gl_end ();
        (* on retrace le bord pour anti-aliasing. A éviter en mode
         feedback ? *)
        Gl.gl_begin Gl.line_loop;
        List.iter (fun (x, y) -> Gl.vertex2f x y) ps;
        Gl.gl_end ()
    | FIG ->
        let depth = get_depth dep and co = fig_of_color !current_color in
        let bco =
          match border_color with None -> co | Some bc -> fig_of_color bc
        in
        let pps = List.append ps [ List.hd ps ] in
        (* on boucle *)
        Printf.fprintf !xfig_main_channel
          "2 3 0 0 %d %d %d -1 20 0.000 0 0 -1 0 0 %d\n" bco co depth
          (List.length pps);
        List.iter
          (fun (x, y) ->
            Printf.fprintf !xfig_main_channel "\t%d %d\n" (int_of_float x)
              (int_of_float y))
          pps

  (* modifier les coords GL *)
  let draw_matrix ?(dev = !default_device) ?(cmap = from_white_cmap)
      ?(min_value = 0) ?(max_value = 255) m =
    let h = Array.length m and w = Array.length m.(0) in
    (* w,i=lignes, h,j=colonnes *)
    let dc = float (max_value - min_value) in
    let cmap = cmap !current_color in
    begin match dev with
    | GRAPHICS -> raise (Not_implemented "GRAPHICS draw_matrix")
    | GL ->
        let dx = 1. /. float w and dy = 1. /. float h in
        for i = 0 to h - 1 do
          for j = 0 to w - 1 do
            let c = float (m.(i).(j) - min_value) /. dc in
            let x = float j *. dx and y = float i *. dy in
            gl_draw_color (cmap c);
            Gl.gl_begin Gl.quads;
            Gl.vertex2f x y;
            Gl.vertex2f x (y +. dy);
            Gl.vertex2f (x +. dx) (y +. dy);
            Gl.vertex2f (x +. dx) y;
            Gl.gl_end ()
          done
        done
    | FIG ->
        let xmin, ymin, xmax, ymax =
          (0., 0., float w, float h)
          (* (match view with
           *  | None -> raise View_expected
           *  | Some  ( { x=x0 ; y=y0 },{ x=x1 ; y=y1 }) -> x0,y0,x1,y1) *)
        in
        let view = Some ({ x = xmin; y = ymin }, { x = xmax; y = ymax }) in
        let dx = (xmax -. xmin) /. float w and dy = (ymax -. ymin) /. float h in
        for i = 0 to h - 1 do
          for j = 0 to w - 1 do
            let c = float (m.(i).(j) - min_value) /. dc in
            let x = xmin +. (float j *. dx) and y = ymin +. (float i *. dy) in
            set_color ~dev (cmap c);
            draw_poly ~dev
              [
                { x; y };
                { x; y = y +. dy };
                { x = x +. dx; y = y +. dy };
                { x = x +. dx; y };
              ]
              view
          done
        done
    end;
    set_color !current_color

  let move3d m =
    let t0 =
      match m.init_time with
      | None ->
          (match m.move with
          | Translate _ -> ()
          | Rotate _ -> ()
          | Zoom (_, Some z0) -> zoom3d := z0
          | Zoom _ -> ());
          let t = time () - int_of_float (1000. *. m.time.min) in
          m.init_time <- Some t;
          t
      | Some t -> t
    in
    if m.time.min < m.time.max then begin
      let t = m.time.min in
      let t' = float (time () - t0) /. 1000. in
      m.time.min <- t';
      let dt = t' -. t in
      match m.move with
      | Translate _ -> raise (Not_implemented "translate3d")
      | Rotate q -> position3d := Geom.q_mult q !position3d
      | Zoom (z, _) -> zoom3d := !zoom3d *. (1. +. (dt *. z t))
    end

  let normal3 (x, y, z) = Gl.normal3f x y z
  let vertex3 (x, y, z) = Gl.vertex3f x y z

  let rec draw_surf3d ?(dev = !default_device) ?(wire = true) gl plot_func mx my
      mz (p1, p2) =
    match dev with
    | GRAPHICS -> raise (Not_implemented "GRAPHICS draw_surf3d")
    | GL -> begin
        enter3d (p1, p2);
        (* ne peut pas être mis dans la displaylist car contient la rotation qui
         doit s'actualiser *)
        match !gl with
        | Some list when not !reset_gllist -> Gl.call_list list
        | _ ->
            (* on stocke les ordres graphiques dans une "display_list" opengl *)
            Debug.print "Creating display list";
            let list = Gl.gen_lists 1 in
            Gl.new_list list Gl.COMPILE_AND_EXECUTE;
            let h = Array.length mx - 2 and w = Array.length mx.(0) - 2 in
            let { r; g; b } = !current_color in
            (* faire une touche pour ça: *)
            (* let () = GlLight.material ~face:`front
           *     (`emission (r/.10., g/.10., b/.10., 1.)) in *)
            let zmin = p1.Point3.z and zmax = p2.Point3.z in
            let setcolor =
              if (r *. r) +. (g *. g) +. (b *. b) > 1. then
                fun ((_, _, z) : float * float * float) ->
                let c = (z -. zmin) /. (zmax -. zmin) in
                gl_draw_color { r = r *. c; g = g *. c; b = b *. c }
              else fun ((_, _, z) : float * float * float) ->
                let c = (z -. zmin) /. (zmax -. zmin) in
                gl_draw_color
                  {
                    r = c +. r -. (r *. c);
                    g = c +. g -. (g *. c);
                    b = c +. b -. (b *. c);
                  }
            in
            let a i j = (mx.(i).(j), my.(i).(j), mz.(i).(j)) in
            let normal_vector i j =
              let a0, a1, a2, a3, a4 =
                (a i j, a i (j - 1), a (i + 1) j, a i (j + 1), a (i - 1) j)
              in
              unit_normal a0 a1 a2
              +| unit_normal a0 a2 a3
              +| unit_normal a0 a3 a4
              +| unit_normal a0 a4 a1
            in
            (* mieux vaut calculer les normales au début avec la fonction ? *)
            let a0 = ref (0., 0., 0.) and a1 = ref (0., 0., 0.) in
            for i = 1 to h - 1 do
              (* à refaire...*)
              a0 := (mx.(i).(1), my.(i).(1), mz.(i).(1));
              a1 := (mx.(i + 1).(1), my.(i + 1).(1), mz.(i + 1).(1));
              gl_draw_color !current_color;
              for j = 1 to w - 1 do
                let a3 = (mx.(i).(j + 1), my.(i).(j + 1), mz.(i).(j + 1))
                and a2 =
                  (mx.(i + 1).(j + 1), my.(i + 1).(j + 1), mz.(i + 1).(j + 1))
                in
                Gl.gl_begin Gl.quads;
                (* ou faire quad_strip ? *)
                if not !light_on then setcolor !a0;
                normal3 (normal_vector i j);
                vertex3 !a0;
                if not !light_on then setcolor !a1;
                normal3 (normal_vector (i + 1) j);
                vertex3 !a1;
                if not !light_on then setcolor a2;
                normal3 (normal_vector (i + 1) (j + 1));
                vertex3 a2;
                if not !light_on then setcolor a3;
                normal3 (normal_vector i (j + 1));
                vertex3 a3;
                Gl.gl_end ();

                a0 := a3;
                a1 := a2
              done
            done;

            (* wire: à faire seulememnt une fois les polygones tracés, sinon pb
             aliasing *)
            if wire then begin
              switch_light false;
              (* bien ? *)
              let r, g, b =
                if !light_on then (r, g, b) else (r /. 2., g /. 2., b /. 2.)
              in
              gl_draw_color { r; g; b };
              for ii = 1 to h / 2 do
                let i = ii * 2 in
                Gl.gl_begin Gl.line_strip;
                for j = 1 to w do
                  let a = (mx.(i).(j), my.(i).(j), mz.(i).(j)) in
                  vertex3 a
                done;
                Gl.gl_end ()
              done;
              for jj = 1 to w / 2 do
                let j = jj * 2 in
                Gl.gl_begin Gl.line_strip;
                for i = 1 to h do
                  let a = (mx.(i).(j), my.(i).(j), mz.(i).(j)) in
                  vertex3 a
                done;
                Gl.gl_end ()
              done
            end;

            leave3d ();
            Gl.end_list ();
            Debug.print "display list created.";
            gl := Some list
      end
    | FIG ->
        let draw () =
          draw_surf3d ~dev:GL ~wire:true gl plot_func mx my mz (p1, p2)
        in
        gl2fig draw plot_func

  (* modifier les coords GL *)
  let rec draw_grid gl ?(dev = !default_device) ?(wire = true) plot_func m
      (p1, p2) =
    let h = Array.length m - 1 and w = Array.length m.(0) - 1 in
    (* w,i=lignes, h,j=colonnes *)
    let { r; g; b } = !current_color in
    match dev with
    | GRAPHICS -> raise (Not_implemented "GRAPHICS draw_grid")
    | GL -> (
        enter3d (p1, p2);
        match !gl with
        | Some list when not !reset_gllist -> Gl.call_list list
        | _ ->
            (* on stocke les ordres graphiques dans une "display_list" opengl *)
            let list = Gl.gen_lists 1 in
            Gl.new_list list Gl.COMPILE_AND_EXECUTE;
            let { Point3.x = x1; y = y1; z = zmin } = p1
            and { Point3.x = x2; y = y2; z = zmax } = p2 in
            let dx = (x2 -. x1) /. float w and dy = (y2 -. y1) /. float h in
            let setcolor =
              if (r *. r) +. (g *. g) +. (b *. b) > 1. then fun z ->
                let c = (z -. zmin) /. (zmax -. zmin) in
                gl_draw_color { r = r *. c; g = g *. c; b = b *. c }
              else fun z ->
                let c = (z -. zmin) /. (zmax -. zmin) in
                gl_draw_color
                  {
                    r = c +. r -. (r *. c);
                    g = c +. g -. (g *. c);
                    b = c +. b -. (b *. c);
                  }
            in
            (* solid triangles(quads) *)
            for i = 0 to h - 1 do
              let y = y1 +. (float i *. dy) in
              for j = 0 to w - 1 do
                let x = x1 +. (float j *. dx) and z = m.(i).(j) in
                setcolor z;
                Gl.gl_begin Gl.quads;
                vertex3 (x, y, z);
                let z = m.(i + 1).(j) in
                setcolor z;
                vertex3 (x, y +. dy, z);
                let z = m.(i + 1).(j + 1) in
                setcolor z;
                vertex3 (x +. dx, y +. dy, z);
                let z = m.(i).(j + 1) in
                setcolor z;
                vertex3 (x +. dx, y, z);
                Gl.gl_end ()
              done
            done;
            gl_draw_color !current_color;

            if wire then begin
              (* rem: pas pour xfig, utiliser des polygones à bord plutôt  ? *)
              (*   GlDraw.polygon_mode ~face:`both `line ; *)
              (*   for i=0 to (h-1) do *)
              (*     let y = -.600. /. 800. +. (float i) *. dy in *)
              (*       for j=0 to (w-1) do *)
              (*         let x  = -.1. +. (float j) *. dx *)
              (*         and z = m.(i).(j) in *)
              (*    Gl.gl_begin `quads; *)
              (*    vertex3(x, y, z); *)
              (*    let z = m.(i+1).(j) in *)
              (*      vertex3(x, y +. dy, z); *)
              (*      let z = m.(i+1).(j+1) in *)
              (*        vertex3(x +. dx, y +. dy, z); *)
              (*        let z = m.(i).(j+1) in *)
              (*          vertex3(x +. dx, y, z); *)
              (*          Gl.gl_end () *)
              (*       done; *)
              (*   done; *)
              (*  GlDraw.polygon_mode ~face:`both `fill ; *)

              (*Gl3.line_width 1.5;*)
              (* on trace une ligne sur deux*)
              for ii = 0 to h / 2 do
                let i = ii * 2 in
                let y = y1 +. (float i *. dy) and z = m.(i).(0) in
                Gl.gl_begin Gl.line_strip;
                vertex3 (x1, y, z);
                for j = 1 to w do
                  let x = x1 +. (float j *. dx) and z = m.(i).(j) in
                  vertex3 (x, y, z)
                done;
                Gl.gl_end ()
              done;
              for jj = 0 to w / 2 do
                let j = jj * 2 in
                let x = x1 +. (float j *. dx) and z = m.(0).(j) in
                Gl.gl_begin Gl.line_strip;
                vertex3 (x, y1, z);
                for i = 1 to h do
                  let y = y1 +. (float i *. dy) and z = m.(i).(j) in
                  vertex3 (x, y, z)
                done;
                Gl.gl_end ()
              done
              (*Gl3.line_width 1.;*)
            end;
            leave3d ();
            Gl.end_list ();
            gl := Some list)
    | FIG ->
        let draw () = draw_grid ~dev:GL ~wire:true gl plot_func m (p1, p2) in
        gl2fig draw plot_func
  (*    set_color !current_color;;*)

  let rec draw_curve3d ?(dev = !default_device) gl plot_func p3d (p1, p2) =
    match dev with
    | GRAPHICS -> raise (Not_implemented "GRAPHICS draw_curve3d")
    | GL -> begin
        enter3d (p1, p2);
        match !gl with
        | Some list when not !reset_gllist -> Gl.call_list list
        | _ ->
            (* on stocke les ordres graphiques dans une "display_list" opengl *)
            let list = Gl.gen_lists 1 in
            Gl.new_list list Gl.COMPILE_AND_EXECUTE;
            Gl.gl_begin Gl.line_strip;
            List.iter (fun { Point3.x; y; z } -> vertex3 (x, y, z)) p3d;
            Gl.gl_end ();
            Gl.disable Gl.depth_test;
            Gl.gl_begin Gl.points;
            List.iter (fun { Point3.x; y; z } -> vertex3 (x, y, z)) p3d;
            Gl.gl_end ();
            leave3d ();
            Gl.end_list ();
            gl := Some list
      end
    | FIG ->
        let draw () = draw_curve3d ~dev:GL gl plot_func p3d (p1, p2) in
        gl2fig draw plot_func

  let draw_segments pl ?(dev = !default_device) ?dep view =
    let ps = rescale_list pl view (bounding_box dev) in
    match dev with
    | GRAPHICS ->
        Graphics.draw_segments
          (Array.of_list
             (let rec pair l =
                match l with
                | (x0, y0) :: (x1, y1) :: ll ->
                    ( int_of_float x0,
                      int_of_float y0,
                      int_of_float x1,
                      int_of_float y1 )
                    :: pair ll
                | _ -> []
              in
              pair ps))
    | GL ->
        Gl.gl_begin Gl.lines;
        List.iter (fun (x, y) -> Gl.vertex2f x y) ps;
        Gl.gl_end ()
    | FIG ->
        let depth = get_depth dep and co = fig_of_color !current_color in
        (* à modifier *)
        List.iter
          (fun (x0, y0, x1, y1) ->
            Printf.fprintf !xfig_main_channel
              "2 1 0 %u %d %d %d -1 -1 0.000 0 0 -1 0 0 2\n\t%d %d %d %d\n"
              (* iscale *) 1 co co depth x0 y0 x1 y1)
          (let rec pair l =
             match l with
             | (x0, y0) :: (x1, y1) :: ll ->
                 ( int_of_float x0,
                   int_of_float y0,
                   int_of_float x1,
                   int_of_float y1 )
                 :: pair ll
             | _ -> []
           in
           pair ps)

  let draw_text ?(dev = !default_device) ?dep view t =
    let x0, y0 = draw_of_point t.pos view (bounding_box dev) in
    match dev with
    | GRAPHICS ->
        Graphics.set_text_size (iscale t.size);
        (* la size n'est pas prise en compte sous GRAPHICS ?? *)
        (* en remplacement: *)
        (* or use an "association list" (size,font) *)
        let size = max 6 (min 40 (iscale t.size) land 62) in
        let font_desc =
          Printf.sprintf "-*-fixed-*-r-*-*-%d-*-*-*-*-*-iso8859-*" size
        in
        let () =
          try Graphics.set_font font_desc
          with _ ->
            Debug.print "Cannot find font: %s" font_desc;
            Graphics.set_font "fixed"
        in
        let w, h = Graphics.text_size t.text in
        let dx =
          match t.align with CENTER -> w / 2 | LEFT -> 0 | RIGHT -> w
        in
        Graphics.moveto (int_of_float x0 - dx) (int_of_float y0 - (h / 2));
        Graphics.draw_string t.text
    | GL ->
        let s =
          match t.pix with
          | Some surf when not !force_refresh -> surf
          (* : on ne calcule l'image qu'une fois ! *)
          | _ ->
              let pix = text_image t.text (iscale t.size) t.flag in
              t.pix <- Some pix;
              pix
        in
        let w, h = Sdl.get_surface_size s in
        let dw, dh = draw_of_pixel (w, h) (bounding_box dev) in
        let dx =
          match t.align with CENTER -> dw /. 2. | LEFT -> 0. | RIGHT -> dw
        in
        Gl.Feedback.pass_through text_token;
        draw_image s (x0 -. dx) (y0 -. (dh /. 2.)) ~mode:Gl.modulate
    | FIG ->
        let _, h =
          ( 315,
            int_of_float
              ((match t.flag with Normal -> 8.1 | Latex -> 6.)
              (* à ajuster...*)
              *. float t.size (* |> scale *)) )
        (* 315 a modifier, ca pose pb pour fig2dev (pas xfig)
       lorsqu'il n'y a qu'un texte: la bounding box eps est
       calculée en fonction de ça ! *)
        and depth = get_depth dep
        and co = fig_of_color !current_color in
        let fig_align =
          match t.align with CENTER -> 1 | LEFT -> 0 | RIGHT -> 2
        in
        Printf.fprintf
          !xfig_main_channel (*modifier taille et le flag special ?*)
          "4 %d %d %d -1 %d %f 0.0000 %d 180 315 %d %d %s\\001\n" fig_align co
          depth
          (match t.flag with Normal -> 16 | Latex -> 0)
          (float t.size *. 0.75 (* |> scale *)) (* facteur correctif *)
          (match t.flag with Normal -> 4 | Latex -> 2)
          (int_of_float x0)
          (int_of_float y0 + (h / 2))
          (String.escaped t.text)
  (* conversion des sequences \.. *)

  (* axes (décorés) passant par le point donné *)
  let sign x =
    if x > 0. then 1. else if x < 0. then -1. else raise Division_by_zero

  let is_finite x =
    classify_float x <> FP_infinite && classify_float x <> FP_nan

  let is_nan x = not (is_finite x)

  (* rem: on autorise les axes en dehors de la figure. Ca peut permettre
   de ne voir qu'un axe sur les deux si on veut. Mais pb pour eps ! *)
  let draw_axis a ?(dev = !default_device) view =
    let view_has_changed =
      !force_refresh
      ||
      match a.view with
      | None -> true
      | Some _ ->
          not (a.view = view && a.window_size = (!window_width, !window_height))
    in
    let axis_segments, text_labels =
      if view_has_changed then begin
        (* print_endline "Calculating axis..."; *)
        a.view <- view;
        a.window_size <- (!window_width, !window_height);
        let { x = xa; y = ya } = a.center in
        let { x = x0; y = y0 }, { x = x1; y = y1 } =
          match view with None -> raise View_expected | Some v -> v
        in
        if is_nan x0 || is_nan y0 || is_nan x1 || is_nan y1 then (
          Printf.sprintf
            "ERROR: cannot draw axis with infinite view (%f,%f,%f,%f)" x0 y0 x1
            y1
          |> print_endline;
          ([], []))
        else
          (* calcul des unités des axes: (à améliorer) *)
          let mymodf x =
            let m = floor x +. 1. in
            (x -. m, m)
          in
          let myround x =
            let r, m = mymodf x and q = log 2. /. log 10. in
            if -.r < q then m else m -. q
          and minxunit, minyunit = point_of_pixel (iscale 18, iscale 18) view in
          let xunit =
            sign minxunit *. (10. ** myround (log10 (abs_float minxunit)))
          (* anciennement 10**(floor (log10 (x1 -. x0)) -. 1.) *)
          and yunit =
            sign minyunit *. (10. ** myround (log10 (abs_float minyunit)))
          (* taille des "ticks" *)
          and vtick, htick = point_of_pixel (iscale 4, iscale 4) view in
          let l_axes =
            [
              { x = x0; y = ya };
              { x = x1; y = ya };
              { x = xa; y = y0 };
              { x = xa; y = y1 };
            ]
          and l_hticks =
            let rec ht i =
              let xi = i *. xunit in
              if abs_float (xi -. x0) > abs_float (x1 -. x0 -. vtick) then []
              else
                { x = xi; y = ya -. htick }
                :: { x = xi; y = ya +. htick }
                :: ht (i +. 1.)
            in
            ht (floor (x0 /. xunit) +. 1.)
          and l_vticks =
            let rec vt i =
              let yi = i *. yunit in
              if abs_float (yi -. y0) > abs_float (y1 -. y0 -. htick) then []
              else
                { x = xa -. vtick; y = yi }
                :: { x = xa +. vtick; y = yi }
                :: vt (i +. 1.)
            in
            vt (floor (y0 /. yunit) +. 1.)
          and l_harrow, l_varrow =
            (* flèches aux extrêmités maximales *)
            let xmax, ymax = (fmax x0 x1, fmax y0 y1)
            and ahtick, avtick = (abs_float htick, abs_float vtick) in
            ( [
                { x = xmax -. avtick; y = ya -. ahtick };
                { x = xmax; y = ya };
                { x = xmax -. avtick; y = ya +. ahtick };
                { x = xmax; y = ya };
              ],
              [
                { x = xa -. avtick; y = ymax -. ahtick };
                { x = xa; y = ymax };
                { x = xa +. avtick; y = ymax -. ahtick };
                { x = xa; y = ymax };
              ] )
          in
          (* draw_segments (List.concat [ l_vticks ; l_hticks ; l_axes ; l_harrow ; l_varrow ]) view ~dev; *)
          (* chiffres *)
          let xtunit =
            sign minxunit *. (10. ** myround (log10 (1.7 *. abs_float minxunit)))
          and ytunit =
            sign minyunit *. (10. ** myround (log10 (1.2 *. abs_float minyunit)))
          in
          let xpos, xalign =
            if x0 < xa -. (4. *. vtick) then (xa -. (1.5 *. vtick), RIGHT)
            else (xa +. (1.5 *. vtick), LEFT (* ne marche que si x0<x1 *))
          and ypos =
            if y0 < ya -. (3. *. htick) then ya -. (2.5 *. htick)
            else ya +. (2.5 *. htick (* ne marche que si y0<y1 *))
          in
          let l_hnum =
            let rec hn i =
              let xi = i *. xtunit in
              if abs_float (xi -. x0) >= abs_float (x1 -. x0) then []
              else
                {
                  pos = { x = xi; y = ypos };
                  text = Printf.sprintf "%g" xi;
                  size = 10;
                  align = CENTER;
                  flag = Normal;
                  pix = None;
                }
                :: hn (i +. 1.)
            in
            hn (floor (x0 /. xtunit) +. 1.)
          and l_vnum =
            let rec vn i =
              let yi = i *. ytunit in
              if abs_float (yi -. y0) >= abs_float (y1 -. y0) then []
              else
                {
                  pos = { x = xpos; y = yi };
                  text = Printf.sprintf "%g" yi;
                  size = 10;
                  align = xalign;
                  flag = Normal;
                  pix = None;
                }
                :: vn (i +. 1.)
            in
            vn (floor (y0 /. ytunit) +. 1.)
          in
          let t =
            ( List.concat [ l_vticks; l_hticks; l_axes; l_harrow; l_varrow ],
              (* = axis_segments *)
              List.concat [ l_hnum; l_vnum ] )
            (* = text_labels *)
          in
          a.ticks <- Some t;
          t
      end
      else match a.ticks with None -> raise View_expected | Some t -> t
    in
    draw_segments axis_segments view ~dev;
    incr counter;
    List.iter (draw_text view ~dev) text_labels
  (*   List.iter (draw_text view ~dev) l_vnum;;*)

  (********************************************************************)

  let line_width x = User (fun _ dev -> set_line_width ~dev (!gl_scale *. x))

  (********************************************************************)

  let window_flush ?(dev = !default_device) () =
    match dev with
    | GRAPHICS -> Graphics.synchronize ()
    | GL -> (*Gl.finish () ? *) do_option !win Sdl.gl_swap_window
    | FIG -> raise (Not_implemented "FIG flush")
  (* flush buffer *)

  (* modifie la position 3d. Rotation autour de l'axe y puis axe x *)
  let rotate3d ax ay =
    let ry = Geom.q_rotation 0. 1. 0. (-.ay)
    and rx = Geom.q_rotation 0. 0. 1. (-.ax) in
    position3d := Geom.q_mult ry (Geom.q_mult rx !position3d)

  let mincr z = z := !z *. 1.01
  let mdecr z = z := !z *. 0.99

  let gl_zoom_in () =
    mincr zoom3d;
    mincr zoom3d

  let gl_zoom_out () =
    mdecr zoom3d;
    mdecr zoom3d

  let gl_mouse_motion x y =
    let dt = 0.01 /. !gl_scale in
    let dX = dt *. float (x - !mouse_x) and dY = dt *. float (y - !mouse_y) in
    rotate3d dY dX;
    mouse_x := x;
    mouse_y := y

  let print_help () =
    print_endline
    @@ Printf.sprintf
         "\n\
          ---------------:-------Oplot help---------\n\
          'h' or '?'     : this help message.\n\n\
          'q' or ESC     : quit\n\n\
          arrows         : rotate the 2D scene\n\
          '='            : 2D zoom out\n\
          SHIFT '='      : 2D zoom in\n\
          TAB            : reset 2D position\n\n\
          CTRL arrows:   : rotate the 3D scene\n\
          CTRL       '=' : 3D zoom out\n\
          CTRL SHIFT '=' : 3D zoom in\n\
          CTRL TAB       : reset 3D position\n\
          CTRL 'l'       : toggle 3D lighting\n\n\
          CTRL 'f'       : toggle fullscreen (may change screen resolution)\n\
          CTRL 's'       : save screenshot (default file = \"%s\")\n\n\
          CTRL 'z'       : suspend (for debugging)\n\
          ---------------:---------------------------\n"
         Sysinit.png_output

  (* gestion du clavier sous SDL. Retourne true si on doit quitter
   (escape ou q) *)
  let sdl_key key =
    let quit = ref false in
    let modifier = Sdl.Event.(get key keyboard_keymod) in
    (match Sdl.Event.(get key keyboard_keycode) with
    | k when k = Sdl.K.question || k = Sdl.K.h -> print_help ()
    | k when k = Sdl.K.left && modifier land Sdl.Kmod.ctrl <> 0 ->
        rotate3d 0. (-0.02)
    | k when k = Sdl.K.left -> gl_rotated2d 1.
    | k when k = Sdl.K.right && modifier land Sdl.Kmod.ctrl <> 0 ->
        rotate3d 0. 0.02
    | k when k = Sdl.K.right -> gl_rotated2d (-1.)
    | k when k = Sdl.K.up && modifier land Sdl.Kmod.ctrl <> 0 ->
        rotate3d (-0.02) 0.
    | k when k = Sdl.K.up -> Gl.rotated (-1.) 1. 0. 0.
    | k when k = Sdl.K.down && modifier land Sdl.Kmod.ctrl <> 0 ->
        rotate3d 0.02 0.
    | k when k = Sdl.K.down -> Gl.rotated 1. 1. 0. 0.
    | k
      when k = Sdl.K.equals
           && modifier land Sdl.Kmod.shift <> 0
           && modifier land Sdl.Kmod.ctrl <> 0 ->
        mincr zoom3d
    | k when k = Sdl.K.equals && modifier land Sdl.Kmod.shift <> 0 ->
        Gl.scalef 1.1 1.1 1.1
    | k when k = Sdl.K.plus -> Gl.scalef 1.1 1.1 1.1
    (* utiliser plutot la caméra?*)
    | k when k = Sdl.K.equals && modifier land Sdl.Kmod.ctrl <> 0 ->
        mdecr zoom3d
    | k when k = Sdl.K.equals -> Gl.scalef 0.91 0.91 0.91
    | k when k = Sdl.K.tab && modifier land Sdl.Kmod.ctrl <> 0 ->
        position3d := default_position3d;
        zoom3d := default_zoom3d
    | k when k = Sdl.K.tab ->
        Gl.pop_matrix ();
        Gl.push_matrix ();
        reset_time ()
    | k when k = Sdl.K.l && modifier land Sdl.Kmod.ctrl <> 0 ->
        light_on := not !light_on;
        reset_gllist := true;
        Debug.print "Light = %b" !light_on
    (* GlLight.material ~face:`front (`emission (c.r/.10., c.g/.10., c.b/.10., 1.)) *)
    | k when k = Sdl.K.f && modifier land Sdl.Kmod.ctrl <> 0 ->
        toggle_fullscreen ()
    | k when k = Sdl.K.p -> decr pause_pass
    (* à améliorer, ex stack *)
    | k when k = Sdl.K.escape || k = Sdl.K.q ->
        close ();
        quit := true
    | k when k = Sdl.K.z && modifier land Sdl.Kmod.ctrl <> 0 ->
        Debug.print "Suspended";
        (* for debug only *)
        quit := true
    | k when k = Sdl.K.s && modifier land Sdl.Kmod.ctrl <> 0 ->
        sdl_screenshot ()
    | _
      when Sdl.get_mod_state () = 0
           (* on reste en pause en cas d'appui sur un
            modificateur (control, shift, etc.) *)
      ->
        resume_pause := true
    | _ -> ());
    !quit

  let sdl_mouse_close () =
    (* print_endline "up button"; *)
    Sdl.set_event_state Sdl.Event.mouse_motion Sdl.disable;
    Sdl.set_event_state Sdl.Event.mouse_button_up Sdl.disable;
    ()

  let sdl_mouse_action _mouse =
    (* sdl_mouse_update mouse; *)
    let but, (x, y) = Sdl.get_mouse_state () in
    if but = Sdl.Button.lmask then gl_mouse_motion x (!window_height - y)
    else sdl_mouse_close ()

  let sdl_mouse_init mouse =
    (* print_endline "down_button"; *)
    if Sdl.Event.(get mouse mouse_button_button) = Sdl.Button.left then begin
      Sdl.set_event_state Sdl.Event.mouse_motion Sdl.enable;
      Sdl.set_event_state Sdl.Event.mouse_button_up Sdl.enable;
      let x, y =
        Sdl.Event.(get mouse mouse_button_x, get mouse mouse_button_y)
      in
      mouse_x := x;
      mouse_y := !window_height - y
    end

  let sdl_mouse_wheel mouse =
    if Sdl.Event.(get mouse mouse_wheel_y) > 0 then gl_zoom_in ()
      (* begin
       sdl_mouse_update (Sdlevent.MOUSEBUTTONUP mouse);
       mincr zoom3d; mincr zoom3d
       end *)
    else if Sdl.Event.(get mouse mouse_wheel_y) < 0 then gl_zoom_out ()
  (* begin
   sdl_mouse_update (Sdlevent.MOUSEBUTTONUP mouse);
   mdecr zoom3d; mdecr zoom3d
   end *)

  let sdl_resize w h =
    (* OS pixels *)
    (* We convert to hardware pixels *)
    let hx = round (float h *. !dpi_scale) in
    let wx = round (float w *. !dpi_scale) in
    resize_window wx hx;
    do_option !win (fun win ->
        let rw, rh = Sdl.gl_get_drawable_size win in
        if (rw, rh) <> (wx, hx) then (
          Debug.print "Obtained: (%i,%i), wanted: (%i,%i). Forcing resize." rw
            rh wx hx;
          Sdl.set_window_size win ~w ~h));
    (* WARNING this triggers events ?*)
    gl_resize ()

  let sdl_event eo =
    (* retourne true si on doit quitter (faire une exception ?) *)
    let e = match eo with Some e -> e | None -> Sdl.Event.create () in
    let rec loop eo =
      if eo <> None || Sdl.poll_event (Some e) then
        let quit = ref false in
        let () =
          match Sdl.Event.(enum (get e typ)) with
          | `Mouse_button_down -> sdl_mouse_init e
          | `Mouse_motion -> sdl_mouse_action e
          | `Mouse_button_up -> sdl_mouse_close ()
          | `Mouse_wheel -> sdl_mouse_wheel e
          | `Key_down -> quit := sdl_key e
          | `Window_event -> begin
              match Sdl.Event.(window_event_enum (get e window_event_id)) with
              | `Size_changed ->
                  let w, h =
                    Sdl.Event.(get e window_data1, get e window_data2)
                  in
                  sdl_resize (Int32.to_int w) (Int32.to_int h)
              | `Close ->
                  close ();
                  quit := true
              | _ -> ()
            end
          | _ -> ()
        in
        !quit || loop None
      else false
    in
    loop eo

  (* Pause sans rafraichissement de la boucle display. t en milisecondes. t=0 pour
   infini. Une touche pour sortir dans tous les cas *)
  let sdl_freeze t =
    let init_time = time () in
    let e = Sdl.Event.create () in
    let has_event = ref false in
    Sdl.set_event_state Sdl.Event.window_event Sdl.disable;
    do_option !win Sdl.gl_swap_window;
    while (not !has_event) && (t = 0 || time () - init_time < t) do
      has_event := Sdl.poll_event (Some e);
      Sdl.delay (Int32.of_int !frame_length)
    done;
    time_delay := !time_delay + time () - init_time;
    Sdl.set_event_state Sdl.Event.window_event Sdl.enable;
    if !has_event && Sdl.Event.(get e typ) = Sdl.Event.key_down then
      match Sdl.Event.(get e keyboard_keycode) with
      | k when k = Sdl.K.escape || k = Sdl.K.q -> Sdl.push_event e |> ignore
      (* ou le faire pour tous ? *)
      (* =l'utilisateur veut sortir *)
      | _ -> ()

  let do_freeze ?(dev = !default_device) t =
    match dev with
    | GRAPHICS ->
        Graphics.synchronize ();
        if t = 0 then ignore (Graphics.read_key ())
        else ignore (Unix.select [] [] [] (float t /. 1000.))
    | GL -> (
        if !counter <= !pause_pass then ()
        else
          match !default_gl with
          | SDL ->
              sdl_freeze t;
              pause_pass := !counter
          | GLUT -> Iglut.freeze t
          | GTK -> () (* ??? TODO manage time delay *))
    | FIG -> ()

  (* ajouter warning *)

  let do_pause ?(dev = !default_device) t =
    match dev with
    | GRAPHICS ->
        Graphics.synchronize ();
        if t = 0 then ignore (Graphics.read_key ())
        else ignore (Unix.select [] [] [] (float t /. 1000.))
    | GL -> (
        if !counter <= !pause_pass then ()
        else
          match !pause_time with
          | None ->
              pause_time := Some (time ());
              do_not_draw := true
          | Some pt -> (
              match !resume_pause || (t != 0 && time () - pt >= t) with
              | false -> do_not_draw := true
              | true ->
                  resume_pause := false;
                  do_not_draw := false;
                  pause_pass := !counter;
                  pause_time := None))
    | FIG -> ()

  (* ajouter warning *)

  let clear_sheet ?(dev = !default_device) c =
    (* à modifier pour n'effacer que la view *)
    match dev with
    | GRAPHICS -> Graphics.clear_graph ()
    (* la couleur est ignorée *)
    | GL ->
        gl_clear_color c;
        Gl3.clear Gl3.color
    | FIG -> raise (Not_implemented "FIG clear")

  let exec_user f view dev =
    let v =
      match view with
      | None ->
          Debug.print "User object could not find a view; using a default";
          default_view
      | Some v -> v
    in
    f v dev

  (* trace un objet (mais pas sheet)*)
  let rec object_plot ?(addcounter = true) ~dev po view =
    if addcounter then incr counter;
    match po with
    | Points pl -> draw_points pl view ~dev
    | Lines pl -> List.iter (fun l -> draw_lines l view ~dev) pl
    | Poly pl -> draw_poly pl view ~dev
    | Axis a -> draw_axis a view ~dev
    | Color c -> set_color c ~dev
    | Text t -> draw_text view t ~dev
    | Matrix m -> draw_matrix m ~dev
    | Grid ((m, v3, w), gl) ->
        let v3 = initialize_view3 v3 in
        draw_grid ~wire:w gl (object_plot ~addcounter:false) m v3 ~dev
    (* pas vraiment besoin de passer !current_view3d partout *)
    | Surf3d ((fx, fy, fz, v3, w), gl) ->
        let v3 = initialize_view3 v3 in
        draw_surf3d ~wire:w gl (object_plot ~addcounter:false) fx fy fz v3 ~dev
    | Curve3d ((p3d, v3), gl) ->
        let v3 = initialize_view3 v3 in
        draw_curve3d gl (object_plot ~addcounter:false) p3d v3 ~dev
    | Move3d m -> move3d m
    | Adapt (vo, f) ->
        let obj =
          match (!vo, view) with
          | (Some w, Some o), Some v when w = v -> o
          | _ ->
              let o = f view in
              vo := (view, Some o);
              o
        in
        object_plot ~dev obj view
    | Pause t -> do_pause t ~dev
    | Freeze t -> do_freeze t ~dev
    | Clear c -> clear_sheet c ~dev
    | View _ -> ()
    | Anim f ->
        let p = f (float (elapsed ()) /. 1000.) in
        object_plot p view ~dev
    | User f -> exec_user f view dev
    | Sheet _ ->
        raise (Invalid_argument "object_plot cannot accept Sheet argument")

  (* use t as a realtime parameter, in seconds *)
  let _anim_plot_old f ?step ?(t0 = 0.) ?(t1 = 0.) x0 x1 =
    let userfu v dev =
      let t =
        if t1 = 0. then t0 +. (float (elapsed ()) /. 1000.)
        else fmin t1 (t0 +. (float (elapsed ()) /. 1000.))
      in
      let p = plot (f t) ?step x0 x1 in
      object_plot p (Some v) ~dev
    in
    User userfu

  (* use t as a realtime parameter, in seconds *)
  let anim_plot f ?step ?(t0 = 0.) ?(t1 = 0.) x0 x1 =
    let animfu time =
      let t =
        if t1 = 0. then t0 +. time else fmin t1 (t0 +. time)
        (* TODO memoize the plot when t1 is reached *)
      in
      plot (f t) ?step x0 x1
    in
    Anim animfu

  let repeat = Anim (fun _ -> Points [])

  (* ne marche pas bien... refaire les pause *)
  let gl_zoom_out t pop =
    let first_time = ref None in
    let foo _ _ =
      match !first_time with
      | None ->
          first_time := Some (time ());
          Debug.print "Initialisation"
      | Some t0 when time () < t0 + t ->
          Gl.scalef 0.91 0.91 0.;
          do_pause 0
      | _ ->
          first_time := None;
          resume_pause := true;
          if pop then (
            Gl.pop_matrix ();
            Gl.push_matrix ())
    in
    foo

  let gl_zoom_in t pop =
    let first_time = ref None in
    let foo _ _ =
      match !first_time with
      | None ->
          first_time := Some (time ());
          Debug.print "Initialisation"
      | Some t0 when time () < t0 + t ->
          Gl.scalef 1.1 1.1 0.;
          do_pause 0
      | _ ->
          first_time := None;
          resume_pause := true;
          if pop then (
            Gl.pop_matrix ();
            Gl.push_matrix ())
    in
    foo

  let cleanup dev = set_line_width ~dev !gl_scale
  (* ETC. TBC**)

  (* tracé de tous les objets (un peu merdique). On commence par traiter
   les premiers ordres s'ils ne nécessitent pas de view. Puis, on
   regarde si on atteint un view, auquel cas on le récupère, ou autre
   chose, auquel cas on initialise le view comme le maxview du premier
   objet. Lorsqu'une sous-feuille est rencontrée le processus
   recommence au début: une *nouvelle* view est initialisée. Lorsque
   la sous-feuille est terminée on retourne à la vue qu'on avait
   laissée...
*)
  let rec draw ~dev sh view =
    if !do_not_draw then ()
    else
      match view with
      | Some _ -> (
          match sh with
          | Sheet [] -> cleanup dev
          | Sheet (po :: ssh) -> (
              match po with
              | View vv -> draw (Sheet ssh) vv ~dev
              | Sheet sssh ->
                  draw (Sheet sssh) None ~dev;
                  (* (on réinitialise la vue) *)
                  draw (Sheet ssh) view ~dev
              | _ ->
                  draw po view ~dev;
                  draw (Sheet ssh) view ~dev)
          | po -> object_plot po view ~dev)
      | None -> (
          match sh with
          | Sheet [] -> ()
          | Sheet (po :: ssh) ->
              let v = maxview po in
              draw po v ~dev;
              draw (Sheet ssh) v ~dev
          | po ->
              let v = maxview po in
              object_plot po v ~dev)
  (* (oui je sais c'est un peu trop compliqué) *)

  let draw ?(dev = !default_device) sh view =
    draw ~dev sh view;
    force_refresh := false;
    reset_gllist := false

  let force_refresh () = force_refresh := true

  (*********** interfaces graphiques *************)

  let graphics_resize () =
    resize_window (Graphics.size_x ()) (Graphics.size_y ())

  (* initialisation d'une fenêtre graphique par le module Graphics *)
  let graphics_init () =
    scale_window ();
    Graphics.open_graph (Printf.sprintf " %dx%d" !window_width !window_height);
    Graphics.set_window_title "Oplot - Graphics Window";
    Graphics.auto_synchronize false

  (* réagit en fonctionde la touche key. retourne vrai si on doit
   quitter *)
  let graphics_key key =
    match key with
    | '\027' ->
        Graphics.close_graph ();
        true
    | _ -> false

  let graphics_event () =
    let status = Graphics.(wait_next_event [ Key_pressed; Button_down ]) in
    if status.Graphics.keypressed then graphics_key status.Graphics.key
    else if status.Graphics.button then (
      graphics_resize ();
      false)
    else false

  (* boucle principale interactive Graphics pour afficher la feuille sh *)
  let rec graphics_mainloop sh =
    let r, g, b = int_of_color default_color in
    Graphics.set_color (Graphics.rgb r g b);
    (* ne pas mettre ici ? *)
    current_color := default_color;
    Graphics.clear_graph ();
    (* ne pas mettre ici ? *)
    draw sh None ~dev:GRAPHICS;
    Graphics.synchronize ();
    if graphics_event () then () else graphics_mainloop sh

  (* boucle principale interactive SDL/GL pour afficher la feuille sh *)
  let unixtime =
    let start = Unix.gettimeofday () in
    fun () -> Unix.gettimeofday () -. start

  let frames = ref 0
  let ot = ref (int_of_float (unixtime ()))

  (* Apparemment Sdl.wait_event a un certain nombre de bugs selon les versions...,
   donc on refait. Attention, push_event n'est pas *immédiatement* détectable
   par poll_event. *)
  let wait_event =
    let e = Sdl.Event.create () in
    fun () ->
      let rec loop () =
        if Sdl.poll_event (Some e) then e
        else (
          Sdl.delay 10l;
          loop ())
      in
      loop ()

  let rec sdl_mainloop sh wait =
    incr frames;
    let t = int_of_float (unixtime ()) in
    if t <> !ot then begin
      Debug.print "%d fps%!" !frames;
      frames := 0;
      ot := t
    end;
    Gl3.clear (Gl3.color_buffer_bit lor Gl3.depth_buffer_bit);
    gl_draw_color default_color;
    current_color := default_color;
    counter := 0;
    start_time := time ();
    draw sh None ~dev:GL;
    do_option !win Sdl.gl_swap_window;
    let elapsed_time = time () - !start_time in
    if elapsed_time < !frame_length then
      Sdl.delay (Int32.of_int (!frame_length - elapsed_time))
    else Sdl.delay 10l;
    (* on laisse respirer le CPU... *)
    let e =
      if wait && (!pause_pass = 0 || (!do_not_draw && !pause_pass != 0)) then
        Some (wait_event ())
        (* on bloque s'il n'y a pas eu de pauses, ou si les pauses ne sont pas
               finies *)
      else None
    in
    if !do_not_draw then do_not_draw := false else pause_pass := 0;
    (* ca repart au debut *)
    if sdl_event e || !interrupt_request then () else sdl_mainloop sh wait

  (* boucle principale interactive GTK/GL pour afficher la feuille sh
   (c'est GTK qui s'occupe de boucler). On retourne le temps mis pour tracer *)
  let gtk_mainloop sh =
    Gl3.clear (Gl3.color_buffer_bit lor Gl3.depth_buffer_bit);
    gl_draw_color default_color;
    current_color := default_color;
    reset_view3 ();
    counter := 0;
    start_time := time ();
    draw sh None ~dev:GL;
    Gl.flush ();
    time () - !start_time

  (* initialisation de la sortie FIG *)
  (* http://linux.math.tifr.res.in/manuals/html/xfig/fig-format.html *)
  let xfig_init () =
    init_fig_colors ();
    xfig_main_channel := open_out (Printf.sprintf "%s.main" xfig_output_tmp);
    xfig_head_channel := open_out (Printf.sprintf "%s.head" xfig_output_tmp);
    output_string !xfig_head_channel
      "#FIG 3.2  Produced by oplot.ml, Vu Ngoc San\n";
    output_string !xfig_head_channel
      "Portrait\nCenter\nMetric\nA4\n100.00\nSingle\n-2\n1200 2\n"

  (* couleurs xfig 32-543 *)
  let write_fig_color c num =
    let r = rgb_of_color c in
    Printf.fprintf !xfig_main_channel "# User color :\n0 %d #%x\n" num r;
    fig_colors.(num) <- r;
    print_int r

  (* on parcours la feuille à la recherche des couleurs nouvelles... car
   elles doivent figurer en tête du fichier fig, ou faire un fichier a
   part et merger ensuite - oui !*)
  let rec fig_first_pass sh num =
    if sh = Sheet [] then ()
    else if num > 543 then raise Fig_Too_Many_Colors
    else
      let next_sh, next_num =
        match sh with
        | Sheet (Color c :: ssh) when fig_of_color c = -1 ->
            write_fig_color c num;
            (ssh, num + 1)
        | Sheet (_ :: ssh) -> (ssh, num)
        | Color c when fig_of_color c = -1 ->
            write_fig_color c num;
            ([], num + 1)
        | _ -> ([], num)
      in
      fig_first_pass (Sheet next_sh) next_num

  (* fn principale (non interactive) FIG  pour afficher la feuille sh *)
  let xfig_mainloop sh =
    (* fig_first_pass sh 32; *)
    set_color default_color ~dev:FIG;
    fig_color_counter := 32;
    draw sh None ~dev:FIG;
    close_out !xfig_main_channel;
    close_out !xfig_head_channel;
    shell "cat %s.head %s.main > %s"
      (* a modifier pour portabilité *)
      xfig_output_tmp xfig_output_tmp xfig_output_tmp;
    shell "rm %s.head %s.main" xfig_output_tmp xfig_output_tmp

  let rec sh_has_latex sh =
    match sh with
    | Sheet [] -> false
    | Text t when t.flag = Latex -> true
    | Sheet (po :: ssh) -> sh_has_latex po || sh_has_latex (Sheet ssh)
    | _ -> false

  (* Transforme le fichier fig en eps/pdf. utiliser psmerge pour plusieurs pages /
   pauses?  Mais incompatible xfig ?? (ou utiliser les depths ?) *)
  let write_eps ?output ?(pdf = true) sh =
    let convert = if pdf then fig2pdf else fig2eps in
    let output =
      match output with
      | Some s -> s
      | None -> if pdf then pdf_output else eps_output
    in
    if sh_has_latex sh then (
      shell "%s --input=%s %s" convert latex_header xfig_output_tmp;
      shell "cp %s.%s %s"
        (Filename.remove_extension xfig_output_tmp)
        (if pdf then "pdf" else "eps")
        output;
      (* attention xfig ecrit les caractères non ascii sous la forme \xxx; il
       faudrait faire une première passe pour les transformer. ocaml devrait
       faire ça tout seul avec print_string *)
      if not pdf then begin
        (*il faut encore enlever la ligne papersize pour être sûr que
        gv cherche la BBox. De toutes façons on peut le forcer avec
        --media=BBox *)
        shell "mv -f %s %s.tmp.ps" output output;
        shell "grep -v \"%%DocumentPaperSizes:\" %s.tmp.ps > %s" output output;
        shell "rm %s.tmp.ps" output
      end)
    else
      shell "fig2dev -L %s -F %s %s"
        (if pdf then "pdf" else "eps")
        xfig_output_tmp output;
    Printf.sprintf "Output file: %s" output |> print_endline

  (* ne traite pas les pauses pour le moment... à intégrer à mainloop ?
*)
  let write_bmp ?(output = png_output) sh =
    gl_init ~show:false ();
    Gl3.clear Gl3.color_buffer_bit;
    gl_draw_color default_color;
    (*Gl.finish ();*)
    current_color := default_color;
    counter := 0;
    draw sh None ~dev:GL;
    window_flush () ~dev:GL;
    (* copy_back_buffer (); *)
    sdl_screenshot ~output ();
    close () ~dev:GL

  (*** (pour l'interactif utiliser plutot display): fonction principale pour
     l'affichage de la feuille. Sortir par ESC. Pour GL/GLUT on peut aussi sortir
     par ESC mais ça tue aussi ocaml ! ***)
  let disp ?(dev = !default_device) ?(fscreen = false) sh =
    pause_init ();
    reset_view3 ();
    (* réinitialiser aussi les variables globales... *)
    match dev with
    | GRAPHICS ->
        graphics_init ();
        graphics_resize ();
        graphics_mainloop sh
    | GL -> (
        gl_init ();
        let wait = not (has_anim sh) in
        (* TODO:  `line_smooth n'a pas d'effet pour GTK?: *)
        if wait then (
          Gl3.enable Gl3.line_smooth;
          Gl3.hint Gl3.line_smooth Gl3.nicest)
        else Gl3.hint Gl3.line_smooth Gl3.fastest;
        resume_pause := false;
        pause_pass := 0;
        interrupt_request := false;
        match !default_gl with
        | GLUT ->
            if fscreen then Iglut.fullscreen ();
            Iglut.mainloop sh
        | SDL ->
            if fscreen <> !fullscreen then toggle_fullscreen ();
            reset_time ();
            Gl3.clear (Gl3.color_buffer_bit lor Gl3.depth_buffer_bit);
            do_option !win Sdl.gl_swap_window;
            sdl_mouse_close ();
            sdl_mainloop sh wait
        | GTK ->
            (* ne pas utiliser comme ca...*)
            reset_time ();
            (* let wait  = not (has_anim sh) in *)
            ignore (gtk_mainloop sh))
    | FIG ->
        xfig_init ();
        (* faire un try pour refermer le fichier en cas d'erreur *)
        xfig_mainloop sh

  let get_gl_scale () = !gl_scale
  let set_gl_scale s = gl_scale := s

  (* raccourci pour concatener deux listes *)
  let ( & ) a b = List.append a b

  (* interactif: wrapper/raccourci pour tracer une liste d'objets *)
  let display ?(dev = !default_user_device) ?(fscreen = false) ?output sh =
    match dev with
    | GRAPHICS_d -> disp (Sheet sh) ~dev:GRAPHICS
    | GL_d -> disp (Sheet sh) ~dev:GL ~fscreen
    | FIG_d ->
        disp (Sheet sh) ~dev:FIG;
        let output = default output fig_output in
        shell "cp %s %s" xfig_output_tmp output;
        Printf.sprintf "Output file: %s" output |> print_endline
    | EPS_d ->
        disp (Sheet sh) ~dev:FIG;
        write_eps ?output ~pdf:false (Sheet sh)
    | PDF_d ->
        disp (Sheet sh) ~dev:FIG;
        write_eps ?output ~pdf:true (Sheet sh)
    | XFIG_d ->
        disp (Sheet sh) ~dev:FIG;
        shell "xfig -correct_font_size -zoom 1 %s &" xfig_output_tmp
    | GV_d -> (
        disp (Sheet sh) ~dev:FIG;
        write_eps ~pdf:false ~output:eps_output_tmp (Sheet sh);
        match psviewer with
        | Some "gv" -> shell "gv --media=BBox --watch %s &" eps_output_tmp
        | Some "kghostview" -> shell "kghostview --portrait %s &" eps_output_tmp
        | Some prog -> shell "%s %s &" prog eps_output_tmp
        | None -> print_endline "No postscript viewer found.")
    | BMP_d ->
        print_endline "Using PNG instead of BMP.";
        write_bmp ?output (Sheet sh)
    | PNG_d -> write_bmp ?output (Sheet sh)
    | IMG_d -> (
        write_bmp (Sheet sh);
        match viewer with
        | Some prog -> shell "%s %s &" prog png_output
        | None -> print_endline "No image viewer found"
        (*  mettre debug *))

  (* inutilisé *)
  let display_eps sh =
    disp (Sheet sh) ~dev:FIG;
    write_eps (Sheet sh);
    shell "gv --watch --scalebase=2 %s&" eps_output_tmp

  (**************************)
  let interrupt () =
    interrupt_request := true;
    (* On simule une touche inoffensive (a) pour passer le Sdlevent.wait ().
     evidemment seulement pour sdl *)
    if !window_open then begin
      let e = Sdl.Event.create () in
      Sdl.Event.(set e typ key_down);
      Sdl.Event.(set e keyboard_state Sdl.pressed);
      Sdl.Event.(set e keyboard_keycode Sdl.K.a);
      Sdl.push_event e |> ignore
    end

  let interruption int =
    prerr_endline (Printf.sprintf "\nInterruption:%d\n" int);
    flush stderr;
    (* for debug only *)
    if !window_open then begin
      let e = Sdl.Event.create () in
      Sdl.Event.(set e typ key_down);
      Sdl.Event.(set e keyboard_state Sdl.pressed);
      Sdl.Event.(set e keyboard_keycode Sdl.K.z);
      Sdl.Event.(set e keyboard_keymod Sdl.Kmod.ctrl);
      Sdl.push_event e |> ignore
    end
    else prerr_endline "Nothing";
    flush stderr;
    (* for debug only *)
    if int = Sys.sigint then (
      remove_tmp_dir ();
      exit 0)

  let () =
    Sys.set_signal Sys.sigusr1 (Sys.Signal_handle interruption);
    Sys.set_signal Sys.sigint (Sys.Signal_handle interruption)
end
(* of module Make *)
(************************************************************************)

(*
 Local Variables:
 compile-command:"cd ..; dune build"
 End:
*)

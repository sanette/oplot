Debug.print "* Loading oplotmain"

(*** à faire: ca serait bien d'utiliser la bibliothèque gl2ps
       pour convertir directement en ps
       http://www.geuz.org/gl2ps/
       mais il faut écrire l'interface ocaml!
  ***)

(* utiliser pdflatex ? (cf le module ~/.inkscape/extensions/textext.py)
*)

module Make (Graphics : Make_graphics.GRAPHICS) = struct
  open Tsdl
  open Common
  open Points
  open Point2
  open Oplotdef
  open Sysinit
  open Renderinit

  let go = Debug.go
  let do_option o f = match o with Some x -> f x | None -> ()
  let force_refresh = ref false
  let xfig_scale = 45.
  (* Un point xfig vaut 1/80 inch. Mais attention "When exporting to EPS,
     PostScript or any bitmap format (e.g. GIF), the line thickness is reduced to
     1/160 inch (0.159mm) to "lighten" the look." *)

  (* (bx0,by0, bx1,by1) : coordonnées logiques (et non physiques) de la fenêtre *)
  let bounding_box dev =
    match dev with
    | X11 -> (1., 1., !fwindow_width, !fwindow_height)
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
      else print_endline "Already scaled."

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

  let sdl_init ~show () =
    let crucial () =
      if Sdl.Init.test (Sdl.was_init None) Sdl.Init.video then
        Debug.print "Using existing SDL context." (* Sdl.quit () *)
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
    GlDraw.viewport ~x:0 ~y:0
      ~w:(!window_width + !left_margin + !right_margin)
      ~h:(!window_height + !top_margin + !bottom_margin);
    GlMat.mode `projection;
    GlMat.load_identity ();
    let bb = bounding_box GL in
    let dxl, dyb = draw_of_pixel (!left_margin, !bottom_margin) bb
    and dxr, dyt = draw_of_pixel (!right_margin, !top_margin) bb in
    GlMat.ortho ~x:(-.dxl, 1. +. dxr) ~y:(-.dyb, 1. +. dyt) ~z:(-2., 2.);
    GlMat.mode `modelview

  let gl_init ?(show = true) () =
    (match !default_gl with
    | GLUT -> Iglut.init ()
    | SDL -> sdl_init ~show ()
    | GTK -> gtk_init ());
    Debug.print "GL inits...";
    GlClear.color (float_of_color !default_bg_color);
    GlClear.clear [ `depth ];

    (*  if !multisampling then Gl.enable `multisample else *)

    (* Gl.enable `line_smooth;*)
    Gl.disable `polygon_smooth;
    (* sinon on voit les triangulations *)
    GlMisc.hint `line_smooth `fastest;
    Gl.enable `blend;

    (* Gl.enable `point_smooth; *)
    (*peut causer d'énormes ralentissements sur certaines implémentations !! *)
    GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;
    GlDraw.line_width !gl_scale;
    GlDraw.point_size !gl_scale;
    Gl.enable `polygon_offset_fill;
    GlDraw.polygon_offset ~factor:1. ~units:1.;
    gl_resize ();

    (*  GlMat.frustum ~x:(-1. , 1.) ~y:(-. 600. /. 800. , 600. /. 800.) ~z:(2. , 6.); *)

    (* l'aspect est réglé sur la taille par défaut 800x600 *)
    (* GluMat.perspective ~fovy:60. ~aspect:(800./.600.) ~z:(100.,-.150.); *)
    (* ??? *)
    (* en cas de frustum:  GlMat.translate ~z:(-4.) (); *)
    GlMat.push ();
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
    | X11 -> Graphics.close_graph ()
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
  let insert_token x = GlMisc.pass_through x

  exception Feedback_Buffer_Overflow

  (* faire un  gl_init (); avant, et un close() ~dev:GL; après...  *)
  (* draw_proc est un procédure qui lance les tracés opengl. *)
  (* retourne le buffer de feedback et le nombre d'éléments. *)
  let feedback_render draw_proc =
    (* on augmente progressivement la taille du buffer en 2^i si nécessaire...
       (pas très fin, évidemment) *)
    let rec loop i =
      reset_gllist := true;
      (* gllists are not rendered in feedback mode. *)
      gl_init ();
      let r = Raw.create_static `float ~len:(1 lsl i) in
      GlMisc.feedback_buffer ~mode:`_3d_color r;
      ignore (GlMisc.render_mode `feedback);
      GlDraw.color (float_of_color default_color);
      Debug.print "draw in feedback mode...";
      draw_proc ();
      Debug.print "done";
      let num = GlMisc.render_mode `render in
      if num < 0 then
        if i < 31 then (
          Raw.free_static r;
          loop (i + 1))
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
    let coord = Raw.gets_float r ~pos ~len:3 in
    let colour = Raw.gets_float r ~pos:(pos + 3) ~len:4 in
    (coord, colour)

  let point_of_vertex (coord, _colour) = { x = coord.(0); y = coord.(1) }
  let depth_of_vertex (coord, _colour) = coord.(2)

  let color_of_vertex (_coord, colour) =
    let a = colour.(3) in
    if a <> 1. then begin
      prerr_endline (Printf.sprintf "Feedback Alpha:%f\n" a);
      flush stderr
    end;
    { r = colour.(0); g = colour.(1); b = colour.(2) }

  (* inutile *)
  let feedback_print r n =
    for i = 0 to n - 1 do
      Printf.printf "%d: %f\n" i (Raw.get_float r ~pos:i)
    done

  let () = Debug.print "Initialise feedback constants"

  let feedback_view () =
    view (float !left_margin) (float !bottom_margin)
      (float (!window_width + !right_margin))
      (float (!window_height + !top_margin))

  let gl_vertex_size = 7
  let gl_pass_through_token = 1792.0
  let gl_point_token = 1793.0
  let gl_line_token = 1794.0
  let gl_polygon_token = 1795.0
  let gl_line_reset_token = 1799.0

  (* fournit un plot object list [Color c ; Points pl] avec des points
     consécutifs (donc de la même couleur) nmax est 1+l'indice max du
     feedback buffer. Doit commencer par gl_point_token.  Rem: fournit
     la liste à l'envers ! On donne aussi la profondeur moyenne.
  *)
  let feedback_parse_point r n0 nmax =
    let rec loop n c0 pl depsum nombre =
      if n >= nmax || Raw.get_float r ~pos:n <> gl_point_token then
        (pl, depsum, nombre, n)
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
      if n >= nmax || Raw.get_float r ~pos:n <> gl_line_token then
        (pl, depsum, nombre, n)
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
    let num = int_of_float (Raw.get_float r ~pos:(n0 + 1)) in
    let v0 = get_vertex r (n0 + 2) in
    let c0 = color_of_vertex v0 in
    let pl, depsum, nombre, n =
      loop (n0 + 2) (n0 + 1 + (num * gl_vertex_size)) [] 0. 0
    in
    let poly_offset = 0.0001 (* !!!!!!!!!! à normaliser ? *) in
    ([ Color c0; Poly pl ], poly_offset +. (depsum /. float nombre), n)

  let feedback_parse_pass r n0 =
    let token = Raw.get_float r ~pos:(n0 + 1) in
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
        let token = Raw.get_float r ~pos:n in
        let pl', dep, n' =
          if token = gl_point_token then feedback_parse_point r n nmax
          else if token = gl_line_token || token = gl_line_reset_token then
            feedback_parse_line r n nmax
          else if token = gl_polygon_token then feedback_parse_poly r n nmax
          else if token = gl_pass_through_token then feedback_parse_pass r n
          else raise (Not_implemented "unknown token")
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
        Gl.enable `lighting;
        GlLight.light ~num:0 (`position (1., -1., 1., 0.5));
        GlLight.light ~num:0 (`specular (0., 0., 0., 1.));
        GlLight.light ~num:0 (`diffuse (0.2, 0.2, 0.2, 0.8));
        GlLight.light_model (`two_side true);
        Gl.enable `light0;
        GlLight.material ~face:`front (`shininess 30.);
        GlLight.material ~face:`front (`emission (0.2, 0.2, 0.2, 1.));
        GlLight.material ~face:`back (`shininess 10.);
        GlLight.material ~face:`back (`emission (0.1, 0.1, 0.1, 1.));
        Gl.enable `color_material;
        GlLight.color_material ~face:`both `specular;
        GlLight.color_material ~face:`both `ambient_and_diffuse
    | false ->
        Gl.disable `lighting;
        Gl.disable `color_material

  (**********  pour entrer dans le mode oplot 3D ************)
  let enter3d ({ Point3.x = x1; y = y1; _ }, { Point3.x = x2; y = y2; _ }) =
    GlMat.push ();
    GlMat.mode `projection;
    GlMat.push ();
    GlMat.load_identity ();
    GlMat.ortho ~x:(x1, x2) ~y:(y1, y2) ~z:(-100., 100.);
    (* à remplacer par les valeurs zmin zmax, ajustées pour permettre la rotation *)
    GlMat.mode `modelview;
    GlMat.load_identity ();
    GlMat.translate ~z:(-50.) ();
    (*perspective : bien ?
      GlMat.frustum ~x:(-1. , 1.) ~y:(-. 600. /. 800. , 600. /. 800.) ~z:(2. , 6.);
      GlMat.translate ~z:(-4.) (); *)
    switch_light !light_on;

    (* si on met dans une displaylist la lumière n'est calculée qu'une fois. *)

    (*****************)
    let zoom = !zoom3d in
    GlMat.scale ~x:zoom ~y:zoom ~z:zoom ();
    (* inutile et lent ? *)
    let rot = GlMat.of_array (Geom.q_matrix !position3d) in
    GlMat.mult rot;
    Gl.enable `depth_test

  let leave3d () =
    Gl.disable `depth_test;
    GlMat.mode `projection;
    switch_light false;
    GlMat.pop ();
    GlMat.mode `modelview;
    GlMat.pop ()

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
    | User _ ->
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
    Raw.free_static r;
    let fb_view, fb_list = (maxview (List.hd parsed), List.tl parsed) in
    List.iter (fun o -> plot_func ~dev:FIG o fb_view) fb_list;
    (* leave3d (); *)
    if not was_init then close () ~dev:GL

  (**************)

  let copy_back_buffer () =
    GlFunc.draw_buffer `front;
    GlFunc.read_buffer `back;
    GlPix.copy ~x:0 ~y:0 ~width:!window_width ~height:!window_height
      ~buffer:`color;
    GlFunc.draw_buffer `back;
    GlFunc.read_buffer `back

  let copy_buffer i =
    GlFunc.draw_buffer (`aux i);
    GlFunc.read_buffer `back;
    GlPix.copy ~x:0 ~y:0 ~width:!window_width ~height:!window_height
      ~buffer:`color;
    GlFunc.draw_buffer `back;
    GlFunc.read_buffer `back

  let recall_buffer i =
    GlFunc.read_buffer (`aux i);
    GlFunc.draw_buffer `back;
    GlPix.copy ~x:0 ~y:0 ~width:!window_width ~height:!window_height
      ~buffer:`color;
    GlFunc.draw_buffer `back;
    GlFunc.read_buffer `back

  let user_flush = function
    | X11 -> Graphics.synchronize ()
    | GL -> (
        match !default_gl with
        | GLUT -> Iglut.swapbuffers ()
        | SDL -> do_option !win Sdl.gl_swap_window
        | GTK -> () (* ??? *))
    | FIG -> ()

  (***********************************)
  (* gestion des fontes ttf avec sdl *)
  (*  (pas si facile !)                 *)

  let sdl_get_pixel surface x y =
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

  (* calcule la plus petite puissance de deux supérieure ou égale à n *)
  (* ( les textures opengl doivent avoir des dimensions puissances de deux) *)
  let power_of_two n =
    let rec pot n i = if i >= n then i else pot n (i lsl 1) in
    pot n 1

  (* renvoie une texture opengl contenant le message affiché avec la
     fonte FreeSans ou avec LaTeX, ainsi que les dimensions du texte *)
  (* would be cool to have instead a vectorial drawing of the glyphs... *)
  (* for truetype text. we use alpha_luminance format, twice smaller as
     the rgba format *)
  let text_image message size flag =
    let text =
      match flag with
      | Normal ->
          if size <> !current_font_size then (
            current_font := Sdlttf.open_font !font_path size |> go;
            current_font_size := size);
          (* avoid opening the same font every time *)
          Sdlttf.render_utf8_blended !current_font message
            (sdl_color (opaque black))
          |> go
      | Latex -> latex_to_sdl message size
    in
    let w, h = Sdl.get_surface_size text in
    let image_width = power_of_two w and image_height = power_of_two (h + 1) in
    (* on initialise l'image a zéro. Sûrement d'autres moyens de faire
       ça. Je ne sais pas d'ailleurs si cela alloue deux fois la place voulue
       ou non: *)
    let r =
      Raw.of_string
        (String.make (image_width * image_height * 2) '\000')
        ~kind:`ubyte
    in
    let pixel =
      GlPix.of_raw r ~format:`luminance_alpha ~width:image_width
        ~height:image_height
    in
    (* on copie l'image à la main (je ne sais pas faire autrement). On
       pourrait aussi "Blitter" en faisant attention de préserver la
       transparence alpha. Cf sdl manual. Ou Sdlgl.to_raw ?? *)
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
        Raw.sets (GlPix.to_raw pixel)
          ~pos:(2 * (((j + 1) * image_width) + i))
            (* j+1: une ligne de rab pour opengl ...? Faut-il augmenter
               image_height aussi ? *)
          (let (a, _, _), d =
             sdl_get_pixel text i (h - 1 - j)
             (* il faut renverser l'image *)
           in
           [| 255 - a; d |])
        (* 255-a pour pngalpha *)
      done
    done;
    (pixel, w, h)

  (* idem pour une image sdl rgba générale. inutilisé pour le moment *)
  let image_of_sdl image =
    let w, h = Sdl.get_surface_size image in
    let image_width = power_of_two w and image_height = power_of_two h in
    let r =
      Raw.of_string
        (String.make (image_width * image_height * 4) '\000')
        ~kind:`ubyte
    in
    let pixel =
      GlPix.of_raw r ~format:`rgba ~width:image_width ~height:image_height
    in
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
        Raw.sets (GlPix.to_raw pixel)
          ~pos:(4 * ((j * image_width) + i))
          (let (a, b, c), d = sdl_get_pixel image i (h - 1 - j) in
           [| a; b; c; d |])
      done
    done;
    (pixel, w, h)

  (* l'image est rescalée pour que la taille soit indépendante de tout
     (donc une fonte 12 points affiche toujours 12 pixels) *)
  (* je ne sais pas si c'est le mieux *)
  (* affiche une image en position x0 y0 et mode opengl "mode" *)
  let draw_image ?(mode = `mode `modulate) image x0 y0 =
    List.iter
      (GlTex.parameter ~target:`texture_2d)
      [ `mag_filter `nearest; `min_filter `nearest ];
    GlTex.image2d image;
    (* utilisation de image comme texture *)
    Gl.enable `texture_2d;
    (* on trace un rectangle de couleur sur lequel la texture va se
       fixer *)
    let fw, fh = (float (GlPix.width image), float (GlPix.height image)) in
    let rx, ry = (fw /. !fwindow_width, fh /. !fwindow_height) in
    (* ratios à adapter à bounding box : draw of pixel !window_width !window_height. 601 au lieu de 600 pour un petit pb d'arrondi (?) que je ne comprends pas trop *)
    GlTex.env mode;
    (* =VERY IMPORTANT: but why any color seem to work ??? *)
    GlDraw.begins `quads;
    GlTex.coord2 (0.0, 0.0);
    GlDraw.vertex2 (x0, y0);
    GlTex.coord2 (0.0, 1.0);
    GlDraw.vertex2 (x0, y0 +. ry);
    GlTex.coord2 (1.0, 1.0);
    GlDraw.vertex2 (x0 +. rx, y0 +. ry);
    GlTex.coord2 (1.0, 0.0);
    GlDraw.vertex2 (x0 +. rx, y0);
    GlDraw.ends ();
    Gl.disable `texture_2d
  (* GlFunc.blend_func ~src:`src_alpha ~dst:`one_minus_src_alpha;; *)

  (***********************************)
  (* screenshot *)
  (***********************************)
  (* I wish there was a conversion Raw.t --> Bigarray !! *)

  let sdl_screenshot ?(output = png_output) () =
    Gl.finish ();
    (* let t0 = time () in *)
    let w = !window_width + !left_margin + !right_margin in
    let h = !window_height + !top_margin + !bottom_margin in
    let s =
      Sdl.create_rgb_surface_with_format Sdl.Pixel.format_rgb24 ~w ~h ~depth:24
      |> go
    in
    (* rgb888 avec depth=24 ne marche pas... en tout cas ça donne un tableau de
       taille 4*w*h *)
    let r =
      GlPix.to_raw
        (GlPix.read ~x:0 ~y:0 ~width:w ~height:h ~format:`rgb ~kind:`ubyte)
    in
    Sdl.lock_surface s |> go;
    let pix = Sdl.get_surface_pixels s Bigarray.int8_unsigned in
    (* Debug.print "Bigarray dim=%i, w*h=%i, Raw=%i" (Bigarray.Array1.dim pix) *)
    (*   (w * h) (Raw.byte_size r); *)
    assert (Bigarray.Array1.dim pix >= w * h * 3);
    (* We put ">=" above instead of "=" because on macos we don't have strict
       equality: w*h*3 + 2024, for some reason... *)
    let pitch = Sdl.get_surface_pitch s in
    (* For some reason, the image is upside-down, so we have to flip it while
       copying: *)
    for j = 0 to h - 1 do
      let i_pix = (h - j - 1) * pitch in
      let i_r = j * w * 3 in
      for i = 0 to (w * 3) - 1 do
        Bigarray.Array1.unsafe_set pix (i_pix + i) (Raw.get r ~pos:(i_r + i))
      done
    done;
    Sdl.unlock_surface s;
    (* Debug.print "Screenshot surface created in %u ms" (time () - t0); *)
    match Tsdl_image.Image.save_png s output with
    | 0 -> print_endline (Printf.sprintf "Screenshot saved to [%s]." output)
    | i -> Sdl.log "Error %i when saving screenshot to: %s" i output

  (********************)
  (* tracé des objets *)
  (********************)

  let set_line_width ?(dev = !default_device) w =
    match dev with
    | X11 -> Graphics.set_line_width (int_of_float w)
    | GL -> GlDraw.line_width w
    | FIG -> raise (Not_implemented "fig set_line_width")

  let set_point_size ?(dev = !default_device) w =
    match dev with
    | X11 -> raise (Not_implemented "X11 set_point_size")
    | GL -> GlDraw.point_size w
    | FIG -> raise (Not_implemented "fig set_line_size")

  let set_color ?(dev = !default_device) c =
    (match dev with
    | X11 ->
        let r, g, b = int_of_color c in
        Graphics.set_color (Graphics.rgb r g b)
    | GL -> GlDraw.color (c.r, c.g, c.b)
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
    | X11 ->
        Graphics.plots
          (Array.of_list
             (List.rev_map (fun (x, y) -> (int_of_float x, int_of_float y)) ps))
    | GL ->
        GlDraw.begins `points;
        List.iter GlDraw.vertex2 ps;
        GlDraw.ends ()
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
    | X11 ->
        Graphics.draw_poly_line
          (Array.of_list
             (List.rev_map (fun (x, y) -> (int_of_float x, int_of_float y)) ps))
    | GL ->
        GlDraw.begins `line_strip;
        List.iter GlDraw.vertex2 ps;
        GlDraw.ends ()
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
    | X11 ->
        Graphics.fill_poly
          (Array.of_list
             (List.rev_map (fun (x, y) -> (int_of_float x, int_of_float y)) ps))
    | GL ->
        GlDraw.begins `polygon;
        List.iter GlDraw.vertex2 ps;
        GlDraw.ends ();
        (* on retrace le bord pour anti-aliasing. A éviter en mode
           feedback ? *)
        GlDraw.begins `line_loop;
        List.iter GlDraw.vertex2 ps;
        GlDraw.ends ()
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
    begin
      match dev with
      | X11 -> raise (Not_implemented "X11 draw_matrix")
      | GL ->
          let dx = 1. /. float w and dy = 1. /. float h in
          for i = 0 to h - 1 do
            for j = 0 to w - 1 do
              let c = float (m.(i).(j) - min_value) /. dc in
              let x = float j *. dx and y = float i *. dy in
              let { r; g; b } = cmap c in
              GlDraw.color (r, g, b);
              GlDraw.begins `quads;
              GlDraw.vertex2 (x, y);
              GlDraw.vertex2 (x, y +. dy);
              GlDraw.vertex2 (x +. dx, y +. dy);
              GlDraw.vertex2 (x +. dx, y);
              GlDraw.ends ()
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
          let dx = (xmax -. xmin) /. float w
          and dy = (ymax -. ymin) /. float h in
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

  let rec draw_surf3d ?(dev = !default_device) ?(wire = true) gl plot_func mx my
      mz (p1, p2) =
    match dev with
    | X11 -> raise (Not_implemented "X11 draw_surf3d")
    | GL -> begin
        enter3d (p1, p2);
        (* ne peut pas être mis dans la displaylist car contient la rotation qui
           doit s'actualiser *)
        match !gl with
        | Some list when not !reset_gllist -> GlList.call list
        | _ ->
            (* on stocke les ordres graphiques dans une "display_list" opengl *)
            Debug.print "Creating display list";
            let list = GlList.create `compile_and_execute in
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
                GlDraw.color (r *. c, g *. c, b *. c)
              else fun ((_, _, z) : float * float * float) ->
                let c = (z -. zmin) /. (zmax -. zmin) in
                GlDraw.color
                  (c +. r -. (r *. c), c +. g -. (g *. c), c +. b -. (b *. c))
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
              GlDraw.color (r, g, b);
              for j = 1 to w - 1 do
                let a3 = (mx.(i).(j + 1), my.(i).(j + 1), mz.(i).(j + 1))
                and a2 =
                  (mx.(i + 1).(j + 1), my.(i + 1).(j + 1), mz.(i + 1).(j + 1))
                in
                GlDraw.begins `quads;
                (* ou faire quad_strip ? *)
                if not !light_on then setcolor !a0;
                GlDraw.normal3 (normal_vector i j);
                GlDraw.vertex3 !a0;
                if not !light_on then setcolor !a1;
                GlDraw.normal3 (normal_vector (i + 1) j);
                GlDraw.vertex3 !a1;
                if not !light_on then setcolor a2;
                GlDraw.normal3 (normal_vector (i + 1) (j + 1));
                GlDraw.vertex3 a2;
                if not !light_on then setcolor a3;
                GlDraw.normal3 (normal_vector i (j + 1));
                GlDraw.vertex3 a3;
                GlDraw.ends ();

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
              GlDraw.color (r, g, b);
              for ii = 1 to h / 2 do
                let i = ii * 2 in
                GlDraw.begins `line_strip;
                for j = 1 to w do
                  let a = (mx.(i).(j), my.(i).(j), mz.(i).(j)) in
                  GlDraw.vertex3 a
                done;
                GlDraw.ends ()
              done;
              for jj = 1 to w / 2 do
                let j = jj * 2 in
                GlDraw.begins `line_strip;
                for i = 1 to h do
                  let a = (mx.(i).(j), my.(i).(j), mz.(i).(j)) in
                  GlDraw.vertex3 a
                done;
                GlDraw.ends ()
              done
            end;

            leave3d ();
            GlList.ends ();
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
    | X11 -> raise (Not_implemented "X11 draw_grid")
    | GL -> (
        enter3d (p1, p2);
        match !gl with
        | Some list when not !reset_gllist -> GlList.call list
        | _ ->
            (* on stocke les ordres graphiques dans une "display_list" opengl *)
            let list = GlList.create `compile_and_execute in
            let { Point3.x = x1; y = y1; z = zmin } = p1
            and { Point3.x = x2; y = y2; z = zmax } = p2 in
            let dx = (x2 -. x1) /. float w and dy = (y2 -. y1) /. float h in
            let setcolor =
              if (r *. r) +. (g *. g) +. (b *. b) > 1. then fun z ->
                let c = (z -. zmin) /. (zmax -. zmin) in
                GlDraw.color (r *. c, g *. c, b *. c)
              else fun z ->
                let c = (z -. zmin) /. (zmax -. zmin) in
                GlDraw.color
                  (c +. r -. (r *. c), c +. g -. (g *. c), c +. b -. (b *. c))
            in
            (* solid triangles(quads) *)
            for i = 0 to h - 1 do
              let y = y1 +. (float i *. dy) in
              for j = 0 to w - 1 do
                let x = x1 +. (float j *. dx) and z = m.(i).(j) in
                setcolor z;
                GlDraw.begins `quads;
                GlDraw.vertex3 (x, y, z);
                let z = m.(i + 1).(j) in
                setcolor z;
                GlDraw.vertex3 (x, y +. dy, z);
                let z = m.(i + 1).(j + 1) in
                setcolor z;
                GlDraw.vertex3 (x +. dx, y +. dy, z);
                let z = m.(i).(j + 1) in
                setcolor z;
                GlDraw.vertex3 (x +. dx, y, z);
                GlDraw.ends ()
              done
            done;
            GlDraw.color (r, g, b);

            if wire then begin
              (* rem: pas pour xfig, utiliser des polygones à bord plutôt  ? *)
              (*   GlDraw.polygon_mode ~face:`both `line ; *)
              (*   for i=0 to (h-1) do *)
              (*     let y = -.600. /. 800. +. (float i) *. dy in *)
              (*       for j=0 to (w-1) do *)
              (*         let x  = -.1. +. (float j) *. dx *)
              (*         and z = m.(i).(j) in *)
              (*    GlDraw.begins `quads; *)
              (*    GlDraw.vertex3(x, y, z); *)
              (*    let z = m.(i+1).(j) in *)
              (*      GlDraw.vertex3(x, y +. dy, z); *)
              (*      let z = m.(i+1).(j+1) in *)
              (*        GlDraw.vertex3(x +. dx, y +. dy, z); *)
              (*        let z = m.(i).(j+1) in *)
              (*          GlDraw.vertex3(x +. dx, y, z); *)
              (*          GlDraw.ends () *)
              (*       done; *)
              (*   done; *)
              (*  GlDraw.polygon_mode ~face:`both `fill ; *)

              (*GlDraw.line_width 1.5;*)
              (* on trace une ligne sur deux*)
              for ii = 0 to h / 2 do
                let i = ii * 2 in
                let y = y1 +. (float i *. dy) and z = m.(i).(0) in
                GlDraw.begins `line_strip;
                GlDraw.vertex3 (x1, y, z);
                for j = 1 to w do
                  let x = x1 +. (float j *. dx) and z = m.(i).(j) in
                  GlDraw.vertex3 (x, y, z)
                done;
                GlDraw.ends ()
              done;
              for jj = 0 to w / 2 do
                let j = jj * 2 in
                let x = x1 +. (float j *. dx) and z = m.(0).(j) in
                GlDraw.begins `line_strip;
                GlDraw.vertex3 (x, y1, z);
                for i = 1 to h do
                  let y = y1 +. (float i *. dy) and z = m.(i).(j) in
                  GlDraw.vertex3 (x, y, z)
                done;
                GlDraw.ends ()
              done
              (*GlDraw.line_width 1.;*)
            end;
            leave3d ();
            GlList.ends ();
            gl := Some list)
    | FIG ->
        let draw () = draw_grid ~dev:GL ~wire:true gl plot_func m (p1, p2) in
        gl2fig draw plot_func
  (*    set_color !current_color;;*)

  let rec draw_curve3d ?(dev = !default_device) gl plot_func p3d (p1, p2) =
    match dev with
    | X11 -> raise (Not_implemented "X11 draw_curve3d")
    | GL -> begin
        enter3d (p1, p2);
        match !gl with
        | Some list when not !reset_gllist -> GlList.call list
        | _ ->
            (* on stocke les ordres graphiques dans une "display_list" opengl *)
            let list = GlList.create `compile_and_execute in
            GlDraw.begins `line_strip;
            List.iter (fun { Point3.x; y; z } -> GlDraw.vertex3 (x, y, z)) p3d;
            GlDraw.ends ();
            Gl.disable `depth_test;
            GlDraw.begins `points;
            List.iter (fun { Point3.x; y; z } -> GlDraw.vertex3 (x, y, z)) p3d;
            GlDraw.ends ();
            leave3d ();
            GlList.ends ();
            gl := Some list
      end
    | FIG ->
        let draw () = draw_curve3d ~dev:GL gl plot_func p3d (p1, p2) in
        gl2fig draw plot_func

  let draw_segments pl ?(dev = !default_device) ?dep view =
    let ps = rescale_list pl view (bounding_box dev) in
    match dev with
    | X11 ->
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
        GlDraw.begins `lines;
        List.iter GlDraw.vertex2 ps;
        GlDraw.ends ()
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
    | X11 ->
        Graphics.set_text_size (iscale t.size);
        (* la size n'est pas prise en compte sous X11 ?? *)
        (* en remplacement: *)
        (* or use an "association list" (size,font) *)
        let size = max 6 (min 40 (iscale t.size) land 62) in
        Graphics.set_font
          (Printf.sprintf "-*-fixed-*-r-*-*-%d-*-*-*-*-*-*-*" size);
        let w, h = Graphics.text_size t.text in
        let dx =
          match t.align with CENTER -> w / 2 | LEFT -> 0 | RIGHT -> w
        in
        Graphics.moveto (int_of_float x0 - dx) (int_of_float y0 - (h / 2));
        Graphics.draw_string t.text
    | GL ->
        let image, w, h =
          match t.pix with
          | Some (pix, w, h) when not !force_refresh -> (pix, w, h)
          (* : on ne calcule l'image qu'une fois ! *)
          | _ ->
              let pix = text_image t.text (iscale t.size) t.flag in
              t.pix <- Some pix;
              pix
        in
        let dw, dh = draw_of_pixel (w, h) (bounding_box dev) in
        let dx =
          match t.align with CENTER -> dw /. 2. | LEFT -> 0. | RIGHT -> dw
        in
        GlMisc.pass_through text_token;
        draw_image image (x0 -. dx) (y0 -. (dh /. 2.)) ~mode:(`mode `modulate)
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

  let window_flush ?(dev = !default_device) () =
    match dev with
    | X11 -> Graphics.synchronize ()
    | GL -> (*Gl.finish () ? *) do_option !win Sdl.gl_swap_window
    | FIG -> raise (Not_implemented "FIG flush")
  (* flush buffer *)

  (* modifie la position 3d. Rotation autour de l'axe y puis axe x *)
  let rotate3d ax ay =
    let ry = Geom.q_rotation 0. 1. 0. ay and rx = Geom.q_rotation 0. 0. 1. ax in
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
    | k when k = Sdl.K.left -> GlMat.rotate ~angle:1. ~z:1. ()
    | k when k = Sdl.K.right && modifier land Sdl.Kmod.ctrl <> 0 ->
        rotate3d 0. 0.02
    | k when k = Sdl.K.right -> GlMat.rotate ~angle:(-1.) ~z:1. ()
    | k when k = Sdl.K.up && modifier land Sdl.Kmod.ctrl <> 0 ->
        rotate3d (-0.02) 0.
    | k when k = Sdl.K.up -> GlMat.rotate ~angle:(-1.) ~x:1. ()
    | k when k = Sdl.K.down && modifier land Sdl.Kmod.ctrl <> 0 ->
        rotate3d 0.02 0.
    | k when k = Sdl.K.down -> GlMat.rotate ~angle:1. ~x:1. ()
    | k
      when k = Sdl.K.equals
           && modifier land Sdl.Kmod.shift <> 0
           && modifier land Sdl.Kmod.ctrl <> 0 ->
        mincr zoom3d
    | k when k = Sdl.K.equals && modifier land Sdl.Kmod.shift <> 0 ->
        GlMat.scale ~x:1.1 ~y:1.1 ~z:1.1 ()
    | k when k = Sdl.K.plus ->
        GlMat.scale ~x:1.1 ~y:1.1 ~z:1.1 () (* utiliser plutot la caméra?*)
    | k when k = Sdl.K.equals && modifier land Sdl.Kmod.ctrl <> 0 ->
        mdecr zoom3d
    | k when k = Sdl.K.equals -> GlMat.scale ~x:0.91 ~y:0.91 ~z:0.91 ()
    | k when k = Sdl.K.tab && modifier land Sdl.Kmod.ctrl <> 0 ->
        position3d := default_position3d;
        zoom3d := default_zoom3d
    | k when k = Sdl.K.tab ->
        GlMat.pop ();
        GlMat.push ();
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
        (* Sdlvideo.save_BMP (Sdlvideo.get_video_surface ())
           bmp_output; *)
        (* ne marche pas. voir le fichier sdl-opengl *)
        sdl_screenshot ()
    | _
      when Sdl.get_mod_state () = 0
           (* on reste en pause en cas d'appui sur un
              modificateur (control, shift, etc.) *) ->
        resume_pause := true
    | _ -> ());
    !quit

  let sdl_mouse_close () =
    (* print_endline "up button"; *)
    (* Sdl.set_event_state Sdl.Event.mouse_motion Sdl.disable; *)
    (* Sdl.set_event_state Sdl.Event.mouse_button_up Sdl.disable; *)
    ()

  let sdl_mouse_action _mouse =
    (* sdl_mouse_update mouse; *)
    let but, (x, y) = Sdl.get_mouse_state () in
    if but = Sdl.Button.lmask then gl_mouse_motion x (!window_height - y)
    else sdl_mouse_close ()

  let sdl_mouse_init mouse =
    (* print_endline "down_button"; *)
    if Sdl.Event.(get mouse mouse_button_button) = Sdl.Button.left then begin
      (* Sdl.set_event_state Sdl.Event.mouse_motion Sdl.enable; *)
      (* Sdl.set_event_state Sdl.Event.mouse_button_up Sdl.enable; *)
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
    (* Sdl.set_event_state Sdl.Event.window_event Sdl.disable; *)
    do_option !win Sdl.gl_swap_window;
    while (not !has_event) && (t = 0 || time () - init_time < t) do
      has_event := Sdl.poll_event (Some e);
      Sdl.delay (Int32.of_int !frame_length)
    done;
    time_delay := !time_delay + time () - init_time;
    (* Sdl.set_event_state Sdl.Event.window_event Sdl.enable; *)
    if !has_event && Sdl.Event.(get e typ) = Sdl.Event.key_down then
      match Sdl.Event.(get e keyboard_keycode) with
      | k when k = Sdl.K.escape || k = Sdl.K.q -> Sdl.push_event e |> ignore
      (* ou le faire pour tous ? *)
      (* =l'utilisateur veut sortir *)
      | _ -> ()

  let do_freeze ?(dev = !default_device) t =
    match dev with
    | X11 ->
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
    | X11 ->
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
    | X11 -> Graphics.clear_graph ()
    (* la couleur est ignorée *)
    | GL ->
        GlClear.color (float_of_color c);
        GlClear.clear [ `color ]
    | FIG -> raise (Not_implemented "FIG clear")

  let exec_user f view dev =
    let v = match view with None -> raise View_expected | Some v -> v in
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
    | User f -> exec_user f view dev
    | Sheet _ ->
        raise (Invalid_argument "object_plot cannot accept Sheet argument")

  (* use t as a realtime parameter, in seconds *)
  let anim_plot f ?pas ?(t0 = 0.) ?(t1 = 0.) x0 x1 =
    let userfu v dev =
      let t =
        if t1 = 0. then t0 +. (float (time () - !initial_time) /. 1000.)
        else fmin t1 (t0 +. (float (time () - !initial_time) /. 1000.))
      in
      let p = plot (f t) ?pas x0 x1 in
      object_plot p (Some v) ~dev
    in
    User userfu

  (* ne marche pas bien... refaire les pause *)
  let gl_zoom_out t pop =
    let first_time = ref None in
    let foo _ _ =
      match !first_time with
      | None ->
          first_time := Some (time ());
          Debug.print "Initialisation"
      | Some t0 when time () < t0 + t ->
          GlMat.scale ~x:0.91 ~y:0.91 ();
          do_pause 0
      | _ ->
          first_time := None;
          resume_pause := true;
          if pop then (
            GlMat.pop ();
            GlMat.push ())
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
          GlMat.scale ~x:1.1 ~y:1.1 ();
          do_pause 0
      | _ ->
          first_time := None;
          resume_pause := true;
          if pop then (
            GlMat.pop ();
            GlMat.push ())
    in
    foo

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
          | Sheet [] -> ()
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

  (* boucle principale interactive Graphics  pour afficher la feuille sh *)
  let rec graphics_mainloop sh =
    let r, g, b = int_of_color default_color in
    Graphics.set_color (Graphics.rgb r g b);
    (* ne pas mettre ici ? *)
    current_color := default_color;
    Graphics.clear_graph ();
    (* ne pas mettre ici ? *)
    draw sh None ~dev:X11;
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
    GlClear.clear [ `color; `depth ];
    GlDraw.color (float_of_color default_color);
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
    GlClear.clear [ `color; `depth ];
    GlDraw.color (float_of_color default_color);
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
    xfig_main_channel := open_out (Printf.sprintf "%s.main" xfig_output_file);
    xfig_head_channel := open_out (Printf.sprintf "%s.head" xfig_output_file);
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
      xfig_output_file xfig_output_file xfig_output_file;
    shell "rm %s.head %s.main" xfig_output_file xfig_output_file

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
      | None -> if pdf then pdf_output_file else eps_output_file
    in
    if sh_has_latex sh then (
      shell "%s --input=%s %s" convert latex_header xfig_output_file;
      shell "cp %s.%s %s"
        (Filename.remove_extension xfig_output_file)
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
        xfig_output_file output;
    Printf.sprintf "Output file: %s" output |> print_endline

  (* ne traite pas les pauses pour le moment... à intégrer à mainloop ?
*)
  let write_bmp ?(output = png_output) sh =
    gl_init ~show:false ();
    GlClear.clear [ `color ];
    GlDraw.color (float_of_color default_color);
    (*Gl.finish ();*)
    current_color := default_color;
    counter := 0;
    draw sh None ~dev:GL;
    window_flush () ~dev:GL;
    copy_back_buffer ();
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
    | X11 ->
        graphics_init ();
        graphics_resize ();
        graphics_mainloop sh
    | GL -> (
        gl_init ();
        let wait = not (has_anim sh) in
        (* TODO:  `line_smooth n'a pas d'effet pour GTK?: *)
        if wait then (
          Gl.enable `line_smooth;
          GlMisc.hint `line_smooth `nicest)
        else GlMisc.hint `line_smooth `fastest;
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
            GlClear.clear [ `color; `depth ];
            do_option !win Sdl.gl_swap_window;
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
    | X11_d -> disp (Sheet sh) ~dev:X11
    | GL_d -> disp (Sheet sh) ~dev:GL ~fscreen
    | FIG_d -> disp (Sheet sh) ~dev:FIG
    | EPS_d ->
        disp (Sheet sh) ~dev:FIG;
        write_eps ?output ~pdf:false (Sheet sh)
    | PDF_d ->
        disp (Sheet sh) ~dev:FIG;
        write_eps ?output ~pdf:true (Sheet sh)
    | XFIG_d ->
        disp (Sheet sh) ~dev:FIG;
        shell "xfig -correct_font_size -zoom 1 %s &" xfig_output_file
    | GV_d -> (
        disp (Sheet sh) ~dev:FIG;
        write_eps ~pdf:false (Sheet sh);
        match psviewer with
        | Some "gv" -> shell "gv --media=BBox --watch %s &" eps_output_file
        | Some "kghostview" ->
            shell "kghostview --portrait %s &" eps_output_file
        | Some prog -> shell "%s %s &" prog eps_output_file
        | None -> print_endline "No postscript viewer found.")
    | BMP_d ->
        print_endline "using PNG instead of BMP.";
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
    shell "gv --watch --scalebase=2 %s&" eps_output_file

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

(************************************************************************)

(*
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

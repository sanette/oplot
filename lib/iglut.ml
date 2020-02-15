(* interface glut bidon, à partir de 0.3 *)
(* les anciennes versions sont en comentaires, et ne marchent que si insérées
   dans oplotmain.ml *)
(* nécessite lablglut *)

exception Not_compiled of string

let noway () = raise (Not_compiled "GLUT interface")

let fullscreen () = noway ()
(* let fullscreen () = Glut.fullScreen ()  *)

let swapbuffers () = noway ()
(* let swapbuffers () = Glut.swapBuffers () *)

let freeze _t = noway ()
(* let freeze t = raise (Not_implemented "GLUT freeze") *)

(* initialisation d'une fenêtre opengl par GLUT *)

let init () = noway ()
(* let glut_init ()  = let _ = Glut.init Sys.argv in *)
(*   Glut.initDisplayMode ~double_buffer:true (); *)
(*   Glut.initWindowSize !window_width !window_height; *)
(*   ignore (Glut.createWindow "Oplot - Glut Window"); *)
(*   Glut.idleFunc ~cb:(Some Glut.postRedisplay); *)
(*   Glut.keyboardFunc ~cb:(fun ~key ~x ~y -> match char_of_int key with *)
(* 			     '\027' -> exit 0 *)
(* 			   | 'F' -> Glut.fullScreen () *)
(* 			   | 'f' -> Glut.reshapeWindow  *)
(* 			       (Glut.get ~gtype:Glut.INIT_WINDOW_WIDTH) *)
(* 				 (Glut.get ~gtype:Glut.INIT_WINDOW_HEIGHT); *)
(* 			       Glut.positionWindow  *)
(* 				 (Glut.get ~gtype:Glut.INIT_WINDOW_X) *)
(* 				 (Glut.get ~gtype:Glut.INIT_WINDOW_Y) *)
(* 				 (\* faire plutot un toggle *\) *)
(* 			   | '+' -> GlMat.scale ~x:1.1 ~y:1.1 () *)
(* 			   | '-' | '=' -> GlMat.scale ~x:0.91 ~y:0.91 () *)
(* 			   | 'p' -> decr pause_pass *)
(* 			   | '\009' -> (GlMat.pop (); GlMat.push ()) *)
(* 			   | _ ->  resume_pause := true ); *)
(*   Glut.specialFunc ~cb:(fun ~key ~x ~y -> match key with *)
(* 			    Glut.KEY_LEFT -> GlMat.rotate ~angle:1. ~z:1. () *)
(* 			  | Glut.KEY_RIGHT -> GlMat.rotate ~angle:(-1.) ~z:1. () *)
(* 			  | Glut.KEY_UP -> GlMat.rotate ~angle:(1.) ~x:1. () *)
(* 			  | Glut.KEY_DOWN -> GlMat.rotate ~angle:(-1.) ~x:1. () *)
(* 			  | _ ->  resume_pause := true );; *)




(* boucle principale interactive GLUT  pour afficher la feuille sh *)

let mainloop _sh = noway ()
(* let glut_mainloop sh =  *)
(*   let gl_reshape ~w ~h = resize_window w h; *)
(*     GlDraw.viewport 0 0 w h; *)
(*   in *)
(*   let gl_display () = *)
(*     GlClear.clear [ `color ]; *)
(*     GlDraw.color (float_of_color default_color); *)
(*     current_color := default_color; *)
(*     counter := 0; *)

(*     (\* just for fun: *\) *)
(*     (\* GlMat.rotate ~angle:(0.01) ~z:1. (); *\) *)
(*     draw sh None ~dev:GL; *)
(*     Glut.swapBuffers (); *)
(*     if !do_not_draw then ( do_not_draw := false ) else  *)
(*       ( pause_pass := 0 )  *)
(*   in *)
(*     Glut.reshapeFunc ~cb:gl_reshape; *)
(*     Glut.displayFunc ~cb:gl_display; *)
(*     Glut.mainLoop ();; *)


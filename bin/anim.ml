(* User-defined animation. Notice the difference with User objects *)

open Oplot.Plt

let v = view 0. (-1.) 20. 1.
let t = text "Hello world" 10. 0. ~size:50
let f t1 t2 x = sin (((1. +. sin t1) *. (x -. 10.)) -. t2)
let ap = anim_plot (fun t -> f t (t *. 1.237)) 0. 20.;;

display
  [
    v;
    Color red;
    t;
    Color cyan;
    ap;
    Color blue;
    text "The curve is continuously updated." 10. (-0.5);
  ]
  ~dev:gl
;;

let user v dev =
  let p =
    plot (f (float (elapsed ()) /. 1000.) (float (elapsed ()) /. 1237.)) 0. 20.
  in
  object_plot p (Some v) ~dev
;;

display
  [
    v;
    Color green;
    t;
    Color cyan;
    User user;
    Color blue;
    text "The curve is updated only when some event occurs." 10. (-0.5);
  ]
  ~dev:gl
;;

quit ()

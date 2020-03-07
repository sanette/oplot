(* Oplot example of User object: bifurcation diagram for the logistic map *)

#use "topfind";;
#require "oplot";;

open Oplot.Plt;;
open Oplot.Points.Point2;;

let rec iterate r x_init i =
  if i == 1 then x_init
  else
    let x = iterate r x_init (i-1) in
    r *. x *. (1.0 -. x);;

let rec trace ~progress x0 x1 step v dev = 
  if x0 > x1 then ()
  else
    let imax = int_of_float (exp 4. *. (x0 -. 2.)) in 
    let rec loop i list =
      if i >= imax then list else
        let x_init = Random.float 1.0 in
        let x_final = iterate x0 x_init 500 in
        let p = point (x0,x_final) in loop (i+1) (p::list)
    in object_plot (Points (loop 0 [])) (Some v) ~dev ; 
    set_color {r = x0 /. 4. ; g = 0.5 ; b = 0.5 } ~dev;
    if progress
    (* we force showing the picture in progress: *)
    then if elapsed () mod 34 = 0 then (copy_back_buffer (); user_flush dev);
    trace ~progress (x0 +. step) x1 step v dev;;

let logistique ?(progress=true) ~step v dev =
  trace ~progress (fst v).x (snd v).x step v dev;
  copy_back_buffer ();;
(* thanks to this, the picture is not erased in the second display invocation
   below when there is no Pause. *)

let v = view 2.4 0. 4. 1.;;
let a = axis 2.5 0.;;
let compute = text "Computing..." 2.7 0.3;;
let finished = text "Done." 2.9 0.3;;

display [v; a; compute; Pause 1; 
         User (logistique ~step:0.001); finished; Freeze 0];;

display [v; a; compute; (* Pause 1; *) 
         User (logistique ~progress:false ~step:0.001); finished; Freeze 0];;
(* BUG: Freeze prevents proper rescaling of the window *)

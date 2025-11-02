(* Oplot example of User object: bifurcation diagram for the logistic map *)
#use "topfind";;

#thread;;

#require "oplot";;

open Oplot.Plt;;
open Oplot.Points.Point2;;

let rec iterate r x_init i =
  if i = 1 then x_init
  else
    let x = iterate r x_init (i - 1) in
    r *. x *. (1.0 -. x)

let time = ref (elapsed ())

let rec trace ~progress x0 x1 step v dev =
  if x0 > x1 then ()
  else
    let imax = int_of_float (exp 4. *. (x0 -. 2.)) in
    let rec loop i list =
      if i >= imax then list
      else
        let x_init = Random.float 1.0 in
        let x_final = iterate x0 x_init 500 in
        let p = point (x0, x_final) in
        loop (i + 1) (p :: list)
    in
    object_plot (Points (loop 0 [])) (Some v) ~dev;
    set_color { r = x0 /. 4.; g = 0.5; b = 0.5 } ~dev;
    let () =
      if progress (* we force showing the picture in progress: *) then
        let t = elapsed () in
        if t - !time > 16 then (
          time := t;
          print_endline (string_of_int t)
          (* user_flush dev; *)
          (* THIS WILL BADLY FLICKER ON MOST HARDWARE *))
    in
    trace ~progress (x0 +. step) x1 step v dev

let logistique ?(progress = true) ~step v dev =
  print_endline "Logistique";
  trace ~progress (fst v).x (snd v).x step v dev
(* copy_back_buffer () *)

(* thanks to this, the picture is not erased in the second display invocation
   below when there is no Pause. *)

let v = view 2.4 0. 4. 1.
let a = axis 2.5 0.
let compute = text "Computing..." 2.7 0.3
let finished = text "Done." 2.9 0.3;;

display [ v; a; compute; User (logistique ~step:0.001); finished ];;

display
  [
    v;
    a;
    compute;
    (* Pause 1; *)
    User (logistique ~progress:false ~step:0.001);
    finished;
  ]
;;

(* BUG: Freeze prevents proper rescaling of the window *)

(* CTRL-c CTRL-b *)
#use "topfind";;

#thread;;

#require "oplot.dummy";;

open Oplot.Plt;;

#use "ode.ml";;

(* VU NGOC San *)
(* Classical mechanics for magnetics fields in 2D *)
(* A=(A_1,A_2) magnetic potential *)
(* B = rot A orthonogal to the 2D plane *)
(* q=(q_1,q_2)=(x,y) p=(p_1,p_2) canonical coordinates *)

(* Example: *)
(* A1 = -y(1+x²/4 + y²/4 + x³/3) *)
(* A2 = x(1+x²/4 + y²/4) *)
(* B = 2+x²+y²+x³/3 *)

let b x y = 2. +. (x *. x) +. (y *. y) +. (x *. x *. x /. 3.)
let db1 x y = (2. *. x) +. (x *. x)
let db2 x y = 2. *. y

let a1 x y =
  -.y *. (1. +. (x *. x /. 4.) +. (y *. y /. 4.) +. (x *. x *. x /. 3.))

let a2 x y = x *. (1. +. (x *. x /. 4.) +. (y *. y /. 4.))
let a [| x; y |] = [| a1 x y; a2 x y |]

(* gradients of magnetic potential *)

(* dA_1/dq_1 *)
let a11 [| x; y |] = -.y *. ((x /. 2.) +. (x *. x))

(* dA_1/dq_2 *)
let a12 [| x; y |] =
  -1. -. (x *. x /. 4.) -. (3. *. y *. y /. 4.) -. (x *. x *. x /. 3.)

(* dA_2/dq_1 *)
let a21 [| x; y |] = 1. +. (3. *. x *. x /. 4.) +. (y *. y /. 4.)

(* dA_2/dq_2 *)
let a22 [| x; y |] = x *. y /. 2.
let tda1 q = [| a11 q; a21 q |]
let tda2 q = [| a12 q; a22 q |]

(* Hamiltonian vector field *)
let dq q p = p -- a q

let dp q p =
  let qdot = dq q p in
  [| pscal (tda1 q) qdot; pscal (tda2 q) qdot |]

(* z=(q1,q2,p1,p2), time independent => _ *)
let magnetic _ z =
  let q, p = split z in
  combine (dq q p) (dp q p)

(* initial condition *)
let init epsilon x0 y0 =
  [| x0; y0; sqrt epsilon +. a1 x0 y0; sqrt epsilon +. a2 x0 y0 |]

let traj epsilon x0 y0 =
  let v0 = init epsilon x0 y0 in
  desolve magnetic v0 0. 100.

let traj2d epsilon x0 y0 =
  List.map (fun a -> { x = a.(0); y = a.(1) }) (traj epsilon x0 y0)

(* contour plot 2D *)

let contour f df1 df2 inits t =
  let champ _ [| x; y |] = [| df2 x y; -.df1 x y |] in
  Lines (List.map (fun (x0, y0) -> desolve_2d champ x0 y0 0. t) inits);;

(* tests *)

let epsilon = 0.1
let x0 = 0.5
let y0 = 0.
let inits = List.map (fun x -> (x0 +. x, y0)) [ -0.15; 0.; 0.15; 0.30 ]
let cnt = contour b db1 db2 inits 5.
let cnt_text = latex ~size:26 "$B^{-1}(\\textup{const})$" 0.85 (-0.6)
let r_text = latex ~size:26 "$r\\sim |\\dot q|/2B$" 0.89 (-0.3)
let plt = Lines [ traj2d epsilon x0 y0 ]
let vw = view (-.(1. +. x0)) (-.(1.2 +. y0)) (1. +. x0) (1.2 +. y0);;

(* display [ vw; Color red; cnt; cnt_text; *)
(*           Color blue; plt; *)
(*           Color black; (axis 0. 0.)];; *)

let anm =
  anim_ode ~time_pos:{ x = 1.; y = 1. } ~show_head:true ~speed:10. magnetic
    (init epsilon x0 y0) 0. 500.

let anm_slow =
  anim_ode ~time_pos:{ x = 1.; y = 1. } ~show_head:true ~speed:0.5 magnetic
    (init epsilon x0 y0) 0. 4.
;;

display
  [
    vw;
    Color red;
    cnt;
    cnt_text;
    Color blue;
    repeat;
    anm_slow;
    r_text;
    Color black;
    axis 0. 0.;
    Pause 0;
  ]
;;

display
  [ vw; Color red; cnt; cnt_text; Color blue; repeat; anm; Color black; axis 0. 0. ];;

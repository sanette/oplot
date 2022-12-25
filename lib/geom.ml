exception Empty_Intersection

open Points

(* géométrie du plan *)
(*********************)

open Point2

type segment = point * point

(* x est-il dans l'intervalle [x1,x2] (ou [x2,x1]) *)
let in_interval (x : float) x1 x2 =
  let xmin, xmax = if x1 <= x2 then (x1, x2) else (x2, x1) in
  xmin <= x && x <= xmax

(* p, p1, p2 étant des points alignés, est-ce que p appartient au segment [p1,p2] *)
let in_segment p (p1, p2) =
  let a, a1, a2 =
    if abs_float (p2.y -. p1.y) > abs_float (p2.x -. p1.x) then (p.y, p1.y, p2.y)
    else (p.x, p1.x, p2.x)
  in
  in_interval a a1 a2

(* déterminant 2D *)
let det a b c d = (a *. d) -. (b *. c)

(* solution du système de cramer ax+by=e, cx+dy=f *)
let cramer a b e c d f =
  let dt = det a b c d in
  (det e b f d /. dt, det a e c f /. dt)

(* intersection de deux segments. Remplacer exception par une option ? *)
let inter_segments (p1, p2) (q1, q2) =
  let x, y =
    try
      cramer (* intersection des droites directrices *)
        (p2.y -. p1.y) (p1.x -. p2.x) (det p1.x p2.x p1.y p2.y) (q2.y -. q1.y)
        (q1.x -. q2.x) (det q1.x q2.x q1.y q2.y)
    with _ -> raise Empty_Intersection
  in
  let p = { x; y } in
  if in_segment p (p1, p2) && in_segment p (q1, q2) then p
  else raise Empty_Intersection

(* intersection du segment s avec la boîte rectangulaire donnée par les coins
   diagonaux p1, p2 *)
let inter_box s (p1, p2) =
  let q1, q2, q3, q4 =
    (p1, { x = p2.x; y = p1.y }, p2, { x = p1.x; y = p2.y })
  in
  try inter_segments s (q1, q2)
  with _ -> (
    try inter_segments s (q2, q3)
    with _ -> (
      try inter_segments s (q3, q4)
      with _ -> (
        try inter_segments s (q4, q1) with _ -> raise Empty_Intersection)))
;;

(* exemple: *)
(************)
(* let p1 = {x=0.; y=0.};; *)
(* let p2 = {x=1.; y=1.};; *)
(* let s = ({x=0.; y=2.}, {x=0.5; y=0.5});; *)
(* inter_box s (p1,p2);; *)
(* - : point = {x = 0.333333333333333315; y = 1.} *)
(************)

Random.self_init ()

let deriv f x =
  let dx = 0.000001 in
  (f (x +. dx) -. f (x -. dx)) /. dx /. 2.

(* longueur approximative d'une courbe (monte carlo) *)
let curve_length fx fy t0 t1 =
  let nt = 50 in
  let dt = (t1 -. t0) /. float nt in
  let rec loop n somme =
    if n = 0 then somme
    else
      let t = t0 +. Random.float (t1 -. t0) in
      let dx = deriv fx t and dy = deriv fy t in
      loop (n - 1) (somme +. sqrt ((dx ** 2.) +. (dy ** 2.)))
  in
  loop nt 0. *. dt

(* rotations dans l'espace *)
(***************************)

let pi = 4. *. atan 1.
let deg2rad angle = pi *. angle /. 180.
let rad2deg angle = angle *. 180. /. pi

(* (inutilisé) rotation infinitesimale, dans les coordonnées des angles d'Euler
   (x,y,z) (en degrés, et x est inutile), pour une direction dsX dY dZ en
   coordonnées cartésiennes. Fournit les variations des angles d'Euler (dx, dy,
   dz) *)
let euler_variation y z dX dY dZ =
  let zz = deg2rad z and yy = deg2rad y in
  let cz = cos zz and sz = sin zz and cy = cos yy and ty = tan yy in
  let dx = (dX *. cz /. cy) -. (dY *. sz *. cy)
  and dy = (dX *. sz) +. (dY *. cz)
  and dz = (-.dX *. ty *. cz) +. (dY *. ty *. sz) +. dZ in
  (rad2deg dx, rad2deg dy, rad2deg dz)

type quaternion = float * float * float * float

let q_mult (p1, p2, p3, p4) (q1, q2, q3, q4) : quaternion =
  ( (q4 *. p1) +. (q3 *. p2) -. (q2 *. p3) +. (q1 *. p4),
    (-.q3 *. p1) +. (q4 *. p2) +. (q1 *. p3) +. (q2 *. p4),
    (q2 *. p1) -. (q1 *. p2) +. (q4 *. p3) +. (q3 *. p4),
    (-.q1 *. p1) -. (q2 *. p2) -. (q3 *. p3) +. (q4 *. p4) )

(* quaternion en fonction d'un angle et d'un axe *)
let q_rotation x y z t : quaternion =
  let s = sin (t /. 2.) in
  (x *. s, y *. s, z *. s, cos (t /. 2.))

(* fonction inverse non normalisée *)
let q_axis (q1, q2, q3, q4) =
  let n = sqrt ((q1 *. q1) +. (q2 *. q2) +. (q3 *. q3)) in
  if n = 0. then (0., 0., 1., 0.) else (q1 /. n, q2 /. n, q3 /. n, 2. *. acos q4)

(* matrice dans SO3 *)
let q_matrix (q1, q2, q3, q4) =
  [|
    [|
      1. -. (2. *. ((q3 *. q3) +. (q4 *. q4)));
      2. *. ((q2 *. q3) -. (q1 *. q4));
      2. *. ((q1 *. q3) +. (q2 *. q4));
      0.;
    |];
    [|
      2. *. ((q2 *. q3) +. (q1 *. q4));
      1. -. (2. *. ((q2 *. q2) +. (q4 *. q4)));
      2. *. ((q3 *. q4) -. (q1 *. q2));
      0.;
    |];
    [|
      2. *. ((q2 *. q4) -. (q1 *. q3));
      2. *. ((q1 *. q2) +. (q3 *. q4));
      1. -. (2. *. ((q2 *. q2) +. (q3 *. q3)));
      0.;
    |];
    [| 0.; 0.; 0.; 1. |];
  |]

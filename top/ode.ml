(*

Dynamical Systems plugin for Oplot
San Vu Ngoc

---

Example:

let f t y dydt = dydt.(0) <- sin (t +. (y.(0)));;
let dydt=Array.make 1 0.;;
f 0. [|1.|] dydt;;

let sys = Gsl.odeiv.make_system f 1;;
(* système avec f, de dimension 1 *)

let step = Gsl.odeiv.make_step Gsl.odeiv.RKF45 1;;
(* choix de la methode RKF45 dimension 1 *)

let y=Array.make 1 0.;;
let yerr=Array.make 1 0.;;

Gsl.odeiv.step_apply step ~t:0. ~h:0.001 ~y:y ~yerr:yerr sys;;
(* on avance de 0.001. y contient la nouvelle valeur et yerr l'erreur *)


*)
#require "gsl"

#require "oplot"

open Oplot.Plt
open Oplot.Points
open Point2
module Osys = Internal

let to_vec3 a = (a.(0), a.(1), a.(2))

(* résolution non adaptative (le pas est fixe, méthode fixe) *)
(* fournit le graphe de la solution (liste inversée) *)
let rec desolve_1d ?pas f y0 t0 t1 =
  let p = match pas with None -> (t1 -. t0) /. 800. | Some pp -> pp in
  let sys = Gsl.Odeiv.make_system (fun t y dydt -> dydt.(0) <- f t y.(0)) 1 in
  let step = Gsl.Odeiv.make_step Gsl.Odeiv.RKF45 ~dim:1 in
  let y = Array.make 1 y0 in
  let yerr = Array.make 1 0. in
  let rec loop t list =
    let t' = t +. p in
    if t' <= t1 then (
      Gsl.Odeiv.step_apply step ~t ~h:p ~y ~yerr sys;
      loop t' ({ x = t'; y = y.(0) } :: list))
    else (
      Gsl.Odeiv.step_apply step ~t ~h:(t1 -. t) ~y ~yerr sys;
      { x = t1; y = y.(0) } :: list)
  in
  loop t0 []

(* résolution non adaptative (le pas est fixe, méthode fixe) *)
(* fournit le tracé paramétrique de la solution *)
(* ici f doit être à valeur tableau -> tableau [| x , y |] *)
let rec desolve_2d ?pas f x0 y0 t0 t1 =
  let p = match pas with None -> (t1 -. t0) /. 800. | Some pp -> pp in
  let sys =
    Gsl.Odeiv.make_system (fun t y dydt -> Array.blit (f t y) 0 dydt 0 2) 2
  in
  let step = Gsl.Odeiv.make_step Gsl.Odeiv.RKF45 ~dim:2 in
  let y = [| x0; y0 |] in
  let yerr = Array.make 2 0. in
  let rec loop t list =
    let t' = t +. p in
    if t' <= t1 then (
      Gsl.Odeiv.step_apply step ~t ~h:p ~y ~yerr sys;
      loop t' ({ x = y.(0); y = y.(1) } :: list))
    else (
      Gsl.Odeiv.step_apply step ~t ~h:(t1 -. t) ~y ~yerr sys;
      { x = y.(0); y = y.(1) } :: list)
  in
  loop t0 [ { x = x0; y = y0 } ]

(* cas general dimension n. y0 est un tableau de dimension n.
   f doit être une fonction float->array->array *)
let rec desolve ?(first = true) ?pas f y0 t0 t1 =
  let p = match pas with None -> (t1 -. t0) /. 800. | Some pp -> pp in
  let dim = Array.length y0 in
  let sys =
    Gsl.Odeiv.make_system (fun t y dydt -> Array.blit (f t y) 0 dydt 0 dim) dim
  in
  let step = Gsl.Odeiv.make_step Gsl.Odeiv.RKF45 ~dim in
  let y = Array.copy y0 in
  let yerr = Array.make dim 0. in
  let rec loop t list =
    let t' = t +. p in
    if t' <= t1 then (
      Gsl.Odeiv.step_apply step ~t ~h:p ~y ~yerr sys;
      loop t' (Array.copy y :: list))
    else (
      Gsl.Odeiv.step_apply step ~t ~h:(t1 -. t) ~y ~yerr sys;
      y :: list)
  in
  loop t0 (if first then [ y0 ] else [])

let pscal v1 v2 =
  let n = Array.length v1 in
  let r = ref 0. in
  for i = 0 to n - 1 do
    r := !r +. (v1.(i) *. v2.(i))
  done;
  !r

let ( ++ ) v1 v2 =
  let n = Array.length v1 in
  let v = Array.make n 0. in
  for i = 0 to n - 1 do
    v.(i) <- v1.(i) +. v2.(i)
  done;
  v

let ( -- ) v1 v2 =
  let n = Array.length v1 in
  let v = Array.make n 0. in
  for i = 0 to n - 1 do
    v.(i) <- v1.(i) -. v2.(i)
  done;
  v

let ( **. ) a v1 =
  let n = Array.length v1 in
  let v = Array.make n 0. in
  for i = 0 to n - 1 do
    v.(i) <- a *. v1.(i)
  done;
  v

let combine u v =
  let n = Array.length u in
  let a = Array.make (2 * n) 0. in
  for i = 0 to n - 1 do
    a.(i) <- u.(i);
    a.(i + n) <- v.(i)
  done;
  a

let split a =
  let n = Array.length a / 2 in
  let u = Array.make n 0. and v = Array.make n 0. in
  for i = 0 to n - 1 do
    u.(i) <- a.(i);
    v.(i) <- a.(i + n)
  done;
  (u, v)

let project indices a =
  let n = Array.length indices in
  let v = Array.make n 0. in
  for i = 0 to n - 1 do
    v.(i) <- a.(indices.(i))
  done;
  v

(* version avec mémoire: *)
(* pour ne pas concaténer des listes, on construit des listes de listes *)
(* le résultat peut être vide (si t1 <= t0) *)
let desolve_2d_memo ?(pas = 0.1) f x0 y0 t0 =
  let memo_points, memo_lastpoint, memo_time =
    (ref [], ref { x = x0; y = y0 }, ref t0)
  in
  let foo t1 =
    if t1 > !memo_time then begin
      let { x = new_x0; y = new_y0 } = !memo_lastpoint in
      let new_points = desolve_2d ~pas f new_x0 new_y0 !memo_time t1 in
      memo_points := new_points :: !memo_points;
      memo_time := t1;
      memo_lastpoint := List.hd new_points
    end;
    !memo_points
  in
  foo

(* idem pour la dimension quelconque, mais ici on applatit la liste,
   et la liste contient toujours le point initial *)
let desolve_memo ?(pas = 0.1) f y0 t0 =
  let memo_points, memo_lastpoint, memo_time, dim =
    (ref [ [ y0 ] ], Array.copy y0, ref t0, Array.length y0)
  in
  let foo t1 =
    if t1 > !memo_time then begin
      let new_y0 = Array.copy memo_lastpoint in
      let new_points = desolve ~first:false ~pas f new_y0 !memo_time t1 in
      memo_points := new_points :: !memo_points;
      memo_time := t1;
      Array.blit (List.hd new_points) 0 memo_lastpoint 0 dim
      (*memo_lastpoint := Array.copy (List.hd new_points)*)
    end;
    List.flatten !memo_points
  in
  (* à sauver dans memo_points ? *)
  foo

(* tracé progressif en temps réel: cas 2D *)
let anim_ode_2d f x0 y0 t0 t1 =
  let mysolve = desolve_2d_memo f x0 y0 t0 in
  let userfu v dev =
    let t = min t1 (t0 +. (float (elapsed ()) /. 1000.)) in
    (* List.iter (fun x -> object_plot (Lines x) (Some v) ~dev) *)
    object_plot (Lines (mysolve t)) (Some v) ~dev
  in
  User userfu

let proj2d a = { x = a.(0); y = a.(1) }

(* tracé progressif en temps réel: dimension quelconque, mais on trace
   les 2 premières coords *)
let anim_ode ?time_pos ?(show_head = false) ?(speed = 1.) f y0 t0 t1 =
  let mysolve = desolve_memo f y0 t0 in
  let userfu v dev =
    let t = min t1 (speed *. (t0 +. (float (elapsed ()) /. 1000.))) in
    (* List.iter (fun x -> object_plot (Lines x) (Some v) ~dev) *)
    let sol = mysolve t in
    let sol2d = List.map proj2d sol in
    object_plot (Lines [ sol2d ]) (Some v) ~dev;
    (match time_pos with
    | Some { x; y } ->
        let tt = text (Printf.sprintf "t=%.2f" t) x y in
        object_plot tt (Some v) ~dev
    | None -> ());
    if show_head then
      let p = List.hd sol2d in
      let hd = text "o" p.x p.y in
      object_plot hd (Some v) ~dev
  in
  User userfu

(* idem mais on trace en 3D *)
(* la "view3" est adaptative: à changer ? *)
let anim_ode_3d f y0 t0 t1 =
  let mysolve = desolve_memo ~pas:((t1 -. t0) /. 800.) f y0 t0 in
  let userfu (a1, a2) dev =
    Osys.set_line_width 4.;
    Osys.set_point_size 1.3;
    let t = min t1 (t0 +. (float (elapsed ()) /. 1000.)) in
    (* List.iter (fun x -> object_plot (Lines x) (Some v) ~dev) *)
    let sol = mysolve t in
    let y0 = List.hd sol in
    let zmin, zmax = (ref y0.(2), ref y0.(2)) in
    let sol3d =
      List.map
        (fun a ->
          let x, y, z = (a.(0), a.(1), a.(2)) in
          if !zmin > z then zmin := z;
          if !zmax < z then zmax := z;
          { Point3.x; y; z })
        sol
    in
    let v3 =
      ( { Point3.x = a1.Point2.x; y = a1.Point2.y; z = !zmin },
        { Point3.x = a2.Point2.x; y = a2.Point2.y; z = !zmax } )
    in

    object_plot
      (Curve3d ((sol3d, v3), Osys.gllist_empty ()))
      ~dev
      (Some (a1, a2));
    Osys.set_line_width 1.3;
    Osys.set_point_size 1.3
  in
  User userfu

(* surface engendrée par deux champs de vecteurs *)
let frobenius f1 f2 ?(width = 40) ?(height = 40) ?(wire = true) y0 u0 u1 v0 v1 =
  let mx = Array.make_matrix (height + 1) (width + 1) 0.
  and my = Array.make_matrix (height + 1) (width + 1) 0.
  and mz = Array.make_matrix (height + 1) (width + 1) 0. in
  let du = (u1 -. u0) /. float width and dv = (v1 -. v0) /. float height in
  let pasu = du /. 5. and pasv = dv /. 5. in
  let zmin = ref y0.(2) and xmin = ref y0.(0) and ymin = ref y0.(1) in
  let zmax = ref !zmin and xmax = ref !xmin and ymax = ref !ymin in
  let ya = Array.copy y0 and yb = Array.copy y0 in
  for i = 0 to height do
    let lignex = mx.(i) and ligney = my.(i) and lignez = mz.(i) in
    print_endline (Printf.sprintf "Computing line:%d/%d..." i height);
    lignex.(0) <- ya.(0);
    ligney.(0) <- ya.(1);
    lignez.(0) <- ya.(2);
    let v = v0 +. (float i *. dv) in
    for j = 1 to width do
      let u = u0 +. (float j *. du) in
      let x, y, z = List.hd (desolve f1 ya ~pas:pasu (u -. du) u) |> to_vec3 in
      lignex.(j) <- x;
      ligney.(j) <- y;
      lignez.(j) <- z;
      ya.(0) <- x;
      ya.(1) <- y;
      ya.(2) <- z;
      if z < !zmin then zmin := z;
      if z > !zmax then zmax := z;
      if x < !xmin then xmin := x;
      if x > !xmax then xmax := x;
      if y < !ymin then ymin := y;
      if y > !ymax then ymax := y
    done;
    let x, y, z = List.hd (desolve f2 yb v ~pas:pasv (v +. dv)) |> to_vec3 in
    ya.(0) <- x;
    ya.(1) <- y;
    ya.(2) <- z;
    yb.(0) <- x;
    yb.(1) <- y;
    yb.(2) <- z
  done;
  Surf3d
    ( ( mx,
        my,
        mz,
        ( { Point3.x = !xmin; y = !ymin; z = !zmin },
          { Point3.x = !xmax; y = !ymax; z = !zmax } ),
        wire ),
      Osys.gllist_empty () )

let sauve_coord list =
  let out = open_out "coords.txt" in
  let rec loop l =
    match l with
    | [] -> ()
    | v :: ll ->
        Printf.fprintf out "%f, %f, %f,\n" v.(0) v.(1) v.(2);
        loop ll
  in
  loop list;
  close_out out

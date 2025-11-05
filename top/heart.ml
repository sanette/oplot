(* Level set curve *)
#use "topfind";;
#thread;;
#require "oplot";;

open Oplot.Plt

let heart x y = (y *. y +. x *. x -. 1.) ** 3. -. x *. x *. (y ** 3.)
let p0, p1 = point (-2., -2.), point (2., 2.)
let v = view (-2.5) (-2.5) 2.5 2.5
let a = axis 0. 0.
let ctr = implicit_curve heart (p0, p1);;

display [v; Color black ; a; line_width 2.; Color blue; ctr];;

display ~dev:pdf [v; Color black ; a; line_width 2.; Color blue; ctr];;






open Printf
open Oplot.Points.Point2

(* You can see that there is a slight discontinuity near (1,0). This problems
   will disappear by using the optional argument [~better:1]. *)

let f p = heart p.x p.y
let ctr, info = Isocurve.compute_level ~debug:true (* ~better:1 *) f (p0, p1)
let gx, gy = info.grid_size
let txt = sprintf "Initial Grid Size=(%u,%u), depth=%i, steps=%i, \
                   poles=%u"
    gx gy info.depth info.steps info.poles;;

let ltx = latex "$(x^2+y^2-1)^3 - x^2 y^3 = 0$" 1. (-2.25);;

Isocurve.print_info info;;

display [v; Color green; line_width 0.5; info.grid; line_width 1.;
         Color red ; a; Color black;
         Color cyan; info.boxes;
         line_width 2.; Color blue; ctr;
         Color blue; text ~align:LEFT txt (-1.5) 2.25;
         Color black; ltx];;






(* Now some animation of the level sets *)
let f u x y = heart x y -. u;;
let param t = 4. *. sin ((t -. 2.)) ** 5.
let anim t =
  let u = param t in
  implicit_curve (f u) (p0, p1);;

let txt t =
  text ~align:LEFT (sprintf "f(x,y)=%.6f" (param t)) 1. (-2.25);;

Internal.reset_time ();;
display [v; Color black ; a; line_width 2.; Color blue; Anim anim; Anim txt];;



(* Now let the chaos enter! *)

let r = 3.
let p0, p1 = point (-.r, -.r), point (r, r)
let v = view (-.r) (-.r) r r


let f x y = sin (10. *. (heart x y));;

let ctr = implicit_curve ~grid_size:(500,500) f (p0, p1);;

display [v; Color blue; ctr];;

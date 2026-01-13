(* Oplot Level set curve Demo *)

(* Uncomment this to obtain debug information: *)
(* #require "unix";; *)
(* Unix.putenv "OPLOT_DEBUG" "true";; *)

open Oplot
open Plt

(* 1. First we show a "simple" implicit plot (with a nice formula that produces
   a heart-shaped figure!) *)

let heart x y = (((y *. y) +. (x *. x) -. 1.) ** 3.) -. (x *. x *. (y ** 3.))
let p0, p1 = (point (-2., -2.), point (2., 2.))
let v = view (-2.5) (-2.5) 2.5 2.5
let a = axis 0. 0.
let ctr = implicit_curve heart (p0, p1);;

display [ v; Color black; a; line_width 2.; Color blue; ctr ];;

(* 2. Now we simplify the lines. The difference can best be seen by exporting to
   PDF and importing in a vector graphics editor like Inkscape: when you try to
   select the curve, if the curve is not simplified you see that the curve is
   split into hundreds of line segments. *)

display
  [ v; Color black; a; line_width 2.; Color blue; ctr ]
  ~dev:pdf ~output:"heart.pdf"
;;

display
  [
    v;
    Color black;
    a;
    line_width 2.;
    Color blue;
    connect_lines ~epsilon:0.0001 ctr;
  ]
  ~dev:pdf ~output:"heart_simplified.pdf"
;;

(* You can see that there is a slight discontinuity near (1,0). This problems
   will disappear by using the optional argument [~better:1]. *)

let ctr = implicit_curve ~better:1 heart (p0, p1);;

display
  [
    v;
    Color black;
    a;
    line_width 2.;
    Color blue;
    connect_lines ~epsilon:0.0001 ctr;
  ]
  ~dev:pdf ~output:"heart_simplified.pdf"
;;

(* 3. Now we plot the same curve with debug information *)

open Printf
open Oplot.Points.Point2

let f p = heart p.x p.y
let ctr, info = Isocurve.compute_level ~debug:true (* ~better:1 *) f (p0, p1)
let gx, gy = info.grid_size

let txt =
  sprintf "Initial Grid Size=(%u,%u), depth=%i, steps=%i, poles=%u" gx gy
    info.depth info.steps info.poles

let ltx = latex "$(x^2+y^2-1)^3 - x^2 y^3 = 0$" 1. (-2.25);;

Isocurve.print_info info;;

display
  [
    v;
    Color green;
    line_width 0.5;
    info.grid;
    line_width 1.;
    Color red;
    a;
    Color black;
    Color cyan;
    info.boxes;
    line_width 2.;
    Color blue;
    ctr;
    Color blue;
    text ~align:LEFT txt (-1.5) 2.25;
    Color black;
    ltx;
  ]
;;

(* 4. This formula displays very interesting level sets; we can discover them by
   using some animation. *)

let f u x y = heart x y -. u
let param t = 4. *. (sin (t -. 2.) ** 5.)

let anim t =
  let u = param t in
  implicit_curve (f u) (p0, p1)

let txt t = text ~align:LEFT (sprintf "f(x,y)=%.6f" (param t)) 1. (-2.25);;

Internal.reset_time ();;
display [ v; Color black; a; line_width 2.; Color blue; Anim anim; Anim txt ];;

(* 5. Now let the chaos enter! With a slight modification of the formula (using
   a sine function), a single implicit plot becomes amazingly complicated! *)

let r = 3.
let p0, p1 = (point (-.r, -.r), point (r, r))
let v = view (-.r) (-.r) r r
let f x y = sin (10. *. heart x y)
let ctr = implicit_curve ~grid_size:(500, 500) f (p0, p1);;

display [ v; Color blue; ctr ];;
print_endline "Exporting to PDF without connecting the lines...";;

Debug.timeit (fun () ->
    display [ v; Color blue; ctr ] ~dev:pdf ~output:"heart_chaotic.pdf")
;;

(* 6. Simplifying the curves by connecting the lines... be patient because it
   has hundreds of thousands of paths! *)

print_endline "Simplifying curve...";;

let spl = Debug.timeit (fun () -> connect_lines ~epsilon:0.0001 ctr);;

print_endline "Re-Simplifying curve...";;

let spl2 = Debug.timeit (fun () -> connect_lines ~epsilon:0.0001 spl);;

print_endline "Exporting to PDF...";;

Debug.timeit (fun () ->
    display [ v; Color blue; spl ] ~dev:pdf
      ~output:"heart_chaotic_simplified.pdf")
;;

Debug.timeit (fun () ->
    display [ v; Color blue; spl2 ] ~dev:pdf
      ~output:"heart_chaotic_simplified2.pdf")
;;

print_endline "Done.";;
quit ()

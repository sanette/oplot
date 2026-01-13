(* This is part of oplot *)
(* San Vu Ngoc 2025 *)

(* essentially same as v3 but cleaned up *)

(* Plotting the level set of a smooth function *)

open Printf
open Points
open Point2
module C = Common

type grid = {
  h : int; (* number of horizontal cells = #horiz sampling points - 1*)
  v : int; (* number of vertical cells = #vert sampling points - 1 *)
  scale : point; (* x and y scales *)
  origin : point; (* position of the bottom-left of the grid *)
}

type info = {
  grid_size : int * int;
  grid : C.plot_object; (* only if debug=true *)
  boxes : C.plot_object;
  steps : int;
  depth : int;
  poles : int;
  message : Buffer.t;
}

let print_info info =
  let gx, gy = info.grid_size in
  let m = Buffer.contents info.message in
  sprintf
    {|Information about the isocurve execution:
----
| Initial grid size: (%i, %i)
| Maximal Newton steps: %i
| Recursive subsampling depth: %i
| Number of suspected poles: %i%s
----
|}
    gx gy info.steps info.depth info.poles
    (if m = "" then "" else "\n| " ^ m)
  |> print_endline

type vertex = int * int (* (i,j) i = horizontal direction (so column index) *)
type direction = Horizontal | Vertical
type edge = { base : vertex; direction : direction }

type intersection = {
  edge : edge;
  distance : float; (* distance factor in [0,1] from lower-left corner *)
}

type intersection_at_edge = {
  horizontal : intersection option; (* un peu redondant ... à optimiser *)
  vertical : intersection option;
}

let do_option o f = Option.iter f o
let sprint_vertex v = sprintf "(%f, %f)" v.x v.y

let vertex_pos grid (i, j) =
  let x = Float.fma (float i) grid.scale.x grid.origin.x in
  let y = Float.fma (float j) grid.scale.y grid.origin.y in
  { x; y }

let inter_pos grid inter =
  let p = vertex_pos grid inter.edge.base in
  match inter.edge.direction with
  | Horizontal -> { x = Float.fma grid.scale.x inter.distance p.x; y = p.y }
  | Vertical -> { x = p.x; y = Float.fma grid.scale.y inter.distance p.y }

(* WARNING i is the horizontal index (ie. column index), not the line index *)
let sample f grid =
  let a = Array.make_matrix (grid.h + 1) (grid.v + 1) 0. in
  for i = 0 to grid.h do
    for j = 0 to grid.v do
      a.(i).(j) <- f (vertex_pos grid (i, j))
    done
  done;
  a

let sign_changes a =
  let positive = ref true in
  let c = ref 0 in
  let h = Array.length a in
  let v = Array.length a.(0) in
  for i = 0 to h - 1 do
    for j = 0 to v - 1 do
      if a.(i).(j) < 0. then begin
        if !positive then (
          incr c;
          positive := false)
      end
      else begin
        if not !positive then (
          incr c;
          positive := true)
      end
    done
  done;
  !c

let make_grid xmin xmax ymin ymax nx ny =
  let cx = (xmax -. xmin) /. float nx in
  let cy = (ymax -. ymin) /. float ny in
  {
    h = nx;
    v = ny;
    scale = { x = cx; y = cy };
    origin = { x = xmin; y = ymin };
  }

let plot_grid grid =
  let vlines = ref [] in
  let height = float grid.v *. grid.scale.y in
  let width = float grid.h *. grid.scale.x in
  for i = 0 to grid.h do
    let p0 = vertex_pos grid (i, 0) in
    let p1 = { p0 with y = p0.y +. height } in
    vlines := [ p0; p1 ] :: !vlines
  done;
  let hlines = ref [] in
  for j = 0 to grid.v do
    let p0 = vertex_pos grid (0, j) in
    let p1 = { p0 with x = p0.x +. width } in
    hlines := [ p0; p1 ] :: !hlines
  done;
  C.Lines (List.concat [ !vlines; !hlines ])

(* Double the size of the grid; we only compute the new samples *)
let refine f (p0, p1) a0 =
  let gx = Array.length a0 - 1 in
  let gy = Array.length a0.(0) - 1 in
  let grid = make_grid p0.x p1.x p0.y p1.y (2 * gx) (2 * gy) in
  (* sample f grid *)
  let a = Array.make_matrix ((2 * gx) + 1) ((2 * gy) + 1) 0. in
  for i = 0 to (2 * gx) - 1 do
    for j = 0 to (2 * gy) - 1 do
      let y =
        if i mod 2 = 0 && j mod 2 = 0 then a0.(i / 2).(j / 2)
        else f (vertex_pos grid (i, j))
      in
      a.(i).(j) <- y
    done
  done;
  a

let guess_grid_size msg f (p0, p1) (gx, gy) maxgx =
  let rec loop a0 a q0 =
    let gx = Array.length a - 1 in
    if gx > maxgx then begin
      Debug.print "Max grid size reached";
      Buffer.add_string msg
        (sprintf
           "The function seems to be oscillating a lot; the maximum grid size \
            (%u) prevents me from analysing finer details. You should maybe \
            try to zoom to your region of interest.\n"
           maxgx);
      a0
    end
    else
      let s = sign_changes a in
      let q = float s /. float gx in
      Debug.print "[guess_grid_size] gx = %u, q = %f" gx q;
      if q > q0 +. 0.1 (* or less? 0.5 ? *) then loop a (refine f (p0, p1) a) q
      else a0
  in

  let grid = make_grid p0.x p1.x p0.y p1.y gx gy in
  let a0 = sample f grid in
  loop a0 a0 0.

let deriv ?(h = 0.000001) f x = (f (x +. h) -. f x) /. h

type sign_change = Zero of float | Pole of float

(* Find an approximate sign change (zero or pole) of f inside [a,b] (or
   [b,a]). f(a) and f(b) must have different signs, f(b) <> 0.  We find an
   approximate zero in "[a,b]" (two-sided Newton method + linear interpolation
   fallback). steps >= 1. If several zeroes are present, which one is chosen is
   not specified. *)
let zero ~steps ~threshold f a b =
  let rec loop steps (a, fa, da) (b, fb, db) =
    if abs_float fa < threshold then Zero a
    else if abs_float fb < threshold then Zero b
    else
      let z_l =
        match da with
        (* Newton at a *)
        | 0. -> None
        | d ->
            let x = a -. (fa /. d) in
            if x < b && x >= a then Some x else None
      in
      let z_r =
        match db with
        (* Newton at b *)
        | 0. -> None
        | d ->
            let x = b -. (fb /. d) in
            if x < b && x >= a then Some x else None
      in
      let xm = Float.fma (b -. a) (fa /. (fa -. fb)) a in
      (* Linear interpolation *)

      (* Now we select the best candidate *)
      let c, fc =
        match (z_l, z_r) with
        | Some x1, Some x2 ->
            let y1 = f x1 in
            let ay1 = abs_float y1 in
            let y2 = f x2 in
            let ay2 = abs_float y2 in
            let ym = f xm in
            if ay1 <= ay2 then if ay1 <= abs_float ym then (x1, y1) else (xm, ym)
            else if ay2 <= abs_float ym then (x2, y2)
            else (xm, ym)
        | Some x, None | None, Some x ->
            let y = f x in
            let ym = f xm in
            if abs_float y <= abs_float ym then (x, y) else (xm, ym)
        | None, None -> (xm, f xm)
      in

      if abs_float fc <= threshold then Zero c
      else if steps <= 1 then
        if
          (fa *. da > 0. || fb *. db < 0.)
          && Float.(max (abs da) (abs db))
             > 10. (* TODO choose constant instead of 10.?? *)
        then Pole c
        else Zero c
      else
        let steps = steps - 1 in
        let za, zb =
          if fc *. fa < 0. then
            ((a, fa, da), (c, fc, deriv ~h:((a -. c) /. 10.) f c))
          else ((c, fc, deriv ~h:((b -. c) /. 10.) f c), (b, fb, fb))
        in
        loop steps za zb
  in
  loop steps
    (a, f a, deriv ~h:((b -. a) /. 10.) f a)
    (b, f b, deriv ~h:((a -. b) /. 10.) f b)

let gradx f p = deriv (fun t -> f { x = t; y = p.y }) p.x
let grady f p = deriv (fun t -> f { x = p.x; y = t }) p.y
let grad f p = { x = gradx f p; y = grady f p }
let sin_angle v1 v2 = det v1 v2 /. (norm v1 *. norm v2)

(* Find an (approximate) intersection of the curve with the edge. If
   [select_first=true], the first vertex is included, the second one is
   excluded.  Otherwise, the converse holds.  You can think of
   [select_first=true] as an edge oriented from p1 to p2. The source vertex is
   included, the target is not. *)
let edge_intersection poles ~select_first ~steps resolution f grid f_sample edge
    =
  let i0, j0 = edge.base in
  let p0 = vertex_pos grid (i0, j0) in
  let gr = grad f p0 in
  let max_thresh_factor = 1. in
  (* TODO study this constant *)
  let (i1, j1), threshold, g =
    match edge.direction with
    | Horizontal ->
        ( (i0 + 1, j0),
          Float.min
            (max_thresh_factor *. resolution.x)
            (resolution.x *. abs_float gr.x *. 0.001),
          fun t -> f { x = Float.fma grid.scale.x t p0.x; y = p0.y } )
    | Vertical ->
        ( (i0, j0 + 1),
          Float.min
            (max_thresh_factor *. resolution.x)
            (resolution.y *. abs_float gr.y *. 0.001),
          fun t -> f { x = p0.x; y = Float.fma grid.scale.y t p0.y } )
  in
  let f0 = f_sample.(i0).(j0) and f1 = f_sample.(i1).(j1) in
  match select_first with
  | true ->
      if f0 = 0. then Some { edge; distance = 0. }
      else if f0 *. f1 >= 0. then None
      else begin
        match zero ~steps ~threshold g 0. 1. with
        | Zero distance -> Some { edge; distance }
        | Pole _ ->
            incr poles;
            None (* TODO optionally return this anyway *)
      end
  | false ->
      if f1 = 0. then Some { edge; distance = 1. }
      else if f0 *. f1 >= 0. then None
      else begin
        match zero ~steps ~threshold g 0. 1. with
        | Zero distance -> Some { edge; distance }
        | Pole _ ->
            incr poles;
            None
      end

(* We report the intersection [inter] --- which describes the (vertical) edge
   corresponding to the whole [line] vector --- to the appropriate subdivision
   of line. **)
let set_v_boundary select_first i col inter =
  let v = Array.length col - 1 in
  (* number of vertical cells *)
  let r, j = Float.modf (inter.distance *. float v) in
  let j = Float.to_int j in
  let j, distance =
    if r = 0. && not select_first then (j - 1, 1.) else (j, r)
  in
  let edge = { base = (i, j); direction = Vertical } in
  col.(j) <- { (col.(j)) with vertical = Some { edge; distance } }

let set_left_boundary select_first a inter =
  set_v_boundary select_first 0 a.(0) inter

let set_right_boundary select_first a inter =
  let h = Array.length a - 1 in
  (* number of horizontal cells *)
  set_v_boundary select_first h a.(h) inter

let set_h_boundary select_first j a inter =
  let h = Array.length a - 1 in
  let r, i = Float.modf (inter.distance *. float h) in
  let i = Float.to_int i in
  let i, distance =
    if r = 0. && not select_first then (i - 1, 1.) else (i, r)
  in
  let edge = { base = (i, j); direction = Horizontal } in
  a.(i).(j) <- { (a.(i).(j)) with horizontal = Some { edge; distance } }

let set_bottom_boundary select_first a inter =
  set_h_boundary select_first 0 a inter

let set_top_boundary select_first a inter =
  let v = Array.length a.(0) - 1 in
  set_h_boundary select_first v a inter

let set_boundary direct_orientation a (bottom, left, top, right) =
  do_option bottom (set_bottom_boundary direct_orientation a);
  do_option top (set_top_boundary direct_orientation a);
  do_option left (set_left_boundary direct_orientation a);
  do_option right (set_right_boundary direct_orientation a)

(* find all intersections TODO ajouter Boundary Conditions *)
let find_intersections ~steps ?bc resolution f grid f_sample =
  let direct_orientation = true in
  (* TODO *)
  let a =
    Array.make_matrix (grid.h + 1) (grid.v + 1)
      { horizontal = None; vertical = None }
  in
  do_option bc (set_boundary direct_orientation a);
  let poles = ref 0 in
  for j = 0 to grid.v do
    (* vertical index *)
    for i = 0 to grid.h - 1 do
      (* horizontal index *)
      let edge = { base = (i, j); direction = Horizontal } in
      let select_first = (i + j) mod 2 = 0 in
      do_option
        (edge_intersection poles ~select_first ~steps resolution f grid f_sample
           edge) (fun inter ->
          let aij = a.(i).(j) in
          if aij.horizontal = None then
            a.(i).(j) <- { aij with horizontal = Some inter })
    done
  done;
  for i = 0 to grid.h do
    for j = 0 to grid.v - 1 do
      let edge = { base = (i, j); direction = Vertical } in
      let select_first = (i + j) mod 2 = 1 in
      do_option
        (edge_intersection poles ~select_first ~steps resolution f grid f_sample
           edge) (fun inter ->
          let aij = a.(i).(j) in
          if aij.vertical = None then
            a.(i).(j) <- { aij with vertical = Some inter })
    done
  done;
  (a, !poles)

(* Return a list of 0, 1 or 2 pairs of connected intersection points. *)
(* Notice that for each face given by [bottom; left; top; right] due to the
   circular search for intersections, each vertex of the face was selected at
   most once. *)
let connect_full_face ~final_pass f grid (i, j) bottom left top right =
  let list = List.filter_map Fun.id [ bottom; left; top; right ] in
  match list with
  | [] -> Some []
  | [ _ ] -> Some []
  | [ a; b ] ->
      let p1 = inter_pos grid a in
      let p2 = inter_pos grid b in
      let g1 = grad f p1 in
      let g2 = grad f p2 in
      if final_pass || norm2 g1 = 0. || norm2 g2 = 0. then Some [ (a, b) ]
      else begin
        match sin_angle g1 g2 with
        | s when abs_float s < 0.1 (* choose ? *) -> Some [ (a, b) ]
        | _ -> None
      end
  | _ when final_pass ->
      let res =
        if List.length list = 3 then begin
          match (bottom, left, top, right) with
          | None, Some l, Some t, Some r -> [ (l, t); (t, r) ]
          | Some b, None, Some t, Some r -> [ (t, r); (r, b) ]
          | Some b, Some l, None, Some r -> [ (r, b); (b, l) ]
          | Some b, Some l, Some t, None -> [ (b, l); (l, t) ]
          | _ ->
              Debug.print "Error !! at (%d,%d)" i j;
              []
        end
        else begin
          Debug.print
            "Isocurve: Quadruple intersection at (i,j)=(%d,%d)   (x,y)=%s" i j
            (sprint_vertex (vertex_pos grid (i, j)));
          match (bottom, left, top, right) with
          | Some b, Some l, Some t, Some r ->
              (* There are three possible choices (incl. one with
             crossing). We use gradient to decide. *)
              let pb = inter_pos grid b in
              let pl = inter_pos grid l in
              let pt = inter_pos grid t in
              let pr = inter_pos grid r in
              let g = grad f pb in
              let vbl = sub pl pb in
              let vbt = sub pt pb in
              let vbr = sub pr pb in
              let dl = abs_float (dot g vbl) /. norm vbl in
              let dt = abs_float (dot g vbt) /. norm vbt in
              let dr = abs_float (dot g vbr) /. norm vbr in
              if dl <= dt then
                if dl <= dr then [ (b, l); (r, t) ] else [ (r, b); (t, l) ]
              else if dt <= dr then [ (b, t); (r, l) ]
              else [ (r, b); (t, l) ]
          | _ -> []
        end
      in
      Some res
  | _ -> None

let connect_all ?(final_pass = true) f grid a =
  let list = ref [] in
  let new_pass = ref [] in
  for j = 0 to grid.v - 1 do
    for i = 0 to grid.h - 1 do
      let bottom = a.(i).(j).horizontal in
      let left = a.(i).(j).vertical in
      let top = a.(i).(j + 1).horizontal in
      let right = a.(i + 1).(j).vertical in
      let cons =
        connect_full_face ~final_pass f grid (i, j) bottom left top right
      in
      match cons with
      | Some c -> list := List.rev_append c !list
      | None ->
          let bc = (bottom, left, top, right) in
          new_pass := ((i, j), bc) :: !new_pass
    done
  done;
  let seg_list : point list list =
    List.map (fun (i1, i2) -> [ inter_pos grid i1; inter_pos grid i2 ]) !list
  in
  (seg_list, !new_pass)

let rec pass_loop poles acc_plist ~debug ~steps ~depth resolution f size
    pass_size = function
  | [] -> []
  | (p0, p1, _bc) :: _rest as plist ->
      let nx, ny = size in
      let cell_size =
        {
          x = abs_float (p1.x -. p0.x) /. float nx;
          y = abs_float (p1.y -. p0.y) /. float ny;
        }
      in
      let final_pass =
        cell_size.x <= resolution.x
        || cell_size.y <= resolution.y
        || pass_size = (1, 1)
      in
      let n = List.length plist in
      Debug.print "Loop over %u cell%s. Cell size = (%g, %g). Final pass = %b" n
        (if n > 1 then "s" else "")
        cell_size.x cell_size.y final_pass;
      List.fold_left
        (fun list p ->
          List.rev_append
            (cell_loop poles acc_plist ~debug ~final_pass ~steps ~depth
               resolution f size pass_size p)
            list)
        [] plist

and cell_loop poles acc_plist ~debug ~final_pass ~steps ~depth resolution f size
    pass_size (p0, p1, bc) : C.points list =
  let nx, ny = size in
  Debug.print "  *  findind contour in cell: (%f,%f,%f,%f) size=(%u,%u)" p0.x
    p0.y p1.x p1.y nx ny;
  let grid = make_grid p0.x p1.x p0.y p1.y nx ny in
  let f_sample = sample f grid in
  let a, pol = find_intersections ?bc ~steps resolution f grid f_sample in
  poles := !poles + pol;
  let final_pass = final_pass || depth <= 1 in
  let list, new_pass = connect_all ~final_pass f grid a in
  let new_plist =
    List.map
      (fun ((i, j), bc) ->
        (vertex_pos grid (i, j), vertex_pos grid (i + 1, j + 1), Some bc))
      new_pass
  in
  if debug then acc_plist := List.rev_append new_plist !acc_plist;
  let llist =
    pass_loop poles acc_plist ~debug ~steps ~depth:(depth - 1) resolution f
      pass_size pass_size new_plist
  in
  List.rev_append llist list

let box p0 p1 = [ p0; { p0 with x = p1.x }; p1; { p0 with y = p1.y }; p0 ]

(* call this function to create a User structure that oplot can display *)
(* TODO: optionnally pass the gradient to speed up computations *)
(* todo MEMOIZE wrt view *)
let compute_level ?(debug = false)
    ?(pixel_size = (!Oplotdef.window_width, !Oplotdef.window_height)) ?grid_size
    ?(sub_size = (2, 2)) ?(steps = 4) ?(better = 0) ?depth f v :
    C.plot_object * info =
  let message = Buffer.create 80 in
  let p0, p1 = v in
  let width, height = pixel_size in
  let gx, gy =
    match grid_size with
    | Some g -> g
    | None ->
        let gxmax = width / 2 in
        let f_sample = guess_grid_size message f v (34, 34) gxmax in
        (* TODO reuse of memoize f_sample *)
        let gx = Array.length f_sample - 1 in
        (gx, gx)
  in
  (* An even number is usually nicer when the functions is
                     symmetric around the origin. TODO non square grid ? *)
  let resolution =
    {
      x = abs_float (p1.x -. p0.x) /. float width;
      y = abs_float (p1.y -. p0.y) /. float height;
    }
  in
  let max_depth =
    log (float width /. float gx) /. log (float (fst sub_size))
    |> Float.ceil
    |> Float.to_int
  in
  let depth =
    match depth with
    | Some d when d > max_depth ->
        Debug.print "Requested depth %u is too high, using %u" d max_depth;
        max_depth
    | Some d -> d
    | None -> max_depth
  in
  let depth = depth + better in
  let steps = steps + (2 * better) in

  let acc_plist = ref [] in
  let poles = ref 0 in
  let llist : C.points list =
    pass_loop ~debug poles acc_plist ~steps ~depth resolution f (gx, gy)
      sub_size
      [ (p0, p1, None) ]
  in
  let boxes = List.map (fun (p0, p1, _) -> box p0 p1) !acc_plist in
  if !poles > 0 then begin
    let a, b, c =
      if !poles = 1 then ("one sign change has", "a pole", "a zero")
      else (sprintf "%u sign changes have" !poles, "poles", "zeroes")
    in
    let msg =
      sprintf
        "Warning, %s been considered as %s and hence not reported as %s. If \
         you think this is not true you should increase resolution or zoom to \
         a smaller region.\n"
        a b c
    in
    Buffer.add_string message msg
  end;
  let grid =
    if debug then plot_grid (make_grid p0.x p1.x p0.y p1.y gx gy) else Lines []
  in
  let info =
    {
      grid_size = (gx, gy);
      grid;
      boxes = Lines boxes;
      depth;
      steps;
      message;
      poles = !poles;
    }
  in
  (Lines llist, info)

(*
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

(* essayer le flot du gradient du carré pour approcher la surface ?*)
(* puis détecter le degré de la courbe au hasard? *)
(* puis orienter les cellules sur le plan tangent ? *)

(* autre idée: partir d'un pavé (ou 1/2 pavé: 3 faces), et le déformer par le
   flot gradient. Il s'agit de savoir comment déformer un plan lorsqu'il se tord *)

open Points
open Point3

let debug = ref true

type final_edge_prop = | Intact
		       | Intersect of point3
and vertex = { coords:point3; f:float }   
and edge = { vertex_1:vertex ref;  vertex_2:vertex ref; mutable props:final_edge_prop }
    
type tetrahedron = { edgeAB:edge ref; edgeBC:edge ref; edgeCA:edge ref;
		     edgeDA:edge ref; edgeDB:edge ref; edgeDC:edge ref}
    (* décomposition de tétrahèdres *)    

(* pour limiter la mémoire, les sommets et arêtes sont stockés sous forme de
   pointeurs, qui peuvent être partagés~: un sommet peut appartenir à plusieurs
   arêtes, une arête peut appartenir à plusieurs tetrahèdres. Si l'un est modifié,
   la modification est faite pour tous ceux qui le partagent. *)

let edge_extrem e = e.vertex_1, e.vertex_2

let edge_of_index tetra i = match i with
  | 0 -> tetra.edgeAB
  | 1 -> tetra.edgeBC
  | 2 -> tetra.edgeCA
  | 3 -> tetra.edgeDA
  | 4 -> tetra.edgeDB
  | 5 -> tetra.edgeDC
  | j -> failwith (Printf.sprintf "There is no edge number %d !" j)

(* essai tracé à compiler avec oplot *)
open Oplot
open Oplotmain

let p1={x=(-2.); y=(-2.); z=(-2.)};;
let p2={x=2.; y=2.; z=2.};;

let draw_point p = GlDraw.vertex3 (p.x, p.y, p.z)

let of_point p = p.x,p.y,p.z

let normal list =
  match List.map of_point list with
    | [a;b;c] -> unit_normal a b c
    | _ -> failwith "Hmm.. should be 3 points..."
	
let draw_triangle list =
  GlDraw.begins `triangles;
  GlDraw.normal3 (normal list); (* ne marche pas ? *)
  let draw_vertex p =
    let c = p.z in 
      GlDraw.color (c, c, c);
      draw_point p in
    List.iter draw_vertex list;
    GlDraw.ends ()
      
let draw_triangles list = 
  enter3d (p1,p2);
  List.iter draw_triangle list;
  leave3d ()
    
let draw_edge edge =
  let (v1,v2) = edge_extrem edge in
    GlDraw.begins `lines;
    draw_point !v1.coords;
    draw_point !v2.coords;
    GlDraw.ends ()

let draw_tetra tetra =
  enter3d (p1,p2);
  GlDraw.color (1., 0., 0.);
  List.iter (fun i -> draw_edge !(edge_of_index tetra i)) [ 0;1;2;3;4;5 ];
  leave3d ()

(* fin tracé *)

(* fonction test pour isosurface f=0. A passer en foncteur ? *)    
let f = ref (fun p -> abs_float (p.x) -. 0.5)

let make_vertex p = ref { coords=p; f=(!f p) }

let make_dull_vertex () = ref { coords={x=0.; y=0.; z=0.}; f=0. }

let make_edge p1 p2 = ref { vertex_1=p1; vertex_2=p2; props=Intact }

let norm e = 
  let (p1,p2) = !(e.vertex_1).coords, !(e.vertex_2).coords in
    (abs_float (p1.x -. p2.x) +.
       abs_float (p1.y -. p2.y) +.
       abs_float (p1.z -. p2.z))
(*    /. ( 1. +. abs_float !(e.vertex_1).f +. abs_float !(e.vertex_2).f ) *)
      
let max_edge tetra : edge ref * int * float =
  let edge0 = tetra.edgeAB in
  let edges = [ tetra.edgeBC; tetra.edgeCA; 
		tetra.edgeDA; tetra.edgeDB; tetra.edgeDC ] in
  let i = ref 0 in (* on numérote les arêtes de 0 à 5 *)
  let edges_norms = List.rev_map (fun e -> incr i; (e, !i, norm !e)) edges in
    List.fold_left (fun (e1,j1,n1) (e2,j2,n2) -> if n1<n2 then (e2,j2,n2) else (e1,j1,n1))
      (edge0, 0, norm !edge0) edges_norms

let median e = 
  let (p1,p2) = !(e.vertex_1).coords, !(e.vertex_2).coords in
  let p = { x=(p1.x +. p2.x)/.2.;
	    y=(p1.y +. p2.y)/.2.;
	    z=(p1.z +. p2.z)/.2. } in
    ref { coords=p; f=(!f p) }
    
let vertices tetra = 
  let (a,b) = edge_extrem !(tetra.edgeAB)
  and (d,c) = edge_extrem !(tetra.edgeDC) in
    (a,b,c,d)

let make_tetra a b c d =
  {edgeAB = make_edge a b; edgeBC = make_edge b c; edgeCA = make_edge c a;
   edgeDA = make_edge d a; edgeDB = make_edge d b; edgeDC = make_edge d c}

let tetrahedron p1 p2 p3 p4 = 
  let (a,b,c,d) = (make_vertex p1, make_vertex p2, make_vertex p3, make_vertex p4) in
    make_tetra a b c d
      
let print_point p = Printf.sprintf "(x=%f, y=%f, z=%f)" p.x p.y p.z


(* creation des modèles de triangulation a=8, b=4, c=2, d=1. Arêtes 0,1,2,3,4,5 *)
let triangles = Array.make 16 [[]];;

triangles.(8) <- [[0;2;3]];
triangles.(4) <- [[1;0;4]];
triangles.(2) <- [[2;1;5]];
triangles.(1) <- [[3;5;4]];

triangles.(7) <- triangles.(8);
triangles.(11) <- triangles.(4);
triangles.(13) <- triangles.(2);
triangles.(14) <- triangles.(1);

triangles.(12) <- [[2;4;1];[4;2;3]];
triangles.(6) <- [[0;2;5];[5;4;0]];
triangles.(10) <- [[1;5;3];[3;0;1]];

triangles.(3) <- triangles.(12);
triangles.(9) <- triangles.(6);
triangles.(5) <- triangles.(10);

triangles.(0) <- [[]];
triangles.(15) <- [[]];;
      
(* intersection (barycentre correspondant à la linéarisation de f) de l'arête i du tetra *)
let intersect tetra i =
  let edge = !(edge_of_index tetra i) in
    match edge.props with
      | Intersect p -> p
      | Intact -> let (p1,p2,f1,f2) = !(edge.vertex_1).coords, !(edge.vertex_2).coords, !(edge.vertex_1).f, !(edge.vertex_2).f in
	  if f1 *. f2 > 0. then failwith "Wrong edge !"
	  else
	    let p t = {x = p1.x *. (1. -. t) +. t *. p2.x;
		       y = p1.y *. (1. -. t) +. t *. p2.y;
		       z = p1.z *. (1. -. t) +. t *. p2.z} in
	    let g t = !f (p t) in
	    let t_inter =
	      let t0 = 1. /. (1. -. f2 /. f1) in
	      let g0 = g t0 in
	      let h = 0.001 in
	      let newton = t0 -. h *. g0 /. (g (t0 +. h) -. g0) in
		if newton > 0. && newton <= 1. && abs_float g0 > abs_float (g newton) then newton else t0 in
	    let p_inter = p t_inter in
	      edge.props <- Intersect p_inter;
	      if !debug && abs_float (g t_inter) > 0.1 then print_endline ("Uh oh");
	      p_inter
		
let make_triangle tetra triangle = List.map (intersect tetra) triangle  

let make_triangles tetra triangles = List.map (make_triangle tetra) triangles
 
let interior vertex = !vertex.f < 0.

(* à changer pour fournir plutôt la liste des indices des points au lieu des points eux-mêmes ? *)
let triangulate tetra =
  if not !debug then  draw_tetra tetra;
  let (a,b,c,d) = vertices tetra in
    if !debug then begin
      Printf.printf "Final tetrahedron: A=%s\tB=%s\tC=%s\tD=%s\n"
	(print_point !a.coords) (print_point !b.coords) (print_point !c.coords) (print_point !d.coords);
      let (e,i,n) = max_edge tetra in
	Printf.printf "Size: %f, " n
    end;
    let index = (if interior a then 8 else 0) +
      (if interior b then 4 else 0) +
      (if interior c then 2 else 0) +
      (if interior d then 1 else 0) in
      if !debug then Printf.printf "Code: %d\n\n" index;
      make_triangles tetra (triangles.(index))
	
(* sépare en deux tetrahèdres en coupant l'arète i correspondant à edge *)
let separate tetra i edge = 
  let (a,b,c,d) = vertices tetra
  and m = median !edge in
    match i with
      | 0 -> ({tetra with edgeAB = make_edge a m; edgeBC = make_edge m c; edgeDB = make_edge d m}, (* m -> b *)
	      {tetra with edgeAB = make_edge m b; edgeCA = make_edge c m; edgeDA = make_edge d m}) (* m -> a *)
      | 1 -> ({tetra with edgeBC = make_edge b m; edgeCA = make_edge m a; edgeDC = make_edge d m}, (* m -> c *)
	      {tetra with edgeAB = make_edge a m; edgeBC = make_edge m c; edgeDB = make_edge d m}) (* m -> b *)
      | 2 -> ({tetra with edgeAB = make_edge m b; edgeCA = make_edge c m; edgeDA = make_edge d m}, (* m -> a *)
	      {tetra with edgeBC = make_edge b m; edgeCA = make_edge m a; edgeDC = make_edge d m}) (* m -> c *)
      | 3 -> ({tetra with edgeAB = make_edge m b; edgeCA = make_edge c m; edgeDA = make_edge d m}, (* m -> a *)
	      {tetra with edgeDA = make_edge m a; edgeDB = make_edge m b; edgeDC = make_edge m c}) (* m -> d *)
      | 4 -> ({tetra with edgeAB = make_edge a m; edgeBC = make_edge m c; edgeDB = make_edge d m}, (* m -> b *)
	      {tetra with edgeDA = make_edge m a; edgeDB = make_edge m b; edgeDC = make_edge m c}) (* m -> d *)
      | 5 -> ({tetra with edgeBC = make_edge b m; edgeCA = make_edge m a; edgeDC = make_edge d m}, (* m -> c *)
	      {tetra with edgeDA = make_edge m a; edgeDB = make_edge m b; edgeDC = make_edge m c}) (* m -> d *)
      | j -> failwith (Printf.sprintf "There should not be a face number %d !" i)
	  (* bien sûr on peut un peu factoriser... *)    
	 
(* isosurface par décomposition en tétraèdres... ne marche pas bien après la
   deuxième décomposition (n>=2) *)
let tesselate tetra ~nmax ~delta = 
  let list = ref [] in
  let rec loop tetr nma =
    let (e,i,n) = max_edge tetr in
      if n <= delta || nma = 0 then begin
	let new_triang = triangulate tetr in
	  if new_triang <> [[]] then list := new_triang::!list
      end
      else let (tetra1,tetra2) = separate tetr i e in
	loop tetra1 (nma-1);
	loop tetra2 (nma-1) in
    loop tetra nmax;
    List.flatten !list
      
      
(*******************************************************************************************)
(* essai *)

let initial_tetra = tetrahedron {x=(1.); y=(-0.3); z=(-0.3)} {x=0.; y=1.; z=(-0.3)} {x=(-1.); y=(-0.3); z=(-0.3)} {x=0.; y=(-0.3); z=1.};;

(* let tess = tesselate initial_tetra ~nmax:4 ~delta:0.1;; *)

(*******************************************************************************************)



let tess = ref [[]];;
let te = User (fun _ _ -> tess:=tesselate initial_tetra ~nmax:4 ~delta:0.1);;
let t = User (fun _ _ -> draw_tetra initial_tetra);;
let u = User (fun _ _ -> draw_triangles !tess);;

let v = view (-2.) (-2.) 2. 2.;;
(* display [v; axis 0. 0.; te; t; u];; *)
(*
open Isosurf;;
open Oplot;;
open Oplotmain;;
open Points;;
open Point3;;

debug:=false;;
let te = User (fun _ _ -> tess:=tesselate initial_tetra ~nmax:99 ~delta:0.8);;
display [v; axis 0. 0.; te; t; u];;

f:=(fun p -> p.x ** 2. +. p.y ** 2. -. 0.07);;
let initial_tetra = tetrahedron {x=(1.); y=(-0.3); z=(-0.3)} {x=0.; y=1.; z=(-0.3)} {x=(-1.); y=(-0.3); z=(-0.3)} {x=0.; y=(-0.3); z=1.};;
let t = User (fun _ _ -> draw_tetra initial_tetra);;

tess:=tesselate initial_tetra ~nmax:99 ~delta:0.2;;

display [v; axis 0. 0.; t; u];;

*)

(*
f:=(fun p -> p.x -. 1.);;
*)


(***************************************************************************)
(* decomposition du cube en tetrahèdres *)


type cube = vertex array

let tetra_of_face cube list = match list with
  | [i;j;k;l] -> make_tetra cube.(i) cube.(j) cube.(k) cube.(l)
  | _ -> failwith "This shoud be a list of 4 indices"

let tetra_of_cube cube =
  let decomposition = 
    [[2;3;7;1];
     [0;1;7;3];
     [6;2;1;7];
     [0;7;1;4];
     [1;5;4;7];
     [1;5;7;6]] in
    List.map (tetra_of_face cube) decomposition
    
let tesselate_cube cube ~nmax ~delta =
  let tetras = tetra_of_cube cube in
    List.map (tesselate ~nmax ~delta) tetras

let draw_triangles_list list = 
  enter3d (p1,p2);
  List.iter (List.iter draw_triangle) list;
  leave3d ()

let make_point (x, y, z) = {x=x; y=y; z=z}

(* essai *)
let cube0 = let a = Array.make 8 (make_dull_vertex ()) in
  List.iter (fun (i,c) -> a.(i) <- make_vertex (make_point c))
    [0, (1., -1., 1.);
     1, (1., 1., 1.);
     2, (-1., 1., 1.);
     3, (-1., -1., 1.);
     4, (1., -1., -1.);
     5, (1., 1., -1.);
     6, (-1., 1., -1.);
     7, (-1., -1., -1.)];
  a

   
(***************************************************************************)
(* parcours du cube ('marching cube/tetrahedron') *)

let isosurface ?(ff = !f) ?(nx = 15) ?(ny = 15) ?(nz = 15) ?(depth = 1) p1 p2 =
(* nx ny nz = nombres de subdivisions (=nombre de faces du cube) selon les axes *)

  f:=ff;
(* tableau temporaires pour stocker les calculs qui vont reservir *)
  let cube =  Array.make 8 (make_dull_vertex ()) in
  let face1 = Array.make (nx+1) (make_dull_vertex ()) in
  let face2 = Array.make_matrix (ny+1) (nx+1) (make_dull_vertex ()) in
    
  let marching {x=xmin; y=ymin; z=zmin} {x=xmax; y=ymax; z=zmax} = 
    let final_tess = ref [] in
    let dx = (xmax -. xmin)/.(float nx)
    and dy = (ymax -. ymin)/.(float ny)
    and dz = (zmax -. zmin)/.(float nz) in
    let delta = (min (abs_float dx) (min (abs_float dy) (abs_float dz))) /. (float depth) in (* diviser par depth ? *)
    let p i j k = {x=xmin +. dx *. (float i); y=ymin +. dy *. (float j); z=zmin +. dz *. (float k)} in
      (* initialisation *)
      for j = 0 to ny do
	let a = face2.(j) in
	  for i = 0 to nx do a.(i) <- make_vertex (p i j 0) done
      done;
      (* loop *)
      for k = 1 to nz do
	for i = 0 to nx do face1.(i) <- make_vertex (p i 0 k) done;
	for j = 0 to ny-1 do
	  let face2j = face2.(j) and
	      face2j' = face2.(j+1) in
	    for i = 1 to nx do
	      cube.(4) <- face2j.(i-1);
	      cube.(5) <- face2j.(i);
	      cube.(6) <- face2j'.(i);
	      cube.(7) <- face2j'.(i-1);
	      cube.(0) <- face1.(i-1);
	      cube.(1) <- face1.(i);
	      (* new vertices *)
	      cube.(2) <- make_vertex (p i (j+1) k);
	      cube.(3) <- make_vertex (p (i-1) (j+1) k);
	      (* tesselate *)
	      begin
		let tess = tesselate_cube cube ~nmax:depth ~delta in (* changer nmax en fonction de la courbure moyenne *)
		  final_tess := List.rev_append (List.flatten tess) !final_tess
	      end;
	      (* turnover *)
	      face2j.(i-1) <- cube.(0);
	      face1.(i-1) <- cube.(3);
	    done;
	    face2j.(nx) <- cube.(1);
	    face1.(nx) <- cube.(2);
	done;
	let a = face2.(ny) in for i = 0 to nx do a.(i) <- face1.(i) done;
      done;
      Printf.printf "Number of triangles: %d\n" (List.length !final_tess);
      !final_tess in
    marching p1 p2

(* essai *)
      
let p1 = {x=(-1.); y=(-1.); z=(-1.)}
  
let p2 = {x=(1.); y=(1.); z=(1.)}

(*
 load oplot.sh and then
open Isosurf;;
open Oplotmain;;
open Oplot;;
open Points;;
open Point3;;

debug:=false;;
 
   tess:=isosurface p1 p2;;
   display [v; axis 0. 0.; u];;
   tess:=isosurface ~depth:0 p1 p2 ~nx:20 ~ny:20 ~nz:20;;
   display [v; axis 0. 0.; u];;

*)

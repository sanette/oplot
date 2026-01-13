(* This file is part of oplot.

   Implementation of the "path" data structure.

   Here paths are not oriented.
   paths with matching endpoints can be concatenated.

   A set of paths is more or less like a graph (edges and vertices), but:
   + edges are themselves lists of points, the vertices are the end points of paths.
   + vertices are points in a metrix space:
     there is a notion of points "being close to each other".

   In terms of algorithms, the idea is close to the one of "union-find", at two levels:
   + we unite points that are close to each other,
     resulting in a partition of the set of "vertices".
   + we unite paths that can be concatenated,
     resulting in a partition into connected compoments.

   The practical goal of this module is to "simplfy" Lines objects by connecting
   as much as possible: [a,b] [b,c,d] [d,e] --> [a,b,c,d,e]
   (The implicit_curve functions produces myriads of such small pairs.)
*)

(* Only for testing; comment out *)
module Debug = struct
  let debug = true
  let print s = Printf.ksprintf (fun s -> if debug then print_endline s) s
end

(** This is the core Path signature, which can be implemented without knowing
    the specific type of point. *)
module type PATH = sig
  type point

  type t
  (** A path is an object with two ends (called left and right). Two paths [p1]
      and [p2] can be concatenated (in this order) if the right end of [p1] is
      "equal" to the left end of [p2]. The equality function is parameterized by
      the user, see {!concat}. *)

  val left : t -> point
  val right : t -> point
  val ends : t -> point * point

  val of_list : point list -> t
  (** [of_list [a,b,c,...,z]] constructs a path with left end equal to [a] and
      right end equal to [z]. *)

  val of_pair : point * point -> t
  (** [of_pair (a,b)] is the same as [of_list [a;b]]. *)

  val to_list : t -> point list
  (** [to_list p] is a list that starts with [left p] and ends with [right p].
      It is the inverse of [of_list]. *)

  val reverse : t -> t
  (** Reverse the orientation of the path. Applying [reverse] twice yields back
      the original path. *)

  val concat : t -> t -> t
  (** [concat p1 p2] is the path obtained by appending p2 to the right of p1,
      after removing either the left element of p2 or the right element of p1.
      In principe it should be applied only once you know that the left element
      of p2 is "equal" to the right element of p1. *)

  val size : t -> int
  (** [size p] the number of points in [to_list p] minus 1. *)
end

(** A point is an object that can be distributed in a "grid": for instance float
    are points, the grid being given by taking the integer part. If two points
    are in the same cell, they may be united (i.e. considered as equal). *)
module type POINT = sig
  type t
  type cell

  type same = t -> t -> bool
  (** An equality function is used to determine whether two points can be
      considered equal. It is reflexive (same a a = true) and symmetric (same a
      b = same b a) but does not need to be transitive (typically, we check
      whether the distance between two point is less than epsilon). *)

  val string : t -> string

  val same : float -> same
  (** [same epsilon a b] is true when [a] and [b] are [epsilon] close, in some
      sense. *)

  val hint : float -> t -> cell
  (** If two elements have the same [hint epsilon] then they must be epsilon
      close to each other: [same epsilon a b = true]. *)

  val neigh : cell -> cell list
  (** Neighbouring cells: if [same a b] is true, and [key = hint epsilon a] then
      [hint epsilon b] must be one of the cells of [neigh key]. *)
end

(** A Component module contains functions to concatenate a group of paths into
    connected components. *)
module type COMPONENT = functor
  (P : POINT)
  (Path : PATH with type point := P.t)
  -> sig
  val concat_lists : float -> P.t list list -> P.t list list
  (** Main function if you don't use the S module. *)

  type t
  (** A "component" is a maximal collection of paths: no two paths can be
      concatenated to each other.*)

  val to_list : t -> P.t list list
  val get_epsilon : t -> float

  val check_concat : float -> Path.t -> Path.t -> bool
  (** Check whether the two paths can be concatenated in this order *)

  val concat : float -> Path.t -> Path.t -> Path.t option
  (** Concatenate the paths, if possible. *)

  val check_component : float -> Path.t list -> bool
  (** Check whether a list of paths is a valid "component", that is: no two
      paths can be concatenated to each other. *)

  (* val add_path : t -> Path.t -> t *)
  val concat_components : float -> t list -> t
  val concat_pairs : float -> (P.t * P.t) list -> t
  val make_component : float -> Path.t list -> t
  val contents : t -> Path.t list
end

(* Examples of Point modules which *extend* the POINT signature with a concrete
   type. (So we should *not* enforce POINT signature.) *)
module Point2d = struct
  (** 2-dimensional points with sup norm. *)

  type t = Points.Point2.t

  open Points.Point2

  type same = t -> t -> bool

  let string z = Printf.sprintf "(%g, %g)" z.x z.y

  type cell = int * int

  let key i j = (i lsl 16) lor j
  (* TODO use this key for optimizing
                                    hashtables. But we need to make sure i and j
                                    are less than 2^(13 or 14?) to have an
                                    injective key. *)

  let same epsilon z1 z2 =
    abs_float (z1.x -. z2.x) < epsilon
    &&
    (* comme ça on évite de faire le
                                             deuxième calcul si ce test est déjà
                                             mauvais. **)
    abs_float (z1.y -. z2.y) < epsilon

  let hint epsilon z =
    (Float.to_int (z.x /. epsilon), Float.to_int (z.y /. epsilon))

  let neigh (i0, j0) =
    [
      (i0, j0);
      (i0 + 1, j0 + 1);
      (i0 + 1, j0);
      (i0 + 1, j0 - 1);
      (i0, j0 - 1);
      (i0 - 1, j0 - 1);
      (i0 - 1, j0);
      (i0 - 1, j0 + 1);
      (i0, j0 + 1);
    ]
end

(* TODO points in phase space => we joint only path in a C1 fashion. On n'a
   besoin de la vitesse que pour les extrêmités, donc pour l'intérieur on
   pourrait rendre le vecteur vitesse optionnel. *)

module PointInt = struct
  (** Points are simply integers: mainly for testing. Athough not used here,
      morally epsilon is 1. *)

  type t = int
  type same = t -> t -> bool

  let string = Int.to_string

  type cell = int

  let same _ = Int.equal
  let hint _ z = z
  let neigh z = [ z ]
end

let rec end_of_list = function
  | [] -> raise (invalid_arg "end_of_list")
  | [ a ] -> a
  | _ :: rest -> end_of_list rest

(* A simple implementation using Lists. We use P only for the type P.t, so
   instead we could write "type 'a path = ... " *)
module List_path (P : POINT) : PATH with type point := P.t = struct
  type point = P.t

  type t = { left : point; content : point list; right : point }
  (** For the path [{a...b}], [content] contains [b] but not [a], exept for the
      singleton [{a}] for which [content] = []. *)

  let left p = p.left
  let right p = p.right
  let ends p = (p.left, p.right)
  let of_pair (a, b) = { left = a; content = [ b ]; right = b }

  let of_list = function
    | a :: tail as l -> { left = a; content = tail; right = end_of_list l }
    | _ -> raise (invalid_arg "[path_of_list]: list should not be empty.")

  let to_list p = p.left :: p.content

  let reverse p =
    let content =
      match p.content with [] -> [] | c -> List.tl (List.rev (p.left :: c))
    in
    { left = p.right; right = p.left; content }

  let concat p1 p2 =
    {
      left = p1.left;
      right = p2.right;
      content = List.append p1.content p2.content;
    }

  (** Measure length *)
  let _length distance p =
    let rec loop d p0 = function
      | [] -> d
      | p1 :: rest -> loop (d +. distance p0 p1) p1 rest
    in
    loop 0. (left p) p.content

  (** Number of segments *)
  let size p = List.length p.content
end

(* A (better?) implementation using Oriented Lists *)
module OList_path (P : POINT) : PATH with type point := P.t = struct
  type point = P.t

  type t = { left : point; content : point list; right : point; reverse : bool }
  (** For the path [{abcdef}], if [reverse=false], then [left]=a,
      [content]=bcde, [right]=f. The singleton [{a}] is forbidden *)

  let left p = if p.reverse then p.right else p.left
  let right p = if p.reverse then p.left else p.right
  let ends p = if p.reverse then (p.right, p.left) else (p.left, p.right)
  let of_pair (a, b) = { left = a; content = []; right = b; reverse = false }

  let of_list = function
    | [ _ ] -> raise (invalid_arg "Olist_path.of_list")
    | a :: tail ->
        let r = List.rev tail in
        { left = List.hd r; content = List.tl r; right = a; reverse = true }
    | _ -> raise (invalid_arg "[path_of_list]: list should not be empty.")

  let to_list p =
    if p.reverse then p.right :: List.rev (p.left :: p.content)
    else List.append (p.left :: p.content) [ p.right ]

  let reverse p = { p with reverse = not p.reverse }

  let concat p1 p2 =
    match (p1.reverse, p2.reverse) with
    | true, false ->
        {
          left = p1.right;
          content = List.rev_append p1.content (p2.left :: p2.content);
          right = p2.right;
          reverse = false;
        }
    | false, true ->
        {
          left = p1.left;
          content = List.append p1.content (p2.right :: List.rev p2.content);
          right = p2.left;
          reverse = false;
        }
    | false, false ->
        {
          left = p1.left;
          content = List.append p1.content (p2.left :: p2.content);
          right = p2.right;
          reverse = false;
        }
    | true, true ->
        {
          left = p2.left;
          content = List.append p2.content (p1.left :: p1.content);
          right = p1.right;
          reverse = true;
        }

  (** Measure length *)
  let _length distance p =
    let rec loop d p0 = function
      | [] -> d
      | p1 :: rest -> loop (d +. distance p0 p1) p1 rest
    in
    loop 0. (left p) p.content

  (** Number of segments *)
  let size p = List.length p.content + 1
end

module Tree_path (P : POINT) : PATH with type point := P.t = struct
  type point = P.t

  type tree =
    | Leaf of point
    | Append of tree * tree
    | Reverse of tree
    | Skip_first of tree
  (* | Skip_last of tree *)

  type t = { first : point; last : point; tree : tree }

  let left p = p.first
  let right p = p.last
  let ends p = (p.first, p.last)

  let reverse p =
    {
      first = p.last;
      last = p.first;
      tree =
        (* Reverse p.tree *)
        (match p.tree with
        | Reverse t -> t
        | t -> Reverse t);
    }

  (* This is O(N) and will compute Reverse (t) while removing all "Reverse"
     keywords.  *)
  (* let _reverse_deep p = *)
  (*   let rec loop = function *)
  (*     | Leaf _ as t -> t *)
  (*     | Append(a,b) -> Append(loop b,loop a) *)
  (*     | Reverse t -> t *)
  (*     | Skip_first t -> Skip_last (loop t) *)
  (*     | Skip_last t -> Skip_first (loop t) *)
  (*   in *)
  (*   { first = p.last; *)
  (*     last = p.first; *)
  (*     tree = loop p.tree } *)

  let of_list = function
    | [] -> raise (invalid_arg "of_list")
    | x :: rest ->
        let first = x in
        let rec loop last tree = function
          | [] -> (last, tree)
          | y :: ys -> loop y (Append (tree, Leaf y)) ys
        in
        let last, tree = loop x (Leaf x) rest in
        { first; last; tree }

  let of_pair (x, y) = { first = x; last = y; tree = Append (Leaf x, Leaf y) }

  let rec to_list_tree acc = function
    | Leaf x -> x :: acc
    | Append (a, b) -> to_list_tree (to_list_tree acc b) a
    (* right first, then left, so this is consistent with Append(Leaf x, Leaf y)
       --> [x ; y] *)
    | Reverse t -> to_list_tree_rev acc t
    | Skip_first t -> drop_first acc t
  (* | Skip_last t -> drop_last acc t *)

  and to_list_tree_rev acc = function
    | Leaf x -> x :: acc
    | Append (a, b) -> to_list_tree_rev (to_list_tree_rev acc a) b
    | Reverse t -> to_list_tree acc t
    | Skip_first t -> drop_last acc t
  (* | Skip_last t -> drop_first acc t *)

  and drop_first acc = function
    | Leaf _ -> acc
    | Append (a, b) -> drop_first (to_list_tree acc b) a
    | Reverse t -> drop_last_rev acc t
    | Skip_first t -> drop_first acc t
  (* | Skip_last t -> drop_last acc t *)

  and drop_last acc = function
    | Leaf _ -> acc
    | Append (a, b) -> to_list_tree (drop_last acc b) a
    | Reverse t -> drop_first_rev acc t
    | Skip_first t -> drop_first acc t
  (* | Skip_last t -> drop_last acc t *)

  (* Drop first and then reverse *)
  and drop_first_rev acc = function
    | Leaf _ -> acc
    | Append (a, b) -> drop_first_rev (to_list_tree_rev acc b) a
    | Reverse t -> drop_last acc t
    | Skip_first t -> drop_first_rev acc t
  (* | Skip_last t -> drop_last_rev acc t *)

  and drop_last_rev acc = function
    | Leaf _ -> acc
    | Append (a, b) -> drop_last_rev (to_list_tree_rev acc a) b
    | Reverse t -> drop_first acc t
    | Skip_first t -> drop_first_rev acc t
  (* | Skip_last t -> drop_last_rev acc t *)

  let to_list p = to_list_tree [] p.tree

  let concat a b =
    {
      first = a.first;
      last = b.last;
      tree = Append (a.tree, Skip_first b.tree);
    }

  let _append a b =
    { first = a.first; last = b.last; tree = Append (a.tree, b.tree) }

  let size p =
    let rec loop s = function
      | Leaf _ -> s + 1
      | Append (a, b) -> loop (loop s a) b
      | Reverse t -> loop s t
      | Skip_first t -> loop (s - 1) t
      (* | Skip_last t -> loop (s-1) t  *)
    in
    loop 0 p.tree

  (* let () = *)
  (*   assert (to_list (append (of_list [0;1]) (reverse (of_list [2;3;4]))) *)
  (*           =  [0; 1; 4; 3; 2]); *)
  (*   assert (to_list (concat (of_list [1]) (of_list [2;3;4])) *)
  (*           = [1; 3; 4]) *)

  let () = ()
end

(* Here we remove the end points from the tree *)
module Tree1_path (P : POINT) : PATH with type point := P.t = struct
  type point = P.t
  type tree = Empty | Leaf of point | Append of tree * tree | Reverse of tree
  type t = { first : point; last : point; tree : tree }

  let left p = p.first
  let right p = p.last
  let ends p = (p.first, p.last)

  let reverse p =
    {
      first = p.last;
      last = p.first;
      tree =
        (* Reverse p.tree *)
        (match p.tree with
        | Empty -> Empty
        | Leaf _ as t -> t
        | Reverse t -> t
        | t -> Reverse t);
    }

  let of_list = function
    | [] | [ _ ] -> raise (invalid_arg "of_list")
    | x :: rest ->
        let first = x in
        let rec loop tree = function
          | [ last ] -> (last, tree)
          | y :: ys -> loop (Append (tree, Leaf y)) ys
          | [] -> assert false
        in
        let last, tree = loop Empty rest in
        { first; last; tree }

  let of_pair (x, y) = { first = x; last = y; tree = Empty }

  let rec to_list_tree acc = function
    | Leaf x -> x :: acc
    | Append (a, b) -> to_list_tree (to_list_tree acc b) a
    (* right first, then left, so this is consistent with Append(Leaf x, Leaf y)
       --> [x ; y] *)
    | Reverse t -> to_list_tree_rev acc t
    | Empty -> acc

  and to_list_tree_rev acc = function
    | Leaf x -> x :: acc
    | Append (a, b) -> to_list_tree_rev (to_list_tree_rev acc a) b
    | Reverse t -> to_list_tree acc t
    | Empty -> acc

  let to_list p = to_list_tree [ p.last ] p.tree |> List.cons p.first

  let concat a b =
    {
      first = a.first;
      last = b.last;
      tree = Append (a.tree, Append (Leaf a.last, b.tree));
    }

  let _append a b =
    {
      first = a.first;
      last = b.last;
      tree = Append (Append (a.tree, Leaf a.last), Append (Leaf b.first, b.tree));
    }

  let size p =
    let rec loop s = function
      | Empty -> s
      | Leaf _ -> s + 1
      | Append (a, b) -> loop (loop s a) b
      | Reverse t -> loop s t
    in
    loop 0 p.tree + 2

  (* let () = *)
  (*   assert (to_list (append (of_list [0;1]) (reverse (of_list [2;3;4]))) *)
  (*           =  [0; 1; 4; 3; 2]); *)
  (*   assert (to_list (concat (of_list [1]) (of_list [2;3;4])) *)
  (*           = [1; 3; 4]) *)

  let () = ()
end

module Path2D = OList_path (Point2d)
(* According to my benchmarks for the heart example, the fastest is
   1. OList_path 2.20
   2. Tree1_path 2.25
   3. List_path 2.27
   4. Tree_path 2.28
   But it's quite close. For other tests (like below), exchange 3/4 and even 2/3.

   For low number of paths (<100 pairs ?) this the opposite: List comes first
   and Olist last!

   On average (long and short paths), maybe Tree1 is the best? On the other
   hand, for short paths, it's very fast anyways, so maybe favor OList which is
   the best for many paths?
*)

(* TODO use this if we know that cell coordinates (i,j) are small enough ( < 2^13?) *)
module IHash : Hashtbl.HashedType = struct
  type t = int

  let equal = Int.equal
  let hash = Hashtbl.hash
end

module Component : COMPONENT =
functor
  (P : POINT)
  (S : PATH with type point := P.t)
  ->
  struct
    type pos_tbl = (P.cell, P.t) Hashtbl.t
    (** A [cell] is (the name of) a group of points that are identified (close
        to each other). It's like a union in a "union-find" structure. *)

    type edge = {
      src : P.cell;
      dst : P.cell;
      path : S.t;
      id : int;
          (* this id is used to "pair" an edge from A to B with its reverse
                  from B to A. An even id means from src to dst. Odd id is the
                  opposite. *)
    }

    type adj_tbl = (P.cell, edge list) Hashtbl.t
    (** Adjacency table: cell -> list of paths (in fact, edges) starting at
        cell. This table is symmetric: if [path] is registered at [cell]
        (because [left path] belongs to [cell]), then [reverse path] must be
        registered at the cell containing [right path]. *)

    type t = {
      paths : S.t list;
      (* pos_tbl : pos_tbl; *)
      (* adj_tbl : adj_tbl; *)
      epsilon : float;
    }

    let new_id =
      let id = ref (-1) in
      fun () ->
        incr id;
        !id lsl 1

    let get_path edge = edge.path
    let get_epsilon comp = comp.epsilon
    let contents comp = comp.paths
    let to_list comp = List.map S.to_list comp.paths
    let check_concat epsilon p1 p2 = P.same epsilon (S.right p1) (S.left p2)

    let concat epsilon p1 p2 =
      if P.same epsilon (S.right p1) (S.left p2) then Some (S.concat p1 p2)
      else None

    let concat_edge e1 e2 =
      {
        src = e1.src;
        dst = e2.dst;
        path = S.concat e1.path e2.path;
        id = new_id ();
      }

    let concat_edges = function
      | [] -> raise (invalid_arg "concat_edge")
      | e :: rest ->
          let rec loop acc = function
            | [] -> acc
            | ee :: rrest -> loop (concat_edge ee acc) rrest
          in
          loop e rest

    let check_position same (pos_tbl : pos_tbl) z cell =
      match Hashtbl.find_opt pos_tbl cell with
      | Some z0 -> same z0 z
      | None -> false

    (* Find a suitable cell for the given point *)
    let register_position epsilon pos_tbl p =
      let cell0 = P.hint epsilon p in
      let ngb = P.neigh cell0 in
      match List.find_opt (check_position (P.same epsilon) pos_tbl p) ngb with
      | Some cell -> cell
      | None ->
          Hashtbl.add pos_tbl cell0 p;
          cell0

    let get_adj_list (adj_tbl : adj_tbl) (cell : P.cell) =
      (* utiliser plutôt Set pour ne pas avoir de doublons? Mais attention l'algo
       actuel a *besoin* des doublons pour fonctionner correctement. *)
      Hashtbl.find_opt adj_tbl cell |> Option.value ~default:[]

    let get_deg_list deg_tbl (cell : P.cell) =
      match Hashtbl.find_opt deg_tbl cell with
      | Some d -> d
      | None ->
          let d = ref 0 in
          Hashtbl.add deg_tbl cell d;
          d

    (* TODO extract *loops* here into a separate table. Warning, if we have many
     points along a curve that are epsilon/2 distant, we should not consider all
     pairs as isolated points! (or self-loops). With the current algo, this is ok:
     once the first point is registered, the second point will be collapsed to it,
     but the third point will correctly be registered at its own position, and so
     on. The final resut is to "ignore" one every two points, which is consistent
     with the epsilon precision requirement. *)
    let register_adjacency epsilon pos_tbl adj_tbl path =
      let src = register_position epsilon pos_tbl (S.left path) in
      let dst = register_position epsilon pos_tbl (S.right path) in
      let id = new_id () in
      let edge = { src; dst; path; id } in
      let egde = { src = dst; dst = src; path = S.reverse path; id = id + 1 } in
      let adj1 = get_adj_list adj_tbl src in
      Hashtbl.replace adj_tbl src (edge :: adj1);
      let adj2 = get_adj_list adj_tbl dst in
      (* attention on peut avoir des doublons . if pos1 = pos2 then it should be
       added *twice* to the adj_tbl. *)
      Hashtbl.replace adj_tbl dst (egde :: adj2)

    (* This version only registers the degree of cells (number of paths connected
     to it). Only used for testing. *)
    let register_degree ?(check = fun _ -> false) epsilon pos_tbl deg_tbl path =
      let src = register_position epsilon pos_tbl (S.left path) in
      let dst = register_position epsilon pos_tbl (S.right path) in
      let d1 = get_deg_list deg_tbl src in
      incr d1;
      let d2 = get_deg_list deg_tbl dst in
      incr d2;
      (check !d1, check !d2)

    let make_adjacency_table epsilon paths =
      let len = List.length paths in
      let pos_tbl = Hashtbl.create len in
      let adj_tbl = Hashtbl.create len in
      List.iter (register_adjacency epsilon pos_tbl adj_tbl) paths;
      (pos_tbl, adj_tbl)

    (* For testing *)
    let check_max_degree d epsilon paths =
      let len = List.length paths in
      let pos_tbl = Hashtbl.create len in
      let deg_tbl = Hashtbl.create len in
      let check x = x <= d in
      let rec loop = function
        | [] -> true
        | path :: rest ->
            let ok1, ok2 =
              register_degree ~check epsilon pos_tbl deg_tbl path
            in
            if ok1 && ok2 then loop rest else false
      in
      loop paths

    let check_component = check_max_degree 1

    (* In fact we don't care about order (==> use set ?) *)
    let list_remove_first f list =
      let rec loop acc = function
        | [] -> acc
        | x :: rest ->
            if f x then List.rev_append acc rest else loop (x :: acc) rest
      in
      loop [] list

    let () =
      assert (
        list_remove_first (( = ) 2) [ 1; 2; 3; 4; 2; 5 ] = [ 1; 3; 4; 2; 5 ])

    let _start_from cell edge = edge.src = cell
    let _ends_in cell edge = edge.dst = cell
    let has_id id edge = edge.id = id
    let pairing_id id = if id land 1 = 0 then id + 1 else id - 1

    (* Extract a path starting by [cell0, cell1,...] (so only in one direction). *)
    let extract_path adj_tbl cell1 =
      let rec loop acc cell1 = function
        (* At this stage the edge (=adjacency) between vertex cell1 and its
         predecessor cell0 must have been removed from the adjacency lists of
         cell0 and cell1 (so that we don't go back to cell0). *)
        | [] ->
            (* If there is no adjacent point, it must be the final point. *)
            Hashtbl.remove adj_tbl cell1;
            acc
        | edge :: rest -> begin
            (* We have more points connected to v1, so we continue... *)
            Hashtbl.replace adj_tbl cell1 rest;
            let cell2 = edge.dst in
            let adj2 = Hashtbl.find adj_tbl cell2 in
            (* = This should exist: During this loop, we may *modify* adjacency
             lists, but we don't *remove* them from the table. So, since v1 was
             connected to v2, the adjacency of v2 cannot be non-existent. *)
            (* pr "  Removing %s from adjacency of %s" (print_pos pos1) (print_pos pos2); *)
            let adj2 = list_remove_first (has_id (pairing_id edge.id)) adj2 in
            Hashtbl.replace adj_tbl cell2 adj2;
            loop (edge :: acc) cell2 adj2
          end
      in
      let adj1 =
        match Hashtbl.find_opt adj_tbl cell1 with
        | None ->
            Debug.print
              "Cannot extract a path from the given cell since it is not in \
               the adj_tbl.";
            assert false
        | Some a -> a
      in
      loop [] cell1 adj1
      |> concat_edges (* TODO register to a new adj_tbl (and new pos_tbl)*)
      |> get_path

    let make_component epsilon paths =
      let _pos_tbl, adj_tbl = make_adjacency_table epsilon paths in
      let cells = Hashtbl.fold (fun cell _ list -> cell :: list) adj_tbl [] in
      (* We first extract paths with disjoints ends; when there is none, we look for
       cyces (=loops), and start again, until everything is visited. *)
      let rec loop found_one paths non_treated = function
        | [] ->
            if non_treated = [] then paths
            else if found_one then begin
              (* New pass *)
              loop false paths [] non_treated
            end
            else begin
              (* Looking for CYCLES*)
              match non_treated with
              | [] ->
                  Debug.print "non_treated should not be empty here";
                  assert false
              | cell1 :: rest ->
                  let path = extract_path adj_tbl cell1 in
                  loop false (path :: paths) [] rest
            end
        | cell1 :: rest -> begin
            (* pr "Looking at %s:" (print_pos cell1); *)
            match Hashtbl.find_opt adj_tbl cell1 with
            | None ->
                (* pr "  already visited."; *)
                loop found_one paths non_treated rest
            | Some [] ->
                (* pr "  already taken by another path"; *)
                loop found_one paths non_treated rest
            | Some [ edge ] when edge.dst = cell1 ->
                (* this case is not strictly necessary, it should work with the
               general case too. *)
                (* pr "  found isolated cycle"; *)
                Hashtbl.remove adj_tbl cell1;
                loop true (edge.path :: paths) non_treated rest
            | Some [ _ ] ->
                (* pr "  found degree one point (connected to %s); we extract the path..." *)
                (*   (print_pos pos2); *)
                let path = extract_path adj_tbl cell1 in
                Hashtbl.remove adj_tbl cell1;
                loop true (path :: paths) non_treated rest
            | Some _ ->
                (* pr "  degree larger than one, skipping for now."; *)
                loop found_one paths (cell1 :: non_treated) rest
          end
      in
      let paths = loop false [] [] cells in
      { paths; (* pos_tbl; adj_tbl; *) epsilon }

    let concat_components epsilon comps =
      (* on pourrait optimiser en sachant que chaque groupe de chemins
       dans une composante ne peut pas s'interconnecter, et en utilisant les
       tables déjà remplies. *)
      make_component epsilon (List.concat (List.map contents comps))

    let concat_pairs epsilon pairs =
      let paths = List.map S.of_pair pairs in
      make_component epsilon paths

    let concat_lists epsilon lists =
      List.map S.of_list lists |> make_component epsilon |> to_list

    (* let add_path comp path = *)
    (*   let src = register_position comp.epsilon comp.pos_tbl (S.left path) in *)
    (*   let dst = register_position comp.epsilon comp.pos_tbl (S.right path) in *)
    (*   () (\* TODO *\) *)
  end

module Comp2D = Component (Point2d) (Path2D)

module Test = struct
  let timeit ?name f n =
    let t0 = Unix.gettimeofday () in
    let cpu0 = Sys.time () in
    for _ = 1 to n do
      f ()
    done;
    let t = Unix.gettimeofday () -. t0 in
    let cpu = Sys.time () -. cpu0 in
    let name = match name with None -> "" | Some s -> "[" ^ s ^ "]: " in
    Debug.print "%stotal time for n=%i     : %fs  (average:%gs)" name n t
      (t /. float n);
    Debug.print "%stotal CPU time for n=%i : %fs  (average:%gs)\n" name n cpu
      (cpu /. float n)

  let test_pairs ?(pr = true) connect_fn ran () =
    let ps = connect_fn ran in
    let len = List.length ps in
    let n = List.length ran in
    if pr then
      Debug.print
        "Initial pairs=%u; Number of paths=%u; number of nodes=%u; reduction \
         coeff:%.2f"
        n len
        (List.length (List.flatten ps))
        (float (len + n) /. float (2 * n))

  let room_size = 500

  let ran_pairs n =
    List.init n (fun _ -> [ Random.int room_size; Random.int room_size ])

  let ran = ran_pairs 10000

  module Path = List_path (PointInt)
  module C = Component (PointInt) (Path)

  let test1 () =
    let pairs = [ (4, 5); (6, 7); (6, 8); (3, 4); (5, 6); (1, 2) ] in
    assert (
      C.concat_pairs 1. pairs
      |> C.to_list
      = [ [ 8; 6 ]; [ 7; 6; 5; 4; 3 ]; [ 1; 2 ] ]);
    let lists = List.map (fun (x, y) -> [ x; y ]) pairs in
    assert (C.concat_lists 1. lists = [ [ 8; 6 ]; [ 7; 6; 5; 4; 3 ]; [ 1; 2 ] ]);
    let my_comp = List.map Path.of_pair [ (1, 2); (3, 4); (6, 5); (8, 9) ] in
    let my_comp2 = List.map Path.of_pair [ (2, 3); (4, 5); (6, 8) ] in
    let bad_comp = List.map Path.of_pair [ (2, 3); (4, 5); (5, 8) ] in
    assert (not (C.check_component 1. bad_comp));
    assert (C.check_component 1. my_comp);
    assert (C.check_component 1. my_comp2);
    let comps =
      List.map (fun x -> [ x ]) (List.rev_append my_comp my_comp2)
      |> List.map (C.make_component 1.)
    in
    assert (C.check_component 1. (C.concat_components 1. comps |> C.contents));
    timeit ~name:"List_path" (test_pairs (C.concat_lists 1.) ran) 5

  module Path2 = OList_path (PointInt)
  module C2 = Component (PointInt) (Path2)

  let test2 () =
    let pairs = [ (4, 5); (6, 7); (6, 8); (3, 4); (5, 6); (1, 2) ] in
    assert (
      C2.concat_pairs 1. pairs
      |> C2.to_list
      = [ [ 8; 6 ]; [ 7; 6; 5; 4; 3 ]; [ 1; 2 ] ]);
    let lists = List.map (fun (x, y) -> [ x; y ]) pairs in
    assert (C2.concat_lists 1. lists = [ [ 8; 6 ]; [ 7; 6; 5; 4; 3 ]; [ 1; 2 ] ]);
    let my_comp = List.map Path2.of_pair [ (1, 2); (3, 4); (6, 5); (8, 9) ] in
    let my_comp2 = List.map Path2.of_pair [ (2, 3); (4, 5); (6, 8) ] in
    let bad_comp = List.map Path2.of_pair [ (2, 3); (4, 5); (5, 8) ] in
    assert (not (C2.check_component 1. bad_comp));
    assert (C2.check_component 1. my_comp);
    assert (C2.check_component 1. my_comp2);
    let comps =
      List.map (fun x -> [ x ]) (List.rev_append my_comp my_comp2)
      |> List.map (C2.make_component 1.)
    in
    assert (C2.check_component 1. (C2.concat_components 1. comps |> C2.contents));
    timeit ~name:"OList_path" (test_pairs (C2.concat_lists 1.) ran) 5

  module Path3 = Tree_path (PointInt)
  module C3 = Component (PointInt) (Path3)

  let test3 () =
    let pairs = [ (4, 5); (6, 7); (6, 8); (3, 4); (5, 6); (1, 2) ] in
    assert (
      C3.concat_pairs 1. pairs
      |> C3.to_list
      = [ [ 8; 6 ]; [ 7; 6; 5; 4; 3 ]; [ 1; 2 ] ]);
    let lists = List.map (fun (x, y) -> [ x; y ]) pairs in
    assert (C3.concat_lists 1. lists = [ [ 8; 6 ]; [ 7; 6; 5; 4; 3 ]; [ 1; 2 ] ]);
    let my_comp = List.map Path3.of_pair [ (1, 2); (3, 4); (6, 5); (8, 9) ] in
    let my_comp2 = List.map Path3.of_pair [ (2, 3); (4, 5); (6, 8) ] in
    let bad_comp = List.map Path3.of_pair [ (2, 3); (4, 5); (5, 8) ] in
    assert (not (C3.check_component 1. bad_comp));
    assert (C3.check_component 1. my_comp);
    assert (C3.check_component 1. my_comp2);
    let comps =
      List.map (fun x -> [ x ]) (List.rev_append my_comp my_comp2)
      |> List.map (C3.make_component 1.)
    in
    assert (C3.check_component 1. (C3.concat_components 1. comps |> C3.contents));
    timeit ~name:"Tree_path" (test_pairs (C3.concat_lists 1.) ran) 5

  module Path4 = Tree1_path (PointInt)
  module C4 = Component (PointInt) (Path4)

  let test4 () =
    let pairs = [ (4, 5); (6, 7); (6, 8); (3, 4); (5, 6); (1, 2) ] in
    assert (
      C4.concat_pairs 1. pairs
      |> C4.to_list
      = [ [ 8; 6 ]; [ 7; 6; 5; 4; 3 ]; [ 1; 2 ] ]);
    let lists = List.map (fun (x, y) -> [ x; y ]) pairs in
    assert (C4.concat_lists 1. lists = [ [ 8; 6 ]; [ 7; 6; 5; 4; 3 ]; [ 1; 2 ] ]);
    let my_comp = List.map Path4.of_pair [ (1, 2); (3, 4); (6, 5); (8, 9) ] in
    let my_comp2 = List.map Path4.of_pair [ (2, 3); (4, 5); (6, 8) ] in
    let bad_comp = List.map Path4.of_pair [ (2, 3); (4, 5); (5, 8) ] in
    assert (not (C4.check_component 1. bad_comp));
    assert (C4.check_component 1. my_comp);
    assert (C4.check_component 1. my_comp2);
    let comps =
      List.map (fun x -> [ x ]) (List.rev_append my_comp my_comp2)
      |> List.map (C4.make_component 1.)
    in
    assert (C4.check_component 1. (C4.concat_components 1. comps |> C4.contents));
    timeit ~name:"Tree1_path" (test_pairs (C4.concat_lists 1.) ran) 5
end

(* let () = Test.test1 () *)
(* let () = Test.test3 () *)
(* let () = Test.test4 () *)
(* let () = Test.test2 () *)

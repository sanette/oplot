(* Three 3D views. *)

open Oplot.Plt

let a = axis 0. 0.
let fx u v = 0.3 +. ((1. +. (0.5 *. cos u)) *. cos v)
let fy u v = 0.2 +. ((1. +. (0.5 *. cos u)) *. sin v)
let fz u _ = 0.5 *. sin u
let s = surf3d_plot ~wire:true ~width:10 ~height:50 fx fy fz 0. 0. 3. 6.29

let set_wire po wire =
  match po with
  | Surf3d ((fx, fy, fz, v3, _), _) ->
      Surf3d ((fx, fy, fz, v3, wire), Internal.gllist_empty ())
  | Grid ((fm, v3, _), _) -> Grid ((fm, v3, wire), Internal.gllist_empty ())
  | _ -> raise Not_found

let t = text "Press CTRL-L to toggle lighting" 0.3 0.1;;

display [ set_wire s false; t ];;
display [ Color cyan; set_wire s true; Color red; a ] ~dev:gl

let fx u v =
  ((3. *. (1. +. sin v)) +. (2. *. (1. -. (cos v /. 2.)) *. cos u)) *. cos v

let fy u v = (4. +. (2. *. (1. -. (cos v /. 2.)) *. cos u)) *. sin v
let fz u v = -2. *. (1. -. (cos v /. 2.)) *. sin u
let s = surf3d_plot ~width:30 ~height:50 fx fy fz 0. 0. 6.283 6.283;;

display [ Clear black; Color cyan; s; Color red ];;
quit ()

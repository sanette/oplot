(* We include here more sophisticated plots that are build upon the core oplot
   modules.  *)

let implicit_curve  ?(pixel_size = (500,500)) ?grid_size ?(sub_size = (2,2)) ?depth
    ?(steps=4) ?(better=0) f v =
  let open Points.Point2 in
  let f p = f p.x p.y in
  Isocurve.compute_level ~debug:false ~pixel_size ?grid_size ~sub_size ~steps ~better ?depth f v
  |> fst

(*
   Local Variables:
   compile-command:"cd ..;dune build"
   End:
*)

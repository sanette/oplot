let () =
  let open Oplot.Plt in
  let p = plot sin (-2.) 20. in
  let a = axis 0. 0. in

  display ~dev:fig [ Color red; p; Color black; a ]

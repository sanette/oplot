(* Matrix example *)
#use "topfind";;

#thread;;

#require "oplot";;

open Oplot.Plt;;

let m = Array.make_matrix 20 30 0;;

for i = 0 to 19 do
  for j = 0 to 29 do
    m.(i).(j) <- Random.int 255
  done
done
;;

display [ Color blue; Matrix m ];;

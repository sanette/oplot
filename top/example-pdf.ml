(* Basic Oplot example *)

#use "topfind";;
#require "oplot";;

open Oplot.Main;;
open Oplot.Def;;

let p = plot sin (-2.) 20.
let a = axis 0. 0. ;;

display ~dev:Oplot.Renderinit.pdf [Color red ; p ; Color black ; a];;

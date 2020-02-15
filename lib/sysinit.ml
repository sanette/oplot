(* initialisations système *)
(* initialise aussi sdl pour charger la fonte... à faire après ? *)

exception Shell_error of (int * string)

let concat = Filename.concat

(* installation *)
let oplot_dir =
  try
    Sys.getenv "OPLOTDIR"
  with 
  | Not_found -> let exe = (* Whereami.exe ()*) Sys.executable_name in
    begin 
      let basename, dirname = Filename.basename, Filename.dirname in
      Debug.print "Executable: %s" (basename exe);
      Debug.print "Directory: %s" (basename (dirname exe));
      match (basename exe), (basename (dirname exe)) with
      | "goplot", "bin"
        (* = cas où la librairie est utilisée par goplot *)
        -> Filename.concat (dirname (dirname exe)) "share/oplot"
      | "goplot.exe", "gui"       (* = lancement par dune exec gui/goplot.exe *)
      | "utop.exe", ".utop"       (* = lancement par dune utop *)
        -> Filename.concat (dirname (dirname exe)) "share"

      | _ -> let system = Unix.open_process_in "opam config var prefix" in
        let res = input_line system in
        match Unix.close_process_in system with
        | Unix.WEXITED 0 -> Filename.concat res "share/oplot"
        | _ -> failwith 
                 "Please tell me where the oplot directory is, by setting the \
                  environment variable OPLOTDIR." 
    end
  | e -> raise e

let () = Debug.print "Using oplotdir=%s " oplot_dir

let first_time = ref true

(* répertoire perso. Inutilisé pour le moment *)
let home_dir = 
  let home = try Sys.getenv "HOME" with Not_found -> "." in
  let home_dir_name = concat home ".oplot" in
  if Sys.file_exists home_dir_name then
    if (Unix.stat home_dir_name).Unix.st_kind = Unix.S_DIR 
    then (first_time := false; 
          home_dir_name)
    else "./" (* then we use the current dir *)
  else (print_endline ("Creating personal oplot directory in " ^ home_dir_name);
        Unix.mkdir home_dir_name 0o755;
        home_dir_name)

            
(* répertoire temporaire; hack around temp_file *)
let init_tmp_dir var = 
  let tmp = Filename.temp_file "oplot" "" in
  Sys.remove tmp;
  Unix.mkdir tmp 0o755;
  print_endline ("Creating temp dir in " ^ tmp);
  var := tmp

let tmp_dir = ref ""
    
let () = init_tmp_dir tmp_dir

let get_tmp_dir () = !tmp_dir
    
(* font path for font *)
let init_font_path ?(fontname="FreeSans.ttf") var =
  let searchlist = 
    [ fontname;
      (concat oplot_dir fontname);
      (concat "/usr/share/fonts/truetype/freefont/" fontname);
      (concat "/usr/share/fonts/TTF/" fontname);
      (concat "/usr/share/vlc/skins2/fonts/" fontname) ] in
  let rec loop l = match l with
    | [] -> print_endline ("Fatal error: font " ^ fontname ^ " not found"); 
 raise Not_found
    | s::ll -> if Sys.file_exists s then var:=s else loop ll in
    loop searchlist

let font_path = ref ""
let () = init_font_path ~fontname:"DejaVuSans.ttf" font_path
(* FreeSans.ttf est beaucoup plus proche du Helvetica de xfig, et donc de la
   sortie postscript. Mais le "hinting" (rendu à l'écran en petite taille) est
   nettement meilleur avec DejaVuSans... *)

let current_font_size = ref 12
let () = Sdlttf.init()
let current_font = ref (Sdlttf.open_font !font_path !current_font_size)

(* device ghostscript: verifier qu'on a bien pngalpha avec *)
(* gs -h. Sinon mettre autre chose ? png??? *)



let xfig_output_file = concat !tmp_dir "oplot.fig"
(* any other way ??: *)
let xfig_main_channel = ref (open_out (concat !tmp_dir ".dummy.main"))
(* let (xfig_main_file , xfig_main_channel) = Filename.open_temp_file
   "oplot_fig" ".main";; *)
let () = close_out !xfig_main_channel
let xfig_head_channel = ref (open_out (concat !tmp_dir ".dummy.head"))
let () = close_out !xfig_head_channel
let fig_color_counter = ref 32

let latex_header = (concat oplot_dir "header.tex")
let eps_output_file = concat !tmp_dir "oplot.eps"
let pdf_output_file = concat !tmp_dir "oplot.pdf"
(* doit être le même que xfig_output_file, avec extension eps (pour
   fig2eps). L'implémenter directement ainsi ? *)

let latex_tmp = "oplot-tmp.tex"

let bmp_output = "oplot.bmp"

(* deux précautions valent mieux qu'une pour éviter de détruire d'importe
   quoi... *)
let remove_tmp_dir () = 
  let filelist = 
    [ "oplot.bmp";
      "oplot.eps";
      "oplot.pdf";
      "oplot-tmp.aux";
      "oplot-tmp.dvi";
      "oplot-tmp.log";
      "oplot-tmp.png";
      "oplot-tmp.ps";
      "oplot-tmp.tex";
      ".dummy.head";
      ".dummy.main";
      "oplot.fig" ] in
  List.iter (fun s -> let file = concat !tmp_dir s in
              if Sys.file_exists file then Sys.remove file) filelist;
  try
    Unix.rmdir !tmp_dir
  with 
  | Unix.Unix_error (err,_,_) when err = Unix.ENOTEMPTY -> 
    print_endline "Warning: temporary directory was not empty. We are not \
                   deleting."
  |  e -> raise e


let shell command = 
  let exec s =
    if Debug.debug then print_endline s;
    match (Sys.command s) with
    | 0 -> ()
    | a -> raise (Shell_error (a,s)) in
  Printf.kprintf exec command
    
(* vérifie si gs est compilé avec le device "pngalpha" *)
let pngalpha () = 
  Sys.command "gs --help | grep pngalpha > /dev/null" = 0

(* vérifie la présence d'un exécutable *)
let has_exe name =
  Sys.command (Printf.sprintf "which %s" name) = 0

let has_latex = has_exe "latex"
let has_gs = has_exe "gs"
let has_xfig = has_exe "xfig"
let has_inkscape = has_exe "inkscape"
let has_fig2dev = has_exe "fig2dev"

let fig2ps =
  if has_exe "fig2ps" then "fig2ps"
      else let local = concat oplot_dir "fig2eps" in
        if (Unix.stat local).st_perm <> 0o755
        then Unix.chmod local 0o755;
        local
let fig2eps = fig2ps ^ " --eps --noforcespecial --nogv"
let fig2pdf = fig2ps ^ " --pdf --noforcespecial --nogv"

(* vérifie la présence d'au moins un exécutable dans une liste. Renvoie le
   premier trouvé (option)*)
let exe_from_list list =
  let rec loop l = match l with
    | [] -> None
    | v::ll -> if has_exe v then (Some v) else loop ll in
    loop list

(* cherche un programme pour voir des images. None si rien trouvé *)
let viewer = 
  exe_from_list ["display"; "kuickshow"; "gwenview"; "gimp"]

(* cherche un programme pour voir du postscript. None si rien trouvé *)
let psviewer = 
  exe_from_list ["gv"; "kghostview"]

let first_time () = !first_time

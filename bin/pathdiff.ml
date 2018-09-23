open Printf
open Lib

let base_dir = Sys.argv.(1)
let contents = Dir.get_files_contents base_dir
let base = "state"

let mapped = Replace.replace_paths base contents
let print_con cons =
  cons
  |> List.map(fun (f,c) -> f ^ "\n" ^ c ^ "\n")
  |> String.concat "--------------------\n"
  
let () =
  printf "%s\n" (print_con mapped);
  Dir.replace_content mapped

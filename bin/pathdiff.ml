open Printf
open Lib

let base_dir = Sys.argv.(1)

(* let file_to_string n res (name, contents) =
    let sep = String.make n '-' in
    List.concat [res; [sep; name; sep; contents; sep; "\n"]]

let my_max = function
    | [] -> None
    | xs -> xs
            |> List.map String.length
            |> List.fold_left max 0
            |> (fun x -> Some x)

let files = Dir.get_dir_contents base_dir

let contents fs n =
    fs
    |> List.map (fun f -> (f, Dir.get_file_contents f))
    |> List.fold_left (file_to_string n) []

let listings = my_max files
    |> Option.map (contents files)
    |> Option.default [""] *)

let haystack = "import { lol } from \"js/state/store\";\nimport { ObjectIdParam } from \"js/state/core/functionDesigner/common/base/typings\";\nimport { Size, Position } from \"js/state/core/functionDesigner/common/typings\";"
let needle = "js/state/core/functionDesigner/"
let needles = Replace.reduce_paths "js/state/core/functionDesigner/functionEditor/sheets"
let somethig_else = needles
|> List.fold_left (fun r n -> Replace.find_and_replace n r) haystack

let () =
    (* printf "%s\n" (String.concat "\n" listings);
    printf "%s\n" (String.concat "\n" (Replace.reduce_paths (Sys.getcwd ())));
    printf "%s\n" (String.concat ", " (List.map string_of_int (Replace.find_occurences needle haystack)));
    printf "%s\n" (String.concat ", " (List.map string_of_int (Replace.find_sep_lengths needle haystack)));
    printf "%s\n" (Replace.find_and_replace needle haystack); *)
    printf "%s\n" (String.concat "\n" needles);
    printf "%s\n" somethig_else;
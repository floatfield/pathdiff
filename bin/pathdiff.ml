open Printf
open Lib

let base_dir = Sys.argv.(1)

let file_to_string n res (name, contents) =
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
    |> Option.default [""]

let () =
printf "%s\n" (String.concat "\n" listings)
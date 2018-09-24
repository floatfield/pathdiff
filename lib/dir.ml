let get_files path =
    Sys.readdir path
    |> Array.to_list

let is_valid_path path =
    let re = Str.regexp ".*/\\..+" in
    not (Str.string_match re path 0)

let rec dir_tree nest_level path =
    get_files path
    |> List.map (fun p -> path ^ p)
    |> List.fold_left (add_paths nest_level) []
and add_paths level res cur_path =
    if is_valid_path cur_path then
        if Sys.is_directory cur_path then
            List.concat [res; dir_tree (level + 1) (cur_path ^ "/")]
        else
            List.concat [res; [cur_path]]
    else
        res    

let get_dir_contents path = dir_tree 0 path

let get_file_contents path =
    let ic = open_in path in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s

let get_files_contents path =
  get_dir_contents path
  |> List.map (fun f -> f, get_file_contents f)

let put_to_file path contents =
    let oc = open_out path in
    output_string oc contents;
    close_out oc

let write_file_contents (name, contents) =
  put_to_file name contents

let replace_content =
  List.iter write_file_contents
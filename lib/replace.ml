let split_path = Str.split (Str.regexp "/")

let rec drop i xs =
    if i = 0 then
        xs
    else
       drop (i - 1) (List.tl xs)

let init i xs =
    xs |> List.rev |> drop i |> List.rev

let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

let reduce_paths path =
    let paths = split_path path in
    0 -- (List.length paths)
    |> List.map (fun i -> String.concat "/" (init i paths))

let find_sep_lengths pattern needle haystack =
    let str_len = String.length needle in
    let rec aux acc position =
        let re = Str.regexp ("\"" ^ needle ^ "/.*\"" ) in
        try
          let i = Str.search_forward re haystack position in
          let needle_len = String.length needle in
          let pat_len = String.length pattern in
          let len = pat_len - needle_len in
          let l = String.sub pattern needle_len len
            |> Core.String.filter ~f:(fun c -> c = '/')
            |> String.length
          in
          aux (l :: acc) (i + str_len)
        with
            Not_found -> acc
    in List.rev (aux [] 0)

let path_up n =
    if n = 0 then
        "."
    else
        0 -- (n - 1)
        |> List.map (fun _x -> "..")
        |> String.concat "/"

let rec replace xs needle haystack =
    if List.length xs = 0 then
        haystack
    else
        try
            let re = Str.regexp ("\"" ^ needle ) in
            let haystack'= Str.replace_first re ("\"" ^ path_up (List.hd xs)) haystack in
            replace (List.tl xs) needle haystack'
        with
            Not_found -> haystack

let find_and_replace pattern needle haystack =
    if String.length needle = 0 then
        haystack
    else
        let seps = find_sep_lengths pattern needle haystack in
        replace seps needle haystack

let replace_occurences dirname contents =
  dirname
  |> reduce_paths
  |> List.fold_left (fun r n -> find_and_replace dirname n r) contents

let get_dir base filename =
  let re = Str.regexp (".*\\(/" ^ base ^ ".*\\)/[^/]*$") in
  let matches = Str.string_match re filename 0 in
  if matches then
    Some (Str.matched_group 1 filename)
  else 
    None

let replace_files_contents base (filename, contents) =
  let dirname = get_dir base filename in
  match dirname with
  | None -> (filename, contents)
  | Some d ->
    let mapped_contents = replace_occurences d contents in
    (filename, mapped_contents)

let replace_paths base =
  List.map (replace_files_contents base)

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

let find_occurences needle haystack =
    let str_len = String.length needle in
    let rec aux acc position =
        let re = Str.regexp ("\"" ^ needle ^ "\\(.*\\)\"" ) in
        try
            let i = Str.search_forward re haystack position in
            aux (i :: acc) (i + str_len)
        with
            Not_found -> acc
    in aux [] 0

let find_sep_lengths needle haystack =
    let str_len = String.length needle in
    let rec aux acc position =
        (* Printf.printf "position: %i\n" position; *)
        let re = Str.regexp ("\"\\(" ^ needle ^ ".*\"\\)" ) in
        try
            let i = Str.search_forward re haystack position in
            let l = (Str.matched_group 1 haystack)
                |> Core.String.filter ~f:(fun c -> c = '/')
                |> String.length
            in
            (* Printf.printf "%i\n%i\n" i str_len; *)
            aux (l :: acc) (i + str_len)
        with
            Not_found -> acc
    in List.rev (aux [] 0)

let path_up n =
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

let find_and_replace needle haystack =
    if String.length needle = 0 then
        haystack
    else
        let seps = find_sep_lengths needle haystack in
        replace seps needle haystack
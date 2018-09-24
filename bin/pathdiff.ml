open Lib

let base_dir = Sys.argv.(1)
let base = Sys.argv.(2)
let contents = Dir.get_files_contents base_dir

let mapped = Replace.replace_paths base contents

let () =
  Dir.replace_content mapped;

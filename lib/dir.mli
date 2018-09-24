val get_files: string -> string list
val get_dir_contents: string -> string list
val get_file_contents: string -> string
val get_files_contents: string -> (string * string) list
val replace_content: (string * string) list -> unit
val put_to_file: string -> string -> unit
val is_valid_path: string -> bool
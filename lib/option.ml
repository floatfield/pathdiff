let map f a = match a with
    | None -> None
    | Some x -> Some (f x)

let default y a = match a with
    | None -> y
    | Some x -> x
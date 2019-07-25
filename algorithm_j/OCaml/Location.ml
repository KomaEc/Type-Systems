
type t = {
    filename : string
  ; line_num : int
  ; offset : int
}

let print outx t = 
  Printf.fprintf outx "%s : line %d : offset %d:" t.filename t.line_num t.offset
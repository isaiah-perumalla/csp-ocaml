open Core
open Garden

exception InvalidInput of string

let blocked_spots line_num line =
  let _, blocked = String.fold line ~init:(0, []) ~f:(fun (x,acc) ch -> 
    let x' = x +1 in
    if (phys_equal ch  ' ') then (x',acc) 
    else (x', (x, line_num)::acc)) in
    blocked

let parse file =   
  let first_line = Option.value_exn (In_channel.input_line file) in 
  let init = blocked_spots 0 first_line in
  let height, blocked = In_channel.fold_lines file ~init: (1, init) 
                            ~f: (fun (number, acc) line ->
      let blocked = List.rev_append acc (blocked_spots number line) in
      (number+1, blocked)) in
  height,blocked, (String.length first_line)

let parse_constraints f = 
  In_channel.fold_lines f ~init:[] ~f: (fun acc line -> 
    match (String.split line ~on: ',') with 
    |[f;qty;dist] -> ((String.get f 0), (int_of_string qty), (int_of_string dist))::acc
    |_ -> raise (InvalidInput line))

let run garden_file flower_file  = 
  In_channel.with_file garden_file ~f: (fun file -> 
   let height, blocked, width  = parse file in
   let constraints = In_channel.with_file flower_file ~f: (fun file ->
    parse_constraints file) in  
   let module MyGarden = MakeGarden(
    struct
      let height = height
      let width = width
      let blocked = blocked
  end) in
  let g = MyGarden.empty in
  Printf.printf "%s" (MyGarden.to_string g);
  
   let fmt = format_of_string  "height=%i width=%i\n" in 
   (* List.iter blocked ~f: (fun (x,y) -> printf "(%d,%d)\n" x y); *)
   List.iter constraints ~f: (fun (f,qty,dist) -> printf "%c, qty=%i, dist=%i \n" f qty dist);
   Printf.printf fmt height width)
    
    


let command =
    Command.basic
      ~summary:"outputs a flower bed design for a garden, subject to some constraints"
      ~readme:(fun () -> "More detailed information")
      Command.Param.(
        map
          (both
             (anon ("garden_file" %: string))
             (anon ("flower_file" %: string)))
          ~f:(fun (garden_file, flower_file) () ->
            run garden_file flower_file))
  
let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command
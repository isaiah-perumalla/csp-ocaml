open Core

let vacant line_num line =
  let _, vacant_spots = String.fold line ~init:(0, []) ~f:(fun (x,acc) ch -> 
    let x' = x +1 in
    if (phys_equal ch  'X') then (x',acc) 
    else (x', (x, line_num)::acc)) in
    vacant_spots

let parse file =   
  let first_line = Option.value_exn (In_channel.input_line file) in 
  let init = vacant 0 first_line in
  let height, empty_spots = In_channel.fold_lines file ~init: (1, init) 
                            ~f: (fun (number, acc) line ->
      let empty_spots = List.rev_append acc (vacant number line) in
      (number+1, empty_spots)) in
  height,empty_spots, (String.length first_line)

  
let run garden_file _  = 
  In_channel.with_file garden_file ~f: (fun file -> 
   let height, empty_spots, width  = parse file in 
   let fmt = format_of_string  "height=%i width=%i\n" in 
   List.iter empty_spots ~f: (fun (x,y) -> printf "(%d,%d)\n" x y);
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
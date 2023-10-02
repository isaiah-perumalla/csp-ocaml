module type Layout = sig
  type pos = (int*int)
  
  type t
  type flower  
  val distance: pos -> pos -> int

  val vacant: pos list
  val is_blocked:  pos -> bool

  val plant: t -> flower -> pos -> (t,unit) Result.t  
  
  val empty: t

  val to_string: t -> string

end

module type Dimension = sig
  val height : int
  val width : int
  val blocked: (int*int) list
end

module MakeGarden(Dim : Dimension) : Layout = struct
  module PosOrd = struct
    type t = (int * int)
  let compare (x0,y0) (x1, y1)= 
    match (compare y0 y1) with
      |0 -> compare x0 x1
      |r -> r

end
  module PositionSet = Set.Make(PosOrd)
  module PositionMap = Map.Make(PosOrd)

  let height = Dim.height
  let width = Dim.width
  let blocked = PositionSet.of_list Dim.blocked

  let is_blocked (x,y) = 
    (x < 0 ) || (y < 0) || (x >= width) || (y >= height) || (PositionSet.mem (x,y) blocked)    


  let all_pos = PositionSet.of_list (List.concat 
      (List.init height (fun y ->
        List.init width (fun x -> (x,y))))) 
  
  
  let vacant = 
    let all = (PositionSet.filter (fun pos -> not (is_blocked pos)) all_pos) in
    PositionSet.fold (fun p acc -> p::acc) all []

  
  type pos = (int * int)

  
  type flower = char

  type t = { flowers: flower PositionMap.t }
  
  
  let empty = {flowers = PositionMap.empty }
  
  let plant t f pos = 
      if is_blocked pos then Error () 
      else if (PositionMap.mem pos t.flowers) then Error ()
      else Ok({flowers = (PositionMap.add pos f t.flowers)})
  
  let distance a b = 
    let (x0,y0) = a in
    let (x1, y1) = b in
    (abs (x1-x0)) + (abs (y0 -y1))
    
  let to_string t = 
    let buf = Buffer.create (height * width) in
    (* List.iter (fun (x,y) -> 
      if x  = 0 then Buffer.add_char buf '\n';
      let ch = match PositionMap.find_opt (x,y) t.flowers with
              |Some(f) ->  f
              |_ -> if (is_blocked (x,y)) then 'X' else ' ' in
      Buffer.add_char buf ch) all_pos;
       *)
       PositionSet.iter (
        fun (x,y) -> 
          let ch = match PositionMap.find_opt (x,y) t.flowers with
              |Some(f) ->  f
              |_ -> if (is_blocked (x,y)) then 'X' else ' ' in
            Buffer.add_char buf ch;
        if x == (width -1) then Buffer.add_string buf "\n"
        ) all_pos;
    Buffer.contents buf

end

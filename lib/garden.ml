module type Layout = sig
  type pos = (int*int)
  
  type t
  type flower  
  val distance: pos -> pos -> int

  val flowers: flower list
  val vacant: pos list
  val is_blocked:  pos -> bool

  val plant: t -> flower -> pos -> (t,unit) Result.t  
  
  val plant_exn: t -> flower -> pos -> t
  
  val pending: t -> flower option

  val choices: t -> flower -> pos list

  val empty: t

  val to_string: t -> string

end

module type Dimension = sig
  val height : int
  val width : int
  val blocked: (int*int) list
  val constraints: (char * (int * int)) list
end

module MakeGarden(Dim : Dimension) : Layout = struct
  module PosOrd = struct
    type t = (int * int)
  let compare (x0,y0) (x1, y1)= 
    match (compare y0 y1) with
      |0 -> compare x0 x1
      |r -> r

end
  
type flower = char
type pos = (int * int)

  
module PositionSet = Set.Make(PosOrd)
module PositionMap = Map.Make(PosOrd)
  module FlowerMap = Map.Make(
    struct
      type t = flower
      let compare = compare
    end)

type t = { flowers: flower PositionMap.t;  f_to_pos: pos list FlowerMap.t}

  let height = Dim.height
  let width = Dim.width
  let blocked = PositionSet.of_list Dim.blocked

  let constraints = Dim.constraints

  let flowers = List.map (fun (f,_) -> f) constraints

  let is_blocked (x,y) = 
    (x < 0 ) || (y < 0) || (x >= width) || (y >= height) || (PositionSet.mem (x,y) blocked)    


  let all_pos = PositionSet.of_list (List.concat 
      (List.init height (fun y ->
        List.init width (fun x -> (x,y))))) 
  
  let find_positions t f = 
    match (FlowerMap.find_opt f t.f_to_pos) with
    |Some(ps) -> ps
    |_ -> [] 
  
  let vacant = 
    let all = (PositionSet.filter (fun pos -> not (is_blocked pos)) all_pos) in
    PositionSet.fold (fun p acc -> p::acc) all []
    

  let distance a b = 
    let (x0,y0) = a in
    let (x1, y1) = b in
    (abs (x1-x0)) + (abs (y0 -y1))
  
  let pending t = 
      let count f = List.length (find_positions t f)  in
      List.find_map (fun (f, (qty, _)) -> if (count f) < qty then Some(f) else None ) constraints

      (* for a given flower valid positions *)
  let choices t f = 
    let (_, dist) = List.assoc f constraints in
    
    let valid (x,y) =
      if PositionMap.mem (x,y) t.flowers then false
      else 
        let positions = find_positions t f in
        List.for_all (fun p -> (distance p (x,y)) >= dist) positions
    
      in
    List.filter (fun p -> valid p) vacant 

  
  let empty = {flowers = PositionMap.empty; f_to_pos = FlowerMap.empty }
  
  let plant t f pos = 
      if is_blocked pos then Error () 
      else if (PositionMap.mem pos t.flowers) then Error ()
      else 
        let positions = (PositionMap.add pos f t.flowers) in
        let f_at_pos = match (FlowerMap.find_opt f t.f_to_pos) with
                            |Some(fs) -> pos::fs
                            |_ -> [pos] in

        Ok({
        flowers = positions;
        f_to_pos = (FlowerMap.add f f_at_pos t.f_to_pos) })

  let plant_exn t f pos = Result.get_ok (plant t f pos)
  
    
  let to_string t = 
    let buf = Buffer.create (height * width) in
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


module MkSolver(L : Layout) = struct    
  
  let search  = 
    let rec plant t =
      match (L.pending t) with
      |None -> Some(t)
      |Some(f) -> List.find_map (fun p -> plant (L.plant_exn t f p)) (L.choices t f) in
    plant L.empty        

end

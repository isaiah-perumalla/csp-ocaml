
type pos = (int * int)

type t = { bottom_right: pos}

let distance a b = 
  let (x0,y0) = a in
  let (x1, y1) = b in
  (abs (x1-x0)) + (abs (y0 -y1)) 

let create width height  = { bottom_right = (width, height)}

let width = function
|{bottom_right = (x, _)} -> x 
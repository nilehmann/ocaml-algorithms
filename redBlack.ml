type red_black = 
  | Empty
  | Red of red_black * int * red_black
  | Black of red_black * int * red_black

let rec search v = function
  | Empty -> false
  | Red (l, x, r)
  | Black (l, x, r) ->
      if v < x then search v l
      else if v > x then search v r
      else true

let balance_left l x r =
  match l, x, r with
  | Red(Red(a, x, b), y, c), z, d
  | Red(a, x, Red(b, y, c)), z, d ->
      Red(Black(a, x, b), y, Black(c, z, d))
  | l, x, r ->
      Black(l, x, r)

let balance_right l x r =
  match l, x, r with
  | a, x, Red(Red(b, y, c), z, d)
  | a, x, Red(b, y, Red(c, z, d)) ->
      Red(Black(a, x, b), y, Black(c, z, d))
  | l, x, r ->
      Black(l, x, r)

let blackify = function
  | Red(l, x, r) -> Black(l, x, r)
  | s -> s

let add v n =
  let rec add_aux = function
  | Empty -> Red(Empty, v, Empty)
  | Red (l, x, r) as n ->
      if v < x then Red(add_aux l, x, r)
      else if v > x then Red(l, x, add_aux r)
      else n
  | Black (l, x, r) as n->
      if v < x then balance_left (add_aux l) x r
      else if v > x then balance_right l x (add_aux r)
      else n
  in blackify (add_aux n)

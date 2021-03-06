open Batteries
open BatDynArray
type point = {x : int; y : int}

(** oa x ob *)
let cross o a b = (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x)

let sort =
  Array.sort (fun p1 p2 -> if p1.x = p2.x then p1.y-p2.y else p1.x-p2.x)

let convex_hull points =
  sort points;
  let hull = create () in
  let aux p = 
    let n = length hull in
    (cross (get hull (n - 2)) (get hull (n - 1)) p) <= 0 in

  for i = 0 to (Array.length points) - 1 do
    let p = points.(i) in
    while (length hull) >= 2 && (aux p) do
      delete_last hull
    done;
    add hull p
  done;

  let lower = length hull in

  for i = (Array.length points) - 2 downto 0 do
    let p = points.(i) in
    while (length hull) >= lower + 1 && (aux p) do
      delete_last hull
    done;
    add hull p
  done;
  delete_last hull;
  hull


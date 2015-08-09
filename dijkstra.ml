module IntInt =
  struct
   type t = int * int
   let compare (x0,y0) (x1,y1) = compare x0 x1
  end

module PairSet = Set.Make(IntInt)

let dijkstra g s e = 
  let n = Array.length g in
  let dist = Array.make n max_int in
  let q = ref PairSet.(empty |> add (0,s)) in
  dist.(s) <- 0;
  while not (PairSet.is_empty !q) do
    let (d, u) = PairSet.min_elt !q in
    q := PairSet.remove (d, u) !q;
    List.iter
      (fun (v, w) -> 
        let newdist = dist.(u) + w in
        if newdist < dist.(v) then
        begin
          q := PairSet.add (newdist, v) !q;
          dist.(v) <- newdist
        end
      )
      g.(u)
  done;
  dist.(e);;

let g = Array.make 6 [] in
g.(0) <- [(1,7); (2,9); (5, 14)];
g.(1) <- [(0,7); (2,10); (3,15)];
g.(2) <- [(0,9); (1,10); (3,11); (5,2)];
g.(3) <- [(1,15); (2,11); (4,6)];
g.(4) <- [(3,6); (5,9)];
g.(5) <- [(0,14); (2,2); (4,9)];
print_int (dijkstra g 0 4);
print_newline ()

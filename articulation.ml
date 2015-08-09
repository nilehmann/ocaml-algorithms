let n = 10
let parent = Array.make n 0 
let level = Array.make n (-1)
let low = Array.make n 0
let is_art = Array.make n false
    
let rec dfs_visit g u p l =
  parent.(u) <- p;
  level.(u) <- l;
  low.(u) <- l;
  List.iter 
    (fun v ->
      if v = p then
        ()
      else if level.(v) = -1 then begin
        dfs_visit g v u (l+1);

        if parent.(u) = -1 && (List.length g.(u)) > 1 then
          is_art.(u) <- true;
        if parent.(u) != -1 && low.(v) >= low.(u) then
          is_art.(u) <- true;
      end
      else if level.(v) < level.(u) then
        low.(u) <- min low.(u) level.(v)
    )
    g.(u)

let arts g = 
  for i = 0 to (Array.length g) - 1 do
    dfs_visit g i (-1) 0
  done


let get f i =
  let s = ref 0 in
  let x = ref i in
  while !x > 0 do
    s := !s + f.(!x);
    x := !x - (!x land -(!x))
  done;
  !s

let get f i j = (get f j) - (get f (i - 1))

let update f i v =
  let x = ref i in
  while !x <= (Array.length f) do
    f.(!x) <- f.(!x) + v;
    x := !x + (!x land -(!x))
  done

let init f arr = 
  Array.iteri (fun i v -> update f (i+1) v) arr;;

let arr = [|1;3;4;5;4;5|] in
let n = (Array.length arr) in
let f = Array.make (n + 1) 0 in
init f arr;
print_int (get f 1 3);
print_newline ();
update f 2 (-2);
print_int (get f 1 4);
print_newline ();;

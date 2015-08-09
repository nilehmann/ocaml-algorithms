let left n = 2 * n
let right n = 2 * n + 1

let rec init arr tree n a b =
  if a = b then
    tree.(n) <- arr.(a)
  else
    let m = (a + b) / 2 in
    init arr tree (left n) a m;
    init arr tree (right n) (m+1) b;
    tree.(n) <- tree.(left n) + tree.(right n)

let rec get tree n a b i j = 
  if j < a || i > b then
    0
  else if i <= a && b <= j then
    tree.(n)
  else
    let m = (a + b) / 2 in
    (get tree (left n) a m i j) + (get tree (right n) (m+1) b i j)

let rec update tree n a b i v = 
  if i < a && i > b then
    ()
  else if a = b then
    tree.(n) <- v
  else
    let m = (a + b) / 2 in
    update tree (left n) a m i v;
    update tree (right n) (m+1) b i v;
    tree.(n) <- tree.(left n) + tree.(right n);;

let arr = [|0;1;3;4;51;4;5|] in
let n = (Array.length arr) - 1 in
let tree = Array.make (4*n) 0 in
init arr tree 1 1 n;
print_int (get tree 1 1 n 1 3);
print_newline ();
update tree 1 1 n 2 2;
print_int (get tree 1 1 n 1 3);
print_newline ();;


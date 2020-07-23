open Printf;;
open Scanf;;
open Array;;
let count = ref 0;;
let rec lern n =
  let n = scanf "%d" (fun x -> x) in
  if n >= 1 && n <= 50 then n else lern 0;;

let rec lerk k =  
  let k = scanf " %d" (fun x -> x) in
  if k >= 1 && k <= 8 then k else lerk 0;;
  
let rec lerab a b n =  
  let (a, b) = scanf " %d %d" (fun x y -> (x,y)) in
  if a >= 0 && a < n && b >=0 && b < n then (a, b) else lerab 0 0 n;;

let n = lern 0
let k = lerk 0
let chess = Array.make_matrix n n 0
let (a, b) = lerab 0 0 n

let rec contarMovimentos chess n a b k pp=
  let verticalJumps = [|-1; -2; 1; -2; -1; 2; 2; 1|] in
  let horizontalJumps = [|-2; -1; 2; 1; 2; -1; 1; -2|] in
  if pp > 7 then 0
  else if k = 0 then 1 
  else if a+verticalJumps.(pp) > (-1) && a+verticalJumps.(pp) < n && b+horizontalJumps.(pp) > (-1) && b+horizontalJumps.(pp) < n then begin
    contarMovimentos (chess) (n) (a) (b) (k) (pp+1) + contarMovimentos (chess) (n) (a+verticalJumps.(pp)) (b+horizontalJumps.(pp)) (k-1) 0;
  end
  else contarMovimentos (chess) (n) (a) (b) (k) (pp+1);
;;
count := contarMovimentos (chess) (n) (a) (b) (k) 0;;
printf "%d\n" (!count);;

 

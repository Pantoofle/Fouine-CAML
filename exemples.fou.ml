
let f (x:int) : int : int = 2 in f;;
let (x : int) : int : int = 2 in x;;


let f (x:int) (y:int) : int = 3 in f ;;
let (f : int -> int -> int) x y = 2 in f ;;
let f x y : int = 2 in f ;;

let x,y = (fun _ -> 1), fun _ -> 2 in x;;

let x = 5 in x;;

let rec f = fun x -> if x then x else f (not x) in f (2=2);;

let (f:int->int) x:int=2 in f;;


let y f =
  let auto x = x x in
  (fun f -> auto (fun x -> f (auto x))) f
in y
;;

1;2;;
4+3;;
7;;

let rec x = 2+2 in x+3;;

if (2 < 3 + 4) then 0 else 1;;

2 + 3;;

let f x y = x * y in
let g x = x - 2 in
let h z = fun x y -> 2*z + x/y in
f (g 3) 4 + f 2 (g 4)
;;

let f x y = x * y in f 3 4;;
let g x = x - 2 in g 3;;

let f x = 2 in
let f x = 3 in
(2=3);;

let rec f = fun x -> 42 in f 3 ;;

(* bug du let rec *)

let rec f x = 42 in f 3 ;;
let rec f x = if (x<0) then 0 else 1 + f (x-1) in f 5 ;;
let rec f x = if (x=0) then 0 else 1 + f (x-1) in f 5 ;;
let rec f x = if (x=0) then 0 else 1 + f (x-1) in f ;;

let rec f x =
  let rec g y =
    if y<5 then y else f (x-1) + g (y-1)
  in if x<3 then x else 2 * f (x-1) * g (x-1)
in f 5;;

let rec f x =
  let rec g y =
    if y<5 then y else f (x-1) + g (y-1)
  in if x<3 then x else 2 * f (x-1) * g (x-1)
in f;;

let rec x = 2 in x ;;
if let rec x = 2=2 in x then 42 else 23;;

!(ref 4);;

let comp f g = fun x -> f g x in comp (fun y->y+1);;

let f x = !x in f (ref 42);;

let rec x = 3+4 in let f x = !x in f (ref x);;

let f = ref (fun y z -> y+z) in f:= (fun x y -> x+y); (!f) 3;;

let f = ref (fun x y -> x+y) in f;;

let f = fun x -> x in f;;
let f = fun x y -> x+y in f;;
let f = fun x y z -> x+y+z in f;;

(*
let x = ref (2=2) in !x;;

let x = ref 4 in feedback (!x+1); fun x -> x;;

let y=2 in feedback y;;
let y=2 in prInt y;;

ref 42;;
ref fun x -> x;;

let f x y = x in f (fun x -> x) 1 2;;

try raise E (1+2) with E x -> if x = 3 then 42 else 0;;

raise E 42; 2+2;;

let x = raise E 42 in 3;;
raise E (!ref 42);;

(*try raise E (fun x -> x) with E x -> if x = 3 then 42 else 0;;*)

ref 3 := 2 ;;

let (a,b) = (2,3) in a;;
let (a,a) = (2,3) in a;;

(*let f (x:int) = x in f (2=2);;*)
*)

let rec f x = x in f;;
let rec f = fun x -> x in f;;
let rec f = fun x y z -> x in f;;
let f = fun x -> x in f;;

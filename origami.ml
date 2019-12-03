type point = float * float

type line = point * point

let (--) (x, y) (z, w)=
	(x -. z, y -. w)

let (++) (x, y) (z, w)=
	(x +. z, y +. w)

let ( *$ ) (x, y) a =
	((x *. a), (y *. a))

(* Squared distance from p to q *)
let distance2 p q = 
	let (x, y) = q -- p
	in (x**2. +. y**2.)

let reflect (p1, p2) q =
	(* Ratio ±|q' -- p1| / |p2 -- p1| where q' is the projection of q onto (p1,p2) and |v| is the length of v*)
	(* Nonnegative iff points appear in the order p1,q',p2 *)
	let projectionR =
		let (x,y),(z,w) = (p2 -- p1),(q -- p1)
		in ((x*.z +. y*.w) /. (distance2 p1 p2))
	in (((p1 ++ ((p2 -- p1) *$ projectionR))) *$ 2.) -- q


let epsilon = 1e-6

(* Checks which side of line (p1,p2) is q*)
(* q is to the right of (p1,p2) line 	-> Returns -1 *)
(* q is on the (p1,p2) line 		-> Returns 0  *)
(* q is to the left of (p1,p2) line 	-> Returns 1  *)
let whichside (p1, p2) q = 
	let abs x =
		max x (-.x)
	in let (x, y),(z, w) = (p2 -- p1), (q -- p1)
	in let crossProduct = (x *. w) -. (y *. z) 
	in if ((abs crossProduct) < epsilon) then 0
	else compare crossProduct 0.

type kartka = point -> int

let prostokat (a,b) (c,d) (x,y) =
	if (( (min a c)-.epsilon <= x  && x <= (max a c)+.epsilon ) &&
	    ( (min b d)-.epsilon <= y  && y <= (max b d)+.epsilon )) 
	then 1 else 0

let kolko p r q=
	if (sqrt (distance2 p q))-.epsilon <= r then 1 else 0

let zloz p1 p2 k q= 
	match (whichside (p1,p2) q) with
		| -1 -> 0
		| 0 -> k q
		| 1 -> (k q) + (k (reflect (p1,p2) q))

let skladaj llist k =
	List.fold_left (fun a (p1,p2) -> zloz p1 p2 a) k llist

(*Author: Jakub Szulc*)

let compose f g x = f (g x)

let forall w f = List.map f w

(*Return list of (f u' v') for any pair u' and v' belonging to list u and v respectively*)
(*ie. (forallall [1;2] [3;4] (+))= [4;5;5;6]*)
let forallall u v f=
	let rec pom1 l acc f=
		let rec pom2 ll a acc f=
			match ll with
			| [] -> acc
			| h::t -> pom2 t a ((f a h)::acc) f
		in match l with
		| [] -> List.rev acc
		| h::t -> pom1 t (pom2 v h acc f) f
	in pom1 u [] f

(*Generalized min for lists*)
(*ignores nan's*)
let min_ l = List.fold_left min infinity (List.filter (compose not Float.is_nan) l)

(*Generalized max for lists*)
(*ignores nan's*)
let max_ l = List.fold_left max neg_infinity (List.filter (compose not Float.is_nan) l)

(*Compact set so that it's represented by at most 2 intervals*)
let compact w = 
	(*sprawdzanie, czy dwa przedziały można złączyć*)
	let compactable (a,b) (c,d) = 
		((c <= b) && (d >= a))
	(*łączenie dwóch przedziałów*)
	in let compact (a,b) (c,d) =
		((min a c),(max b d))
	in let pom [u;v] w =
		let _ = assert ((compactable u w) || (compactable v w))
		in if (compactable u w) then [(compact u w);v]
		else [u; (compact v w)]
	in if ((List.length w) <= 2) then w
	else List.fold_left pom [(List.hd w); (List.nth w 1)] w

(*Sum of intervals*)
type wartosc=((float * float) list)

let wartosc_dokladnosc x y=	[(min (x-.(x*.y/.100.)) (x+.(x*.y/.100.))),
				(max (x-.(x*.y/.100.)) (x+.(x*.y/.100.)))]

let wartosc_od_do x y=[(x,y)]

let wartosc_dokladna x=[(x,x)]

let wartosc_nan = wartosc_dokladna nan

let rec has_nan w = 
	match w with
	| [] -> false
	| (a,b)::t -> (Float.is_nan a) || (Float.is_nan b) || has_nan t

(*Is every interval finite?*)
(*is_finite implies !has_nan*)
let rec is_finite w=
	match w with
	| [] -> true
	| (a,b)::t -> (Float.is_finite a) && (Float.is_finite b) && is_finite t

let in_wartosc w x=
	let rec pom w x=
		match w with
		| [] -> false
		| (a,b)::t -> (a <= x && x <= b) || pom t x
	in if has_nan w then false else pom w x

let min_wartosc w=
	let rec pom w=
		match w with
		| [] -> infinity
		| (a,b)::t -> min a (pom t)
	in if has_nan w then nan else pom w

let max_wartosc w=
	let rec pom w=
		match w with
		| [] -> neg_infinity
		| (a,b)::t -> max b (pom t)
	in if has_nan w then nan else pom w

let sr_wartosc w=
	(min_wartosc w +. max_wartosc w)/.2.

let rec plus u v=
	let pom (a,b) (c,d)=
		(a+.c,b+.d)
	in if ((has_nan u) || (has_nan v)) then wartosc_nan else (compact (forallall u v pom))

let rec minus u v=
	let pom (a,b) (c,d)=
		(a-.d,b-.c)
	in if ((has_nan u) || (has_nan v)) then wartosc_nan else (compact (forallall u v pom))

let rec razy u v=
	let ( *. ) x y = 
		match (x,y) with
		| (neg_infinity,0.0) -> 0.0
		| (infinity,0.0) -> 0.0
		| (0.0,neg_infinity) -> 0.0
		| (0.0,infinity) -> 0.0
		| _,_ -> x*.y
	in let pom (a,b) (c,d)=
		(min_ [a*.c;a*.d;b*.c;b*.d],
		 max_ [a*.c;a*.d;b*.c;b*.d])
	in if ((has_nan u) || (has_nan v)) then wartosc_nan else (compact (forallall u v pom))

let odwrotnosc w=
	(*jeśli przedział zawiera zero to rozdzielamy go na część dodatnią i ujemną*)
	let rec pom = function
		| (a,0.) when a<0. -> [(neg_infinity,(1./.a))]
		| (a,0.) when a>=0. -> []
		| (0.,b) when b>0. -> [((1./.b),infinity)]
		| (0.,b) when b<=0. -> []
		| (a,b)	 when (in_wartosc (wartosc_od_do a b) 0.) -> (pom (a,0.))@(pom(0.,b))
		| (a,b)-> [(min (1./.a) (1./.b)),(max (1./.a) (1./.b))]
	in let ans = List.flatten (forall w pom) 
	(*jeśli np. w =[(0.,0.)] to ans=[], więć trzeba to sprawdzić*)
	in if ((has_nan w) || (ans=[])) then wartosc_nan else (compact ans)

let podzielic u v=
	razy u (odwrotnosc v)


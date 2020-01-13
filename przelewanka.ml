
exception Found of int

let (<--) s (i,x) = 
	let s' = Array.copy s
	in (s'.(i) <- x; s')

let rec gcd x y = 
	if y=0 then x
	else gcd y (x mod y)
 
let reachable a =
	let array_filter f a = 
		Array.of_list (List.filter f (Array.to_list a))
	in let a = array_filter (fun (x,y) -> x<>0) a
	in let n = Array.length a
	in let a_capacity = Array.map fst a
	in let a_goal = Array.map snd a
	in let gcd_ = Array.fold_left (fun acc x -> gcd acc x) 0 a_capacity
	in
	   (n=0)
	|| ((Array.fold_left (fun acc (x,y) -> if (y=x || y=0) then true else acc) false a)
	&&  (Array.fold_left (fun acc y   -> if (y mod gcd_ = 0) then acc else false) true a_goal))

let rec print_list f l = ()
	(*match l with
	| [] -> print_string "\n"
	| h::t -> f h ; print_string " " ; print_list f t *)

let print_array f a = ()
	(*print_list f (Array.to_list a)*)

let przelewanka a =
	try
		let n = Array.length a
		in let a_capacity = Array.map fst a
		in let a_goal = Array.map snd a
		in let visited = Hashtbl.create 1 
		in let when_visited s =
			if (Hashtbl.mem visited s) then (Hashtbl.find visited s)
			else (-1)
		in let set_visited s time =
			Hashtbl.replace visited s time;
			if s=a_goal then raise (Found time)
		in let q = Queue.create ()
		in let empty = Array.make n 0
		in if (not (reachable a)) then (-1)
		else begin
			Queue.push empty q;
			set_visited empty 0;
			while (not (Queue.is_empty q)) && ((when_visited a_goal)=(-1)) do
				let s = Queue.pop q
				(*in let _ = (print_string "Visiting now \n"; print_array print_int s)*)
				in let time = (when_visited s)
				in let add s' =
					if(when_visited s' = -1) then
						begin
							set_visited s' (time+1);
							Queue.push s' q
						end
				in
					for i=0 to (n-1) do
							(*print_int i; print_string "\n";*)
							if(s.(i)>0) then add (s <-- (i,0));

							if(s.(i)<a_capacity.(i)) then add (s <-- (i,a_capacity.(i)));

							for j=0 to (n-1) do 
								if (i<>j) && (s.(i)>0) && (s.(j)<a_capacity.(j)) then 
									let new_sj = min a_capacity.(j) (s.(i)+s.(j))
									in let new_si = s.(i) - ( new_sj - (s.(j)) )
									in add (s <-- (i,new_si) <-- (j,new_sj))
							done
					done
			done;
			when_visited a_goal
		end
	with 
	| (Found time) -> time
	| e -> raise e
		

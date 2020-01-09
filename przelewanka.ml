
let (<--) s (i,x) = 
	let s' = Array.copy s
	in (s'.(i) <- x; s')

in let rec ( *** ) la lb =
	match (la,lb) with
	| [],[] -> []
	| h::t, _ -> (List.map (fun x -> (h,x)) lb) @ (t *** lb)
	| _, h::t -> (List.map (fun x -> (x,h)) la) @ (la *** t)

in let rec range n = 
	match n with 
	| 0 -> []
	| _ -> (range (n-1))@[n-1]

in let rec print_list f l = 
	match l with
	| [] -> print_string "\n"
	| h::t -> f h ; print_string " " ; print_list f t

in let print_array f a = 
	print_list f (Array.to_list a)

in let przelewanka a =
	let n = Array.length a
	in let a_fst = Array.map fst a
	in let a_snd = Array.map snd a
	in let range_n = range n
	in let visited = Hashtbl.create 1 
	in let when_visited s =
		if (Hashtbl.mem visited s) then (Hashtbl.find visited s)
		else (-1)
	in let set_visited s time = 
		Hashtbl.replace visited s time
	in let gen_states s =
		  List.fold_left (fun acc i -> 
						if s.(i)>0 then (s <-- (i,0))::acc 
						else acc) 
				[] range_n
		@ List.fold_left (fun acc i ->
						if s.(i)< a_snd.(i) then (s <-- (i,a_snd.(i)))::acc 
						 else acc)
				 [] range_n
		@ List.fold_left (fun acc (i,j) -> 
						if (i<>j) && (s.(i)>0) && (s.(j)<a_snd.(j)) then 
							let new_sj = min a_snd.(j) s.(i)+s.(j)
							in let new_si = s.(i) - ( new_sj - (s.(j)) )
							in (s <-- (i,new_si) <-- (j,new_sj))::acc
						else acc) 
				[] (range_n *** range_n)
	in let q = Queue.create ()
	in begin
		Queue.push (Array.make n 0) q;
		set_visited 
		while (not (Queue.is_empty q)) && (not (is_visited a_fst)) do
			let top = Queue.pop q
			in begin
				set_visited top;
				List.iter (fun s -> if (not (is_visited s)) then Queue.push s q) (gen_states top)
			end
		done;
		when_visited a_fst
	end
in przelewanka [|(0,3);(2,5)|]
		

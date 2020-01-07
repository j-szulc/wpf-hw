exception Cykliczne

let topol l =
	(* Hashtbl storing the list of neighbours for every node *)
	(* By neighbour we mean a node [u] such that there's an edge [v] -> [u] *)
	let graph = Hashtbl.create (List.length l)
	(* Hashtbl storing the degree of each node *)
	(* By degree we mean the number of incoming edges *)
	in let deg = Hashtbl.create (List.length l)
	(* Stack storing all sources *)
	(* By source we mean a node with degree 0 *)
	in let sources = Stack.create ()
	in let result = ref []	
	in let is_graph_empty () = 
		Hashtbl.fold (fun _ d acc -> if d<0 then acc else false) deg true
	(* Return the list of neighbours of [v] *)
	(* By neighbour we mean a node [u] such that there's an edge [v] -> [u] *)
	in let get_nb v =
		if (Hashtbl.mem graph v) then (Hashtbl.find graph v)
		else []
	(* Draw new edges from [v] to all nodes in [nb_list] *)
	in let append_nb v nb_list=
		Hashtbl.replace graph v (nb_list@(get_nb v))
	in let get_deg v =
		if (Hashtbl.mem deg v) then (Hashtbl.find deg v)
		else 0
	(* Update the degree of [v] in the 'deg' Hashtbl by adding [a] to it *)
	in let add_deg v a =
		Hashtbl.replace deg v ((get_deg v)+a)
	in begin
		(* Build the 'graph' Hashtbl from l *)
		List.iter (fun (v,nb_list) -> append_nb v nb_list) l;
		(* Calculate the degree and store it in the 'deg' Hashtbl *)
		Hashtbl.iter 
		(fun v nb_list-> 
			(* We want every node to have an entry in the 'deg' Hashtbl even if it's zero *)
			(* Otherwise we wouldn't be able to find the first source *)
			add_deg v 0;

			List.iter (fun nb -> add_deg nb 1) nb_list)
		graph;

		(* Find the first source and put it in the 'sources' Stack *)
		Hashtbl.iter (fun v d -> if d=0 then Stack.push v sources) deg;

		while (not (Stack.is_empty sources)) do
			let s = (Stack.pop sources)
			in begin
				result := s::(!result);
				(* We 'remove' the node by making the degree negative *)				
				add_deg s (-1);
				List.iter 
				(fun nb -> 
					add_deg nb (-1);
					if (get_deg nb)=0 then Stack.push nb sources) 
				(get_nb s)
			end
		done;
		
		(* If we ran out of sources before emptying the graph, the graph was cyclic *)
		if (is_graph_empty ()) then List.rev (!result)
		else raise Cykliczne
	end
				
(*				
(* Autor: M.Drozdowski
 * Licence: Unlicensed
 * Original repo: https://github.com/mdrozdowski1996/topoli *)

(******** SPRAWDZACZKA *********)
(**** POZYCJA a W LIŚCIE l *****)
let pos a l = 
	let poz = ref 0 and use = ref false in
	List.iter (fun x -> if (x <> a && (!use) = false) then poz := (!poz) + 1
	           else use := true ) l;
	(!poz);;	
(***FUNKCJA SPRAWDZAJĄCA POPRAWNOŚĆ ROZWIĄZANIA, GDY NIE MA CYKLU***)
let is_valid l ans =
	let rec pom_valid acc li ans =
		match li with 
			|[] -> acc
			|(f,s)::t -> 
				if (List.fold_left (fun a x -> (if (pos f ans) > (pos x ans) then false else a)) 
				(true) s) then (pom_valid acc t ans)
				else (pom_valid false t ans)
	in pom_valid true l ans	

			 
(*******TESTY*******)
(*******CYKLICZNE*******)

let l = (1,[1])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
let l =(1,[2])::(2,[3])::(3,[2])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
let l = (1,[2])::(2,[3])::(3,[4;5])::(4,[5])::[];;
assert(is_valid l (topol l));;
let l = (1,[2])::(2,[3])::(3,[4;5])::(4,[2;5])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;

(******NIEISTNIEJĄCE_W_LIŚCIE_GŁÓWNEJ_WIERZCHOŁKI******)
let l = [];;
let l = (1,[0])::l;;
assert (is_valid l (topol l));;
let l = (2,[0])::l;;
assert (is_valid l (topol l));;

(*******SPRAWDZENIE_CZY_DZIAŁA_NA_INNYCH_TYPACH_NIŻ_INT******)
let l = [];;
let l = ('a',['b';'d'])::l;;
let l = ('b',['c';'d'])::l;;
let l = ('c',['d'])::l;;
assert (is_valid l (topol l));;
let l = [];;
let l = ("fst",["snd";"thr"])::l;;
let l = ("xyz",["abc";"snd"])::l;;
let l = ("cos",["fst";"xyz"])::l;;
assert (is_valid l (topol l));;
let l = [];;
let l = (true,[false])::l;;
let l = (false,[true])::l;;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;

(******TESTY_RÓŻNE******)
let l = [];;
let l = (1,[0])::l;;
assert (is_valid l (topol l));;
let l = (0,[2])::l;;
assert (is_valid l (topol l));;
let l = (2,[3])::l;;
assert (is_valid l (topol l));;
let l = (4,[2;3])::l;;
assert (is_valid l (topol l));;
let l = (6,[2;3])::l;;
assert (is_valid l (topol l));;
let l = (9,[10;11])::l;;
assert (is_valid l (topol l));;
let l = (10,[9])::l;;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
*)
			
		

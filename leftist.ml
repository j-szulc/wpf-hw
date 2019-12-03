(*Author: Jakub Szulc	*)

type 'a queue = Node of { left : 'a queue; priority : 'a; right : 'a queue; length : int } | Null

let empty = Null

(*Exception thrown by [delete_min] when trying to remove from an empty list*)
exception Empty

(*Joining two priority queues*)
let rec join q1 q2 = 
	match (q1,q2) with
	| Null, _ -> q2
	| _, Null -> q1
	| Node n1, Node n2 when n1.priority > n2.priority -> 
		join q2 q1
	| Node n1, Node n2 ->
		(* Sortuje kolejki w kolejności nierosnącej pod względem długości *)
		(* Zakładamy, że kolejka Null ma najmniejszą długość (wynoszącą 0) *)
		let sortq q1 q2=
			match (q1,q2) with
			| Null, _ -> (q2,q1)
			| _, Null -> (q1,q2)
			|(Node n1), (Node n2) ->
				if (n1.length < n2.length) then
					(q2,q1)
				else
					(q1,q2)
		in let (nowyleft,nowyright) = sortq (n1.left) (join (n1.right) q2)
		in Node{ 	
			left = nowyleft;
			priority = (n1.priority);
			right = nowyright;
			length = 
				(*Jeśli np. right podkopiec jest pusty, to skrajnie prawa ścieżka znajduje się w leftm podkopcu*)
				match nowyleft,nowyright with
				| Null, Null -> 0+1
				| Node n, Null -> n.length+1
				| _, Node n -> n.length+1
		}

(*Add element e to queue q*)
let add e q = 
	join (Node {left=Null; priority=e; right=Null; length=1}) q

(*Remove minimal element (root) of q *)
let delete_min q = 
	match q with
	| Null -> raise Empty
	| Node n -> (n.priority, (join (n.left) (n.right)))

let is_empty q = 
	(q = Null)

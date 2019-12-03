(* Author: Jakub Szulc *)

type interval = int * int

(* Polymorphic set *)
type 'k set =
  | Empty
  | Node of 'k set * 'k * 'k set * int * int

(* Type representing a set of integers as a set of intervals (also called iset) *)
type t = interval set

(* An empty iset *)
let empty = Empty

(* Checks if iset is empty *)
let is_empty s = (s = Empty)

(* [which n s] returns either: *)
(* None, if iset [s] doesn't contain number [n] *)
(* or Some (a, b), if (a, b) is the interval containing [n] in iset [s] *)
let rec which n s =
  let target (a, b) =
    (a <= n) && (n <= b)
  in match s with
    | Empty -> None
    | Node(l, k, r, _, _) when (target k) -> Some k
    | Node(l, (a, b), r, _, _) when n < a -> which n l
    | Node(l, (a, b), r, _, _) when n > b -> which n r
    | _ -> assert false

(* [mem n s] returns [true] if [s] contains [n], and [false] otherwise. *)
let mem n s =
  (which n s) <> None

(* Return [x + y] if [x + y < max_int] and [max_int] otherwise *)
(* Assumes [x, y >= 0] *)
let (++) x y =
  if (x+y < 0) then max_int
  else x+y

(* Return [x - y] if [x-y < max_int] and [max_int] otherwise *)
(* Assumes [x - y >= 0] *)
let (--) x y =
  if (x-y < 0) then max_int
  else x-y

(** FROM PSET LIBRARY **)
(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996 - 2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 * License: LGPL
*)

(* Returns height of given iset *)
let height = function
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

(*Returns number of elements in given subtree if it's less than max_int or max_int otherwise*)
let count = function
  | Node (_, _, _, _, c) -> c
  | Empty -> 0

(* Combines isets [l] and [r] with [k] as a root *)
let make l ((a,b) as k) r = Node (l, k, r, max (height l) (height r) + 1, (count l) ++ (b -- a ++ 1) ++				 (count r))

(* Balances and combines isets [l] and [r] with [k] as a root *)
(* Is not recursive and reduces difference of heights by at most 1 *)
(* Assumes [li] < [k] < [ri] for every interval [li] in [l] and [ri] in [r] *)
let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

(* Returns the minimal interval of given iset *)
let rec min_elt = function
  | Node (Empty, k, _, _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found

(* Returns given iset with minimal interval removed *)
let rec remove_min_elt = function
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

(* Merges two isets *)
(* Is not recursive and reduces difference of heights by at most 1 *)
(* Similar to [bal], but does not need a new root *)
let merge t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      bal t1 k (remove_min_elt t2)

(* Adds interval [x] to the iset [s] *)
(* Unsafe - treats [x] as a 'new member' of [s] and [add_unsafe x s] may not be a valid iset *)
(* i.e. assumes every interval in [s] is away from [x] by at least 2 *)
(* i.e. if [x] = (a, b) then assumes [s] is disjoint with (a - 1, b + 1) *)
let rec add_unsafe x s =
  match s with
  | Node (l, k, r, h, _) ->
      let c = compare x k in
      if c = 0 then s
      else if c < 0 then
        let nl = add_unsafe x l in
        bal nl k r
      else
        let nr = add_unsafe x r in
        bal l k nr
  | Empty -> make Empty x Empty

(* Joins and balances isets [l] and [r] with [v] as a candidate for the root *)
(* Similar to [bal] but is recursive (difference in heights between [l] and [r] may be arbitrary) *)
(* Assumes [li] < [k] < [ri] for every interval [li] in [l] and [ri] in [r] *)
let rec join l v r =
  match (l, r) with
    (Empty, _) -> add_unsafe v r
  | (_, Empty) -> add_unsafe v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* [split_unsafe x s] returns a triple [(l, present, r)], where
    [l] is the set of intervals of [s] that are strictly lesser than interval [x];
    [r] is the set of intervals of [s] that are strictly greater than interval [x];
    [present] is [false] if [s] contains no interval equal to interval [x],
    or [true] if [s] contains an interval equal to interval [x]. *)
(* Unsafe - assumes that for every interval [i] in [set] either [i] < [x] or [i] = [x] or [i] > [x] *)
let split_unsafe x set =
  let rec loop x = function
      Empty ->
        (Empty, false, Empty)
    | Node (l, v, r, _, _) ->
        let c = compare x v in
        if c = 0 then (l, true, r)
        else if c < 0 then
          let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
  in
  let setl, pres, setr = loop x set in
  (setl, pres, setr)

(* Removes interval [x] from iset [set] *)
(* Unsafe - assumes [x] is a member of [set] *)
let remove_unsafe x set =
  let rec loop = function
    | Node (l, k, r, _, _) ->
        let c = compare x k in
        if c = 0 then merge l r else
        if c < 0 then bal (loop l) k r else bal l k (loop r)
    | Empty -> Empty in
  (loop set)

(* [iter f s] applies [f] to all continuous intervals in the set [s].
    The intervals are passed to [f] in increasing order. *)
let iter f set =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop set

(* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
    ... xN are all continuous intervals of s, in increasing order. *)
let fold f set acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc set

(* Return the list of all continuous intervals of the given set.
    The returned list is sorted in increasing order. *)
let elements set =
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] set

(** NOT FROM PSET LIBRARY **)

(* Merges two isets *)
(* Similar to [merge] but uses recursive [join] instead of [bal] so the difference in heights may be arbitrary *)
(* Similar to [join], but does not need a new root *)
let merge_full t1 t2 =
  match t1, t2 with
  | Empty, _ -> t2
  | _, Empty -> t1
  | _ ->
      let k = min_elt t2 in
      join t1 k (remove_min_elt t2)

(* [[split x s] returns a triple [(l, present, r)], where
    [l] is the iset of all elements of [s] strictly lesser than [x];
    [r] is the iset of all elements of [s] strictly greater than [x];
    [present] is [false] if [s] contains no element equal to [x],
    or [true] if [s] contains an element equal to [x]. *)
(* Similar to [split_unsafe x s], but x can be an arbitrary number *)
let split n s =
  (* Function to add list of intervals [l] to [acc] using [add_unsafe] *)
  let rec add_list_unsafe acc l =
    List.fold_left (fun a x -> (add_unsafe x a)) acc l
  (* If there's an interval containing n, split it into two parts *)
  (* Otherwise [s'] = [s] *)
  in let s' = match (which n s) with
    | None -> s
    | Some (a, b) when ((a < n) && (n < b)) -> add_list_unsafe (remove_unsafe (a, b) s) [(a, n - 1); (n + 1, b)]
    | Some (a, b) when ((a = n) && (n < b)) -> add_list_unsafe (remove_unsafe (a, b) s) [(n + 1, b)]
    | Some (a, b) when ((a < n) && (n = b)) -> add_list_unsafe (remove_unsafe (a, b) s) [(a, n - 1)]
    | Some (a, b) when ((a = n) && (n = b)) -> add_list_unsafe (remove_unsafe (a, b) s) []
    | _ -> assert false
  (* Now [s'] satisfies conditions of [split_unsafe] *)
  in let (l, _, r) = split_unsafe (n, n) s'
  in (l, (mem n s), r)

(* [below n s] returns the quantity of numbers in [s] that are lesser
    or equal to [n]. If there are more than max_int such elements,
    the result should be max_int. *)
let below n s =
  let fst (x, _, _) = x
  in let c = count (fst (split n s))
  in if (mem n s) then c ++ 1
  else c

(* [remove (x, y) s] returns an iset containing the same numbers as [s],
    except for all those which are included between [x] and [y].
    Assumes [x <= y]. *)
let remove (a, b) s =
  let fst (x, _, _) = x
  and trd (_, _, x) = x
  (* Split [s] into two parts: less than [a], greater than [b] and merge them together *)
  in merge_full (fst (split a s)) (trd (split b s))

(* [add (x, y) s] returns an iset containing the same numbers as [s],
    plus all numbers of the interval [[x, y]] including [x] and [y].
    Assumes [x <= y]. *)
let add (a, b) s =
  (* Calculate the big interval covering (a, b) and its neighbours (if there are any) *)
  let lside =
    if ((a <> min_int) && (mem (a - 1) s)) then (let Some (x, y) = (which (a - 1) s) in x)
    else a
  and rside =
    if ((b <> max_int) && (mem (b + 1) s)) then (let Some (x, y) = (which (b + 1) s) in y)
    else b
  (* Replace everything in [s] from [lside] to [rside] by a single interval *)
  in (add_unsafe (lside, rside) (remove (lside, rside) s))

(** TESTS **)

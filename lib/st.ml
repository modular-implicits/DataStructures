


(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Sets over ordered types *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty: t
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val disjoint: t -> t -> bool
    val diff: t -> t -> t
    val cardinal: t -> int
    val elements: t -> elt list
    val min_elt: t -> elt
    val min_elt_opt: t -> elt option
    val max_elt: t -> elt
    val max_elt_opt: t -> elt option
    val choose: t -> elt
    val choose_opt: t -> elt option
    val find: elt -> t -> elt
    val find_opt: elt -> t -> elt option
    val find_first: (elt -> bool) -> t -> elt
    val find_first_opt: (elt -> bool) -> t -> elt option
    val find_last: (elt -> bool) -> t -> elt
    val find_last_opt: (elt -> bool) -> t -> elt option
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val map: (elt -> elt) -> t -> t
    val filter: (elt -> bool) -> t -> t
    val filter_map: (elt -> elt option) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val split: elt -> t -> t * bool * t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val subset: t -> t -> bool
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list: elt list -> t
  end

let empty {M : S} = M.empty 
let add {M : S} = M.add 
let singleton {M : S} = M.singleton 
let remove {M : S} = M.remove 
let union {M : S} = M.union 
let inter {M : S} = M.inter 
let disjoint {M : S} = M.disjoint 
let diff {M : S} = M.diff 
let cardinal {M : S} = M.cardinal   
let elements {M : S} = M.elements 
let min_elt {M : S} = M.min_elt 
let min_elt_opt {M : S} = M.min_elt_opt 
let max_elt {M : S} = M.max_elt 
let max_elt_opt {M : S} = M.max_elt_opt 
let choose {M : S} = M.choose 
let choose_opt {M : S} = M.choose_opt 
let find {M : S} = M.find 
let find_opt {M : S} = M.find_opt 
let find_first {M : S} = M.find_first 
let find_first_opt {M : S} = M.find_first_opt 
let find_last {M : S} = M.find_last 
let find_last_opt {M : S} = M.find_last_opt 
let iter {M : S} = M.iter 
let fold {M : S} = M.fold 
let map {M : S} = M.map 
let filter {M : S} = M.filter 
let filter_map {M : S} = M.filter_map 
let partition {M : S} = M.partition 
let split {M : S} = M.split 
let is_empty {M : S} = M.is_empty 
let mem {M : S} = M.mem 
let equal {M : S} = M.equal 
let compare {M : S} = M.compare 
let subset {M : S} = M.subset 
let for_all {M : S} = M.for_all 
let exists {M : S} = M.exists 
let to_list {M : S} = M.to_list 
let of_list {M : S} = M.of_list 





type 'a tree = Empty | Node of 'a node and 'a node = {l : 'a tree; v : 'a; r : 'a tree; h : int}


implicit module Make {X : Imp.Data.Ord} : S with type elt = X.t and type t = X.t tree = 
  struct
    type elt = X.t
    (* type 'a tree = Empty | Node of 'a node and 'a node = {l : 'a tree; v : 'a; r : 'a tree; h : int} *)

    type t = elt tree



    let compare'' (x : elt) (y : elt) : int = match (X.compare x y) with
    | LT -> -1
    | EQ -> 0
    | GT -> 1

    (* Sets are represented by balanced binary trees (the heights of the
       children differ by at most 2 *)

    let height = function
        Empty -> 0
      | Node {h;_} -> h

    (* Creates a new node with left son l, value v and right son r.
       We must have all elements of l < v < all elements of r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. *)

    let create l v r =
      let hl = match l with Empty -> 0 | Node {h;_} -> h in
      let hr = match r with Empty -> 0 | Node {h;_} -> h in
      Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    (* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced and | height l - height r | <= 3.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. *)

    let bal l v r =
      let hl = match l with Empty -> 0 | Node {h;_} -> h in
      let hr = match r with Empty -> 0 | Node {h;_} -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Set.bal"
        | Node{l=ll; v=lv; r=lr; _} ->
            if height ll >= height lr then
              create ll lv (create lr v r)
            else begin
              match lr with
                Empty -> invalid_arg "Set.bal"
              | Node{l=lrl; v=lrv; r=lrr; _}->
                  create (create ll lv lrl) lrv (create lrr v r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Set.bal"
        | Node{l=rl; v=rv; r=rr; _} ->
            if height rr >= height rl then
              create (create l v rl) rv rr
            else begin
              match rl with
                Empty -> invalid_arg "Set.bal"
              | Node{l=rll; v=rlv; r=rlr; _} ->
                  create (create l v rll) rlv (create rlr rv rr)
            end
      end else
        Node{l; v; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    (* Insertion of one element *)

    let rec add x = function
        Empty -> Node{l=Empty; v=x; r=Empty; h=1}
      | Node{l; v; r; _} as t ->
          let c = compare'' x v in
          if c = 0 then t else
          if c < 0 then
            let ll = add x l in
            if l == ll then t else bal ll v r
          else
            let rr = add x r in
            if r == rr then t else bal l v rr

    let singleton x = Node{l=Empty; v=x; r=Empty; h=1}

    (* Beware: those two functions assume that the added v is *strictly*
       smaller (or bigger) than all the present elements in the tree; it
       does not test for equality with the current min (or max) element.
       Indeed, they are only used during the "join" operation which
       respects this precondition.
    *)

    let rec add_min_element x = function
      | Empty -> singleton x
      | Node {l; v; r; _} ->
        bal (add_min_element x l) v r

    let rec add_max_element x = function
      | Empty -> singleton x
      | Node {l; v; r; _} ->
        bal l v (add_max_element x r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v r =
      match (l, r) with
        (Empty, _) -> add_min_element v r
      | (_, Empty) -> add_max_element v l
      | (Node{l=ll; v=lv; r=lr; h=lh}, Node{l=rl; v=rv; r=rr; h=rh}) ->
          if lh > rh + 2 then bal ll lv (join lr v r) else
          if rh > lh + 2 then bal (join l v rl) rv rr else
          create l v r

    (* Smallest and greatest element of a set *)

    let rec min_elt = function
        Empty -> raise Not_found
      | Node{l=Empty; v; _} -> v
      | Node{l; _} -> min_elt l

    let rec min_elt_opt = function
        Empty -> None
      | Node{l=Empty; v; _} -> Some v
      | Node{l; _} -> min_elt_opt l

    let rec max_elt = function
        Empty -> raise Not_found
      | Node{v; r=Empty; _} -> v
      | Node{r; _} -> max_elt r

    let rec max_elt_opt = function
        Empty -> None
      | Node{v; r=Empty; _} -> Some v
      | Node{r; _} -> max_elt_opt r

    (* Remove the smallest element of the given set *)

    let rec remove_min_elt = function
        Empty -> invalid_arg "Set.remove_min_elt"
      | Node{l=Empty; r; _} -> r
      | Node{l; v; r; _} -> bal (remove_min_elt l) v r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2. *)

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> join t1 (min_elt t2) (remove_min_elt t2)

    (* Splitting.  split x s returns a triple (l, present, r) where
        - l is the set of elements of s that are < x
        - r is the set of elements of s that are > x
        - present is false if s contains no element equal to x,
          or true if s contains an element equal to x. *)

    let rec split x = function
        Empty ->
          (Empty, false, Empty)
      | Node{l; v; r; _} ->
          let c = compare'' x v in
          if c = 0 then (l, true, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
          else
            let (lr, pres, rr) = split x r in (join l v lr, pres, rr)

    (* Implementation of the set operations *)

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec mem x = function
        Empty -> false
      | Node{l; v; r; _} ->
          let c = compare'' x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec remove x = function
        Empty -> Empty
      | (Node{l; v; r; _} as t) ->
          let c = compare'' x v in
          if c = 0 then merge l r
          else
            if c < 0 then
              let ll = remove x l in
              if l == ll then t
              else bal ll v r
            else
              let rr = remove x r in
              if r == rr then t
              else bal l v rr

    let rec union s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node{l=l1; v=v1; r=r1; h=h1}, Node{l=l2; v=v2; r=r2; h=h2}) ->
          if h1 >= h2 then
            if h2 = 1 then add v2 s1 else begin
              let (l2, _, r2) = split v1 s2 in
              join (union l1 l2) v1 (union r1 r2)
            end
          else
            if h1 = 1 then add v1 s2 else begin
              let (l1, _, r1) = split v2 s1 in
              join (union l1 l2) v2 (union r1 r2)
            end

    let rec inter s1 s2 =
      match (s1, s2) with
        (Empty, _) -> Empty
      | (_, Empty) -> Empty
      | (Node{l=l1; v=v1; r=r1; _}, t2) ->
          match split v1 t2 with
            (l2, false, r2) ->
              concat (inter l1 l2) (inter r1 r2)
          | (l2, true, r2) ->
              join (inter l1 l2) v1 (inter r1 r2)

    (* Same as split, but compute the left and right subtrees
       only if the pivot element is not in the set.  The right subtree
       is computed on demand. *)

    type split_bis =
      | Found
      | NotFound of t * (unit -> t)

    let rec split_bis x = function
        Empty ->
          NotFound (Empty, (fun () -> Empty))
      | Node{l; v; r; _} ->
          let c = compare'' x v in
          if c = 0 then Found
          else if c < 0 then
            match split_bis x l with
            | Found -> Found
            | NotFound (ll, rl) -> NotFound (ll, (fun () -> join (rl ()) v r))
          else
            match split_bis x r with
            | Found -> Found
            | NotFound (lr, rr) -> NotFound (join l v lr, rr)

    let rec disjoint s1 s2 =
      match (s1, s2) with
        (Empty, _) | (_, Empty) -> true
      | (Node{l=l1; v=v1; r=r1; _}, t2) ->
          if s1 == s2 then false
          else match split_bis v1 t2 with
              NotFound(l2, r2) -> disjoint l1 l2 && disjoint r1 (r2 ())
            | Found -> false

    let rec diff s1 s2 =
      match (s1, s2) with
        (Empty, _) -> Empty
      | (t1, Empty) -> t1
      | (Node{l=l1; v=v1; r=r1; _}, t2) ->
          match split v1 t2 with
            (l2, false, r2) ->
              join (diff l1 l2) v1 (diff r1 r2)
          | (l2, true, r2) ->
              concat (diff l1 l2) (diff r1 r2)

    type enumeration = End | More of elt * t * enumeration

    let rec cons_enum s e =
      match s with
        Empty -> e
      | Node{l; v; r; _} -> cons_enum l (More(v, r, e))

    let rec compare_aux e1 e2 =
        match (e1, e2) with
        (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More(v1, r1, e1), More(v2, r2, e2)) ->
          let c = compare'' v1 v2 in
          if c <> 0
          then c
          else compare_aux (cons_enum r1 e1) (cons_enum r2 e2)

    let compare s1 s2 =
      compare_aux (cons_enum s1 End) (cons_enum s2 End)

    let equal s1 s2 =
      compare s1 s2 = 0

    let rec subset s1 s2 =
      match (s1, s2) with
        Empty, _ ->
          true
      | _, Empty ->
          false
      | Node {l=l1; v=v1; r=r1; _}, (Node {l=l2; v=v2; r=r2; _} as t2) ->
          let c = compare'' v1 v2 in
          if c = 0 then
            subset l1 l2 && subset r1 r2
          else if c < 0 then
            subset (Node {l=l1; v=v1; r=Empty; h=0}) l2 && subset r1 t2
          else
            subset (Node {l=Empty; v=v1; r=r1; h=0}) r2 && subset l1 t2

    let rec iter f = function
        Empty -> ()
      | Node{l; v; r; _} -> iter f l; f v; iter f r

    let rec fold f s accu =
      match s with
        Empty -> accu
      | Node{l; v; r; _} -> fold f r (f v (fold f l accu))

    let rec for_all p = function
        Empty -> true
      | Node{l; v; r; _} -> p v && for_all p l && for_all p r

    let rec exists p = function
        Empty -> false
      | Node{l; v; r; _} -> p v || exists p l || exists p r

    let rec filter p = function
        Empty -> Empty
      | (Node{l; v; r; _}) as t ->
          (* call [p] in the expected left-to-right order *)
          let l' = filter p l in
          let pv = p v in
          let r' = filter p r in
          if pv then
            if l==l' && r==r' then t else join l' v r'
          else concat l' r'

    let rec partition p = function
        Empty -> (Empty, Empty)
      | Node{l; v; r; _} ->
          (* call [p] in the expected left-to-right order *)
          let (lt, lf) = partition p l in
          let pv = p v in
          let (rt, rf) = partition p r in
          if pv
          then (join lt v rt, concat lf rf)
          else (concat lt rt, join lf v rf)

    let rec cardinal = function
        Empty -> 0
      | Node{l; r; _} -> cardinal l + 1 + cardinal r

    let rec elements_aux accu = function
        Empty -> accu
      | Node{l; v; r; _} -> elements_aux (v :: elements_aux accu r) l

    let elements s =
      elements_aux [] s

    let choose = min_elt

    let choose_opt = min_elt_opt

    let rec find x = function
        Empty -> raise Not_found
      | Node{l; v; r; _} ->
          let c = compare'' x v in
          if c = 0 then v
          else find x (if c < 0 then l else r)

    let rec find_first_aux v0 f = function
        Empty ->
          v0
      | Node{l; v; r; _} ->
          if f v then
            find_first_aux v f l
          else
            find_first_aux v0 f r

    let rec find_first f = function
        Empty ->
          raise Not_found
      | Node{l; v; r; _} ->
          if f v then
            find_first_aux v f l
          else
            find_first f r

    let rec find_first_opt_aux v0 f = function
        Empty ->
          Some v0
      | Node{l; v; r; _} ->
          if f v then
            find_first_opt_aux v f l
          else
            find_first_opt_aux v0 f r

    let rec find_first_opt f = function
        Empty ->
          None
      | Node{l; v; r; _} ->
          if f v then
            find_first_opt_aux v f l
          else
            find_first_opt f r

    let rec find_last_aux v0 f = function
        Empty ->
          v0
      | Node{l; v; r; _} ->
          if f v then
            find_last_aux v f r
          else
            find_last_aux v0 f l

    let rec find_last f = function
        Empty ->
          raise Not_found
      | Node{l; v; r; _} ->
          if f v then
            find_last_aux v f r
          else
            find_last f l

    let rec find_last_opt_aux v0 f = function
        Empty ->
          Some v0
      | Node{l; v; r; _} ->
          if f v then
            find_last_opt_aux v f r
          else
            find_last_opt_aux v0 f l

    let rec find_last_opt f = function
        Empty ->
          None
      | Node{l; v; r; _} ->
          if f v then
            find_last_opt_aux v f r
          else
            find_last_opt f l

    let rec find_opt x = function
        Empty -> None
      | Node{l; v; r; _} ->
          let c = compare'' x v in
          if c = 0 then Some v
          else find_opt x (if c < 0 then l else r)

    let try_join l v r =
      (* [join l v r] can only be called when (elements of l < v <
         elements of r); use [try_join l v r] when this property may
         not hold, but you hope it does hold in the common case *)
      if (l = Empty || compare'' (max_elt l) v < 0)
      && (r = Empty || compare'' v (min_elt r) < 0)
      then join l v r
      else union l (add v r)

    let rec map f = function
      | Empty -> Empty
      | Node{l; v; r; _} as t ->
         (* enforce left-to-right evaluation order *)
         let l' = map f l in
         let v' = f v in
         let r' = map f r in
         if l == l' && v == v' && r == r' then t
         else try_join l' v' r'

    let try_concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) -> try_join t1 (min_elt t2) (remove_min_elt t2)

    let rec filter_map f = function
      | Empty -> Empty
      | Node{l; v; r; _} as t ->
         (* enforce left-to-right evaluation order *)
         let l' = filter_map f l in
         let v' = f v in
         let r' = filter_map f r in
         begin match v' with
           | Some v' ->
              if l == l' && v == v' && r == r' then t
              else try_join l' v' r'
           | None ->
              try_concat l' r'
         end

    let of_sorted_list l =
      let rec sub n l =
        match n, l with
        | 0, l -> Empty, l
        | 1, x0 :: l -> Node {l=Empty; v=x0; r=Empty; h=1}, l
        | 2, x0 :: x1 :: l ->
            Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1; r=Empty; h=2}, l
        | 3, x0 :: x1 :: x2 :: l ->
            Node{l=Node{l=Empty; v=x0; r=Empty; h=1}; v=x1;
                 r=Node{l=Empty; v=x2; r=Empty; h=1}; h=2}, l
        | n, l ->
          let nl = n / 2 in
          let left, l = sub nl l in
          match l with
          | [] -> assert false
          | mid :: l ->
            let right, l = sub (n - nl - 1) l in
            create left mid right, l
      in
      fst (sub (List.length l) l)

    let to_list = elements

    let of_list l =
      match l with
      | [] -> empty
      | [x0] -> singleton x0
      | [x0; x1] -> add x1 (singleton x0)
      | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
      | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
      | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
      | _ -> of_sorted_list (List.sort_uniq compare'' l)

  end


  (*
  
  open Imp.Data
  let empty {X : Ord} = empty' {Make{X}}
  let add {X : Ord} = add' {Make{X}}

  let singleton {X : Ord} = singleton' {Make{X}}
  let remove {X : Ord} = remove' {Make{X}}
  let union {X : Ord} = union' {Make{X}}
  let inter {X : Ord} = inter' {Make{X}}
  let disjoint {X : Ord} = disjoint' {Make{X}}
  let diff {X : Ord} = diff' {Make{X}}
  let cardinal {X : Ord} = cardinal' {Make{X}}
  let elements {X : Ord} = elements' {Make{X}}
  let min_elt {X : Ord} = min_elt' {Make{X}}
  let min_elt_opt {X : Ord} = min_elt_opt' {Make{X}}
  let max_elt {X : Ord} = max_elt' {Make{X}}
  let max_elt_opt {X : Ord} = max_elt_opt' {Make{X}}
  let choose {X : Ord} = choose' {Make{X}}
  let choose_opt {X : Ord} = choose_opt' {Make{X}}
  let find {X : Ord} = find' {Make{X}}
  let find_opt {X : Ord} = find_opt' {Make{X}}
  let find_first {X : Ord} = find_first' {Make{X}}
  let find_first_opt {X : Ord} = find_first_opt' {Make{X}}
  let find_last {X : Ord} = find_last' {Make{X}}
  let find_last_opt {X : Ord} = find_last_opt' {Make{X}}
  let iter {X : Ord} = iter' {Make{X}}
  let fold {X : Ord} = fold' {Make{X}}
  let map {X : Ord} = map' {Make{X}}
  let filter {X : Ord} = filter' {Make{X}}
  let filter_map {X : Ord} = filter_map' {Make{X}}
  let partition {X : Ord} = partition' {Make{X}}
  let split {X : Ord} = split' {Make{X}}
  let is_empty {X : Ord} = is_empty' {Make{X}}
  let mem {X : Ord} = mem' {Make{X}}
  let equal {X : Ord} = equal' {Make{X}}
  let compare {X : Ord} = compare' {Make{X}}
  let subset {X : Ord} = subset' {Make{X}}
  let for_all {X : Ord} = for_all' {Make{X}}
  let exists {X : Ord} = exists' {Make{X}}
  let to_list {X : Ord} = to_list' {Make{X}}
  let of_list {X : Ord} = of_list' {Make{X}}

  *)
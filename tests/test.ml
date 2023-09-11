
open DataStructures.Mp;;
open DataStructures

open Imp.Data;;
open Imp.Control;;


let y = let z = empty {Mp.Make {Int}} in
          add 5 "hello" z;;





let () = 
begin 
  assert (mem 5 y);
  assert (not (mem 6 y));
end;;

open DataStructures.St;;


let y {O : Ord} : O.t St.tree = empty {St.Make {O}};;


let my_set = create_set {Int}

(* Example of making a generic function on sets: *)
let my_add_set {O : Ord} {Set : St.S with type elt = O.t} (x : O.t) (s : Set.t) : Set.t = St.add x s

(* let my_map  = empty {Mp.Make {Int}} *)
let my_map = create_map {Int}

let my_add_map {O : Ord} {Map : Mp.S with type key = O.t} (x : O.t) (m : int Map.t) : int Map.t = Mp.add x 10 m



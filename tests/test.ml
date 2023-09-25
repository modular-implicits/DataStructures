
open DataStructures.Mp;;
open DataStructures

open Imp.Data;;
(* open Imp.Control;; *)


let y = let z = Mp.empty {Mp.Make{Int}} in
          add 5 "hello" z;;





let () = 
begin 
  assert (mem 5 y);
  assert (not (mem 6 y));
end;;


let y {O : Ord} : O.t St.tree = St.empty {St.Make{O}};;


let my_set = St.empty {St.Make{Int}}

(* Example of making a generic function on sets: *)
let my_add_set {O : Ord} {Set : St.S with type elt = O.t} (x : O.t) (s : Set.t) : Set.t = St.add x s

(* let my_map  = empty {Mp.Make {Int}} *)
let my_map = Mp.empty {Mp.Make{Int}}

let my_add_map {O : Ord} (x : O.t) m = Mp.add x 10 m;;


let add_to_mp_map m = Mp.add 5 5 m;;
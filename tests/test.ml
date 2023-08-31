
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



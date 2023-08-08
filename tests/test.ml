
open DataStructures.Mp;;
open Imp.Data;;




let y = let z = empty {GetMap {Int}} in
          add 5 "hello" z;;


let () = 
begin 
  assert (mem 5 y);
  assert (not (mem 6 y));
end 
  


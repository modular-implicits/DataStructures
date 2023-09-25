Implicit versions of commonly used OCaml data structures.

To use this library, you must have a type implementing the `Ord` signature from the `imp` library:

Using the Set data structure:
```ocaml
(* Creating an empty Set data structure *)
let my_set = St.empty {St.Make{Int}}

(* Example of making a generic function on sets: *)
let my_add_set {O : Ord} {Set : St.S with type elt = O.t} (x : O.t) (s : Set.t) : Set.t = St.add x s

```

Using the Map data structure:
```ocaml
let my_map = Mp.empty {Mp.Make{Int}}


let my_add_map {O : Ord} {Map : Mp.S with type key = O.t} (x : O.t) (m : int Map.t) : int Map.t = Mp.add x 10 m
```
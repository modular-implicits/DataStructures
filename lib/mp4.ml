open Imp.Data;;
open Map;;

module type Mp =
  sig
    type key
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  end

let empty {M : Mp} = M.empty
let is_empty {M : Mp} = M.is_empty
let mem {M : Mp} = M.mem
let add {M : Mp} = M.add
let singleton {M : Mp} = M.singleton
let remove {M : Mp} = M.remove
let merge {M : Mp} = M.merge
let compare {M : Mp} = M.compare
let equal {M : Mp} = M.equal
let iter {M : Mp} = M.iter
let fold {M : Mp} = M.fold
let for_all {M : Mp} = M.for_all
let exists {M : Mp} = M.exists
let filter {M : Mp} = M.filter
let partition {M : Mp} = M.partition
let cardinal {M : Mp} = M.cardinal
let bindings {M : Mp} = M.bindings
let min_binding {M : Mp} = M.min_binding
let max_binding {M : Mp} = M.max_binding
let choose {M : Mp} = M.choose
let split {M : Mp} = M.split
let find {M : Mp} = M.find
let map {M : Mp} = M.map
let mapi {M : Mp} = M.mapi



(* Resolver sucks, don't know how to extract the MyMapDef out to the type of Mp??*)

implicit module GetMap {X : Ord} : Mp with type key = X.t (* and type 'a t = 'a MyMapDef.t *) = struct 
  
  module MyMapDef = Map.Make(struct 
  type t = X.t
  let compare = X.compare
  end);;

  type key = X.t
  type 'a t = 'a MyMapDef.t

  let empty = MyMapDef.empty
  let is_empty = MyMapDef.is_empty
  let mem = MyMapDef.mem
  let add = MyMapDef.add
  let singleton = MyMapDef.singleton
  let remove = MyMapDef.remove
  let merge = MyMapDef.merge
  let compare = MyMapDef.compare
  let equal = MyMapDef.equal
  let iter = MyMapDef.iter
  let fold = MyMapDef.fold
  let for_all = MyMapDef.for_all
  let exists = MyMapDef.exists
  let filter = MyMapDef.filter
  let partition = MyMapDef.partition
  let cardinal = MyMapDef.cardinal
  let bindings = MyMapDef.bindings
  let min_binding = MyMapDef.min_binding
  let max_binding = MyMapDef.max_binding
  let choose = MyMapDef.choose
  let split = MyMapDef.split
  let find = MyMapDef.find
  let map = MyMapDef.map
  let mapi = MyMapDef.mapi
end;;
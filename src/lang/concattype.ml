
type 'funtype t = 
  | Atomic of 'funtype
  | Concat of 'funtype t * 'funtype t

(**
 Will short circuit
*)
let frozen_flatmap (x : 'funtype t) ~(f : 'funtype -> 'a) ~(join : (unit -> 'a) -> (unit -> 'a) -> (unit -> 'a)) : 'a =
  let rec helper x ~f ~join =
    match x with
    | Atomic funtype -> fun _ ->  (f funtype)
    | Concat (f_1, f_2) -> join (helper f_1 ~f ~join) (helper f_2 ~f ~join)
  in (helper x ~f ~join) ()

let rec flatmap (x : 'funtype t) ~(f : 'funtype -> 'b) ~(join : 'b -> 'b -> 'b) : 'b =
  match x with
  | Atomic typ -> f typ
  | Concat (typ_a, typ_b) -> join (flatmap typ_a ~f ~join) (flatmap typ_b ~f ~join)

let any (x : 'funtype t) ~(pred : 'funtype -> bool) : bool = 
  flatmap x ~f:pred ~join:(||)

let to_list (x : 'funtype t) : 'funtype list =
  flatmap x ~f:List.singleton ~join:(@)
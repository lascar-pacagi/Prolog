module CmpInt =
 struct
  type t = int
  let compare = Stdlib.compare
 end

module IntMap = Map.Make(CmpInt)

type env = Type.term IntMap.t

let empty () = IntMap.empty

let add env id v = IntMap.add id v env

let add_all env ids vals = List.fold_left2 (fun res id v -> IntMap.add id v res) env ids vals

let create l = List.fold_left (fun res (id, v) -> IntMap.add id v res) IntMap.empty l

let find env id = IntMap.find id env

let defined env id = IntMap.mem id env

let map env f = IntMap.map f env

let equal env1 env2 = IntMap.equal (=) env1 env2

let iter env f1 f2 =
  IntMap.iter
    (fun id t ->
      f1 id;
      f2 t;
      Printf.printf "\n")
    env

let filter env f = IntMap.filter (fun x _ -> f x) env

let size = IntMap.cardinal

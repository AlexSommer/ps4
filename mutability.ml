(*  precondition: n and k are ints
    postcondition: returns a function f with the following properties:
    f returns n+(i-1)*k where i is the ith time the function has
    been called. *)
let count_up_from n k =
  let iter = ref 0 in
  (fun () -> iter:=!iter+1; n + (!iter-1)*k)

(*   precondition: take a function : (int -> 'a) and an int >= 0 representing the array
      length 
      postcondition: returns an array of length n where each index i has the value of 
      (f i) *)
let tabulate f n =
  let result = Array.make n (f 0) in
  for id=0 to (n-1) do 
    result.(id) <- (f id)
  done;
  result

let fold_left_imp f acc xs =
  let build = ref acc in
  let lst = ref xs in
  while (!lst <> []) do
    match !lst with
    | [] -> ()
    | hd::tl ->
        lst:=tl;
        build:= (f !build hd);
  done;
  !build


type t = int list  (* TODO: change unit to whatever you want *)
type u = unit  (* TODO: change unit to whatever you want *)
let lst : t list = 
let zardoz (x:t) : u = failwith "TODO"


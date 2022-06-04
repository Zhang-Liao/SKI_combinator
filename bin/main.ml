type cl = 
  | S        
  | K
  | I
  | App of cl * cl
  | Var of string
  (* | List of cl list *)

let root_reduce term = 
  match term with
  | App (App (App (S, u) , v), w) -> App (App(u, w), App(v, w)), true
  | App (App (K, u), _) -> u, true
  | App (I, t) -> t, true
  | _ -> term, false

let rec left_out_reduce term =
  let term', reduced = root_reduce term in
  if reduced then term', true else 
    match term with
    | App (s, t) -> (
        match left_out_reduce s with
        | s', true -> App (s', t), true
        | _ -> (
          match left_out_reduce t with
          | t', true -> App (s, t'), true
          | _ -> term, false
      ))
    | _ -> term, false

let rec cl_str t = 
  match t with
  | S -> "S"
  | K -> "K"
  | I -> "I"
  | App (t1, t2) -> "("^cl_str t1^cl_str t2^")"
  | Var x -> x
  (* | List ts -> "("^ List.fold_left (fun acc t' -> acc ^ cl_str t') "" ts ^")" *)

let print_cl t = print_endline@@cl_str t

let left_out_reduces_to t1 t2 =
  let rec aux t = 
    match left_out_reduce t with
    | t',  true -> print_cl t'; if t' = t2 then print_endline "success" else aux t'
    | t', false -> print_cl t'; if t' = t2 then print_endline "success" else print_endline "fail"
  in
  print_cl t1; aux t1

let b = App(App(S, (App (K, S))), K)
let c = App(App(S, (App (App (b, b), S))), (App (K, K)))
let y = App(App (App (b, (App (S, I))), (App(App(S, I), I))), (App(App(b, (App(S, I))),(App(App(S, I), I)))))
(* let _ = left_out_reduces_to (App(App(App(b, Var "x"), Var "y"), Var "z")) (App(Var "x", App(Var "y", Var "z"))) *)
(* let _ = left_out_reduces_to (App(App(App(c, Var "x"), Var "y"), Var "z")) (App(App(Var "x", Var "z"), Var "y")) *)
let _ = left_out_reduces_to (App(y, Var "x")) (App(Var "x", App(y ,Var "x"))) 
(* let _ = left_out_reduces_to (App((App (S, b)), App (K ,I))) (App(K, I)) *)
(* let _ = left_out_reduces_to 
  (App (App (App (App (App (App (App (S, S), S), S), S), S), S), S))
  (App (App(S, S), App(App(S, S), App(App(S, S), App(S, S))))) *)


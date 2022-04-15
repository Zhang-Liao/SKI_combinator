let _ =
  let str = Marshal.to_string "((TacAlias Coq.ssr.ssreflect.by_#_2AAA68AE ((TacML (CAst (Reference (CAst $1))))) (CAst (TacGeneric (TacAlias Coq.ssr.ssreflect.move_#_#_2AAA68AA ((TacML (CAst (Reference (CAst $1)) (Reference (CAst $2))))) (CAst (TacGeneric (CAst (CRef (CAst contra))) notb_notc (CAst (CRef (CAst negbT)))) (TacGeneric)))))))	(State (Goal (Prod _ Relevant (Prod _ Relevant (App (Const Coq.Init.Datatypes.is_true) (Var c)) (App (Const Coq.Init.Datatypes.is_true) (Var b))) (Prod _ Relevant (App (Ind Coq.Init.Logic.eq) (Ind Coq.Init.Datatypes.bool) (Var b) (Construct Coq.Init.Datatypes.false Coq.Init.Datatypes.bool)) (App (Const Coq.Init.Datatypes.is_true) (App (Const Coq.Init.Datatypes.negb) (Var c)))))) (Hypotheses ((b (Ind Coq.Init.Datatypes.bool)) (c (Ind Coq.Init.Datatypes.bool)))))" [Marshal.No_sharing] in 
  print_int@@Hashtbl.hash_param 255 255 str; print_newline ()
  (* print_string str *)


let _ = 
  let ic = open_in_bin "/home/zhang/test_OCaml/byte.txt" in
  let n = in_channel_length(ic) in
  let b = Buffer.create(n) in
  let _= Buffer.add_channel(b)(ic)(n) in
  let _= close_in ic in
  let line = Buffer.contents b in
  print_endline line;
  print_int@@Hashtbl.hash_param 255 255 line; print_newline () 

let _ = 
  let ic = open_in_bin "/home/zhang/test_OCaml/8604636_byte.txt" in
  let n = in_channel_length(ic) in
  let b = Buffer.create(n) in
  let _= Buffer.add_channel(b)(ic)(n) in
  let _= close_in ic in
  let line = Buffer.contents b in
  print_endline line;
  print_int@@Hashtbl.hash_param 255 255 line; print_newline () 

let _ = 
  let ic = open_in_bin "/home/zhang/test_OCaml/100893355_byte.txt" in
  let n = in_channel_length(ic) in
  let b = Buffer.create(n) in
  let _= Buffer.add_channel(b)(ic)(n) in
  let _= close_in ic in
  let line = Buffer.contents b in
  print_int@@Hashtbl.hash_param 255 255 line; print_newline () 




(* let _ =
  let ic = open_in "/home/zhang/test_OCaml/8604636_byte.txt" in
  let line = input_line ic in 
  print_int@@Hashtbl.hash_param 255 255 line; print_newline () *)

open Printf

type cl = 
  | S        
  | K
  | I
  | App of cl * cl

let b_comb = App(App(S, (App (K, S))), K)
let c_comb = App(App(S, (App (App (b_comb, b_comb), S))), (App (K, K)))
let test = App (App (S, K), K)

type cl_type = 
  | Arrow of cl_type * cl_type
  | Type of string

let mk_type kind i name = 
  Type (kind^"_"^Stdlib.string_of_int i^"_"^name)

let rec mk_constraint t ty i acc =
  match t with
  | I -> 
    let a = mk_type "i" i "a" in
    (ty, Arrow (a, a))::acc, i+1
  | K -> 
    let a1 = mk_type "k" i "a1" in
    let a2 = mk_type "k" i "a2" in
    (ty, Arrow (a1, (Arrow (a2, a1))))::acc, i+1
  | S -> 
    let a1 = mk_type "s" i "a1" in  
    let a2 = mk_type "s" i "a2" in  
    let a3 = mk_type "s" i "a3" in  
    (* (ty, Arrow (Arrow (Arrow (Arrow (a1, Arrow (a2, a3)), Arrow (a1, a2)), a1), a3))::acc, *)
    (ty, Arrow (Arrow (a1, Arrow (a2, a3)), Arrow (Arrow(a1, a2), Arrow (a1, a3))))::acc,
    i+1
  | App (t1, t2) ->  
    let a1 = mk_type "app" i "a1" in
    let a2 = mk_type "app" i "a2" in
    let acc = (ty, a2)::acc in
    let acc, i = mk_constraint t1 (Arrow (a1, a2)) (i+1) acc in 
    mk_constraint t2 a1 i acc 

let rec cl_type2str = function 
  | Type s -> s 
  | Arrow (t1, t2) -> "(" ^ cl_type2str t1 ^ " -> " ^ cl_type2str t2 ^")"

let rec print_constraint c = 
  match c with
  | [] -> ()
  | (t1, t2)::tl -> printf "%s = %s\n" (cl_type2str t1) (cl_type2str t2); print_constraint tl


(* let rec print_subst s = 
  match c with
  | [] -> ()
  | (t1, t2)::tl -> printf "%s = %s\n" (cl_type2str t1) (cl_type2str t2); print_constraint tl *)

let rec not_fv var typ =
  match typ with
  | Type _ -> if var = typ then false else true
  | Arrow (a1, a2) -> not_fv var a1 &&  not_fv var a2


let rec subst_typ t a b =
  match t with
  | Type _ -> if t = a then b else t
  | Arrow (t1, t2) -> Arrow (subst_typ t1 a b, subst_typ t2 a b)

let rec subst_constraint c a b =
  match c with
  | (t1, t2) :: tl -> (subst_typ t1 a b, subst_typ t2 a b)::subst_constraint tl a b  
  | [] -> []

let rec unify constraints subst = 
  match constraints with 
  | (t1, t2)::tl -> (
      if t1 = t2 then unify tl subst else 
        match t1, t2 with
        | Type _, _ when not_fv t1 t2  -> unify (subst_constraint tl t1 t2) ((t1, t2)::subst)
        | _, Type _ when not_fv t2 t1 -> unify (subst_constraint tl t2 t1) ((t2, t1)::subst)
        | Arrow (ty1, ty2), Arrow(ty1', ty2') -> unify ((ty1, ty1')::(ty2, ty2')::tl) (subst)
        | _ -> failwith "cannot unify"
    )
  | [] -> subst


let _ = 
  let c, _ = mk_constraint c_comb (mk_type "root" 0 "") 1 [] in
  let _ = print_endline "constraint"; print_constraint c in
  let substs = unify c [] in
  print_endline "\nsubstution"; print_constraint substs 
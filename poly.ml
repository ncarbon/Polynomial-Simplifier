(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int (*
      First int is the constant
      Second int is the power of x 
      10  -> Term(10,0)
      2x -> Term(2,1)
      3x^20 -> Term(3, 20)
    *)
  | Plus of pExp list
  (*
    List of terms added
    Plus([Term(2,1); Term(1,0)])
  *)
  | Times of pExp list (* List of terms multiplied *)
  | Expo of pExp*int

(*
  Function to traslate betwen AST expressions
  to pExp expressions
*)
let rec from_expr (_e: Expr.expr) : pExp =
    match _e with
    | Num i -> Term(i, 0)
    | Var x -> Term(1, 1)
    | Neg ex -> Times [Term(-1, 0); (from_expr ex)]
    | Sub (expr1, expr2) -> Plus [(from_expr expr1); (Times[Term(-1, 0); (from_expr expr2)])]
    | Add (expr1, expr2) -> Plus [(from_expr expr1); (from_expr expr2)]
    | Mul (expr1, expr2) -> Times [(from_expr expr1); (from_expr expr2)]
    | Pow (expr, i) ->  Expo((from_expr expr), i) 
    | _ -> Term(0, 0)

(* 
  Compute degree of a polynomial expression.

  Hint 1: Degree of Term(n,m) is m
  Hint 2: Degree of Plus[...] is the max of the degree of args
  Hint 3: Degree of Times[...] is the sum of the degree of args 
*)
let degree (_e:pExp): int = 0 (* TODO *)

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
let compare (e1: pExp) (e2: pExp) : bool =
  degree e1 > degree e2

(* Print a pExpr nicely 
  Term(3,0) -> 3
  Term(5,1) -> 5x 
  Term(4,2) -> 4x^2
  Plus... -> () + () 
  Times ... -> ()() .. ()

  Hint 1: Print () around elements that are not Term() 
  Hint 2: Recurse on the elements of Plus[..] or Times[..]
*)

let match_exp_print(const: int)(exp: int): unit =
  match exp with
      | 0 -> Printf.printf "%d" const 
      | 1 -> Printf.printf "%dx" const
      | _ -> Printf.printf "%d^%d" const exp

let rec print_term(const: int)(exp: int): unit =
  match const with
  | 0 -> Printf.printf "%d" 0
  | 1 -> (match exp with
        | 0 -> Printf.printf "%d" const
        | 1 -> Printf.printf "x"
        | _ -> Printf.printf "x^%d" exp)
  | _ -> (match exp with
        | 0 -> Printf.printf "%d" const 
        | 1 -> Printf.printf "%dx" const 
        | _ -> Printf.printf "%d^%d" const exp)
and
  print_op_list (lst: pExp list) (op: string) =
    match lst with
    | [] -> Printf.printf ""
    | [x] -> print_pExp x;
    | h::t -> print_pExp h; 
              Printf.printf "%s" op;
              print_op_list t op;
and
  print_pExp (_e: pExp): unit =
    match _e with
    | Term(c,v) -> print_term c v 
    | Times(lst) -> Printf.printf "(";  print_op_list lst "*"; Printf.printf ")";
    | Expo(e, i) -> print_pExp e; Printf.printf "^"; Printf.printf "%d" i;
    | Plus(lst) -> Printf.printf "(";  print_op_list lst "+"; Printf.printf ")";
    (* | Minus(lst) -> Printf.printf "(";  print_op_list lst "-"; Printf.printf ")"; *)
    | _ -> Printf.printf("Not implemented")

(* 
  Function to simplify (one pass) pExpr

  n1 x^m1 * n2 x^m2 -> n1*n2 x^(m1+m2)
  Term(n1,m1)*Term(n2,m2) -> Term(n1*n2,m1+m2)

  Hint 1: Keep terms in Plus[...] sorted
  Hint 2: flatten plus, i.e. Plus[ Plus[..], ..] => Plus[..]
  Hint 3: flatten times, i.e. times of times is times
  Hint 4: Accumulate terms. Term(n1,m)+Term(n2,m) => Term(n1+n2,m)
          Term(n1, m1)*Term(n2,m2) => Term(n1*n2, m1+m2)
  Hint 5: Use distributivity, i.e. Times[Plus[..],] => Plus[Times[..],]
    i.e. Times[Plus[Term(1,1); Term(2,2)]; Term(3,3)] 
      => Plus[Times[Term(1,1); Term(3,3)]; Times[Term(2,2); Term(3,3)]]
      => Plus[Term(2,3); Term(6,5)]
  Hint 6: Find other situations that can arise
*)

(* let rec flatten (pLst: pExp list): pExp list = 
  match pLst with
  | Term (c, x) -> [Term(c, x)]
  | Plus(lst) -> List.concat(List.map flatten lst) *)

let flatten list =
  let rec aux acc = function
  | [] -> acc
  | Term (c, x) :: t -> aux(Term(c, x)::acc) t
  | Times lst :: t -> aux(Times lst ::acc) t
  | Expo (c, x) :: t -> aux(Expo(c, x)::acc) t
  | Plus lst :: t -> aux(aux acc lst) t in
        List.rev(aux [] list)


let simplify1 (e:pExp): pExp =
    (* flatten plus *)
    match e with
    | Plus pExpLst -> Plus (flatten pExpLst)
    | _ -> e


(* compute if two pExpr lists are the same *)
let rec equal_pExpLst (l1) (l2): bool =
  match l1, l2 with
  | [], [] -> true
  | [], _  -> false
  | _, [] -> false
  | h1::t1, h2::t2 -> h1 = h2 && equal_pExpLst t1 t2

(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  match _e1, _e2 with
    | (Plus lst1), (Plus lst2) -> equal_pExpLst lst1 lst2
    | (Times lst1), (Times lst2) -> equal_pExpLst lst1 lst2
    | (Term (c1, v1)), (Term (c2,v2)) -> c1 = c2 && v1 = v2
    | (Expo (e1, i1)), (Expo (e2, i2)) ->  e1 = e2 && i1 = i2
    | _ -> false

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
(* let rec simplify (e:pExp): pExp =
    let rE = simplify1(e) in
      print_pExp rE;
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE) *)
let rec simplify (e:pExp): pExp =
  let rE = simplify1(e) in
      print_pExp rE;
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)




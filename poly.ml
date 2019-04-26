(* Sum type to encode efficiently polynomial expressions *)
type pExp =
  | Term of int*int 
  | Plus of pExp list
  | Times of pExp list
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
    | Sub (expr1, expr2) -> Plus ([(from_expr expr1); (Times[Term(-1, 0); (from_expr expr2)])])
    | Add (expr1, expr2) -> Plus ([(from_expr expr1); (from_expr expr2)])
    | Mul (expr1, expr2) -> Times ([(from_expr expr1); (from_expr expr2)])
    | Pow (expr, i) ->  (match from_expr expr with
                          Term(c, x) -> Term(c, i) (* convert Pow(var(x), i) -> Term(1, i) *)
                          | _ -> Expo((from_expr expr), i) )
    | _ -> Term(0, 0)

let rec maxDegreePlus (pLst: pExp list): int =
  match pLst with
    | [] -> 0
    | [Term(n, m)] -> m
    | Term(n, m)::t -> max m (maxDegreePlus t)
    | [Expo(exp, i)] -> i
    | Expo(exp, i)::t -> max i (maxDegreePlus t)
    | [Times (lst)] -> degree (Times lst)
    | Times (lst)::t -> max (degree (Times lst)) (maxDegreePlus t)
    | _ -> 0

and 
sumDegreeTimes (pLst: pExp list) (sum: int): int =
  match pLst with
    | [] -> sum
    | [Term(n, m)] -> sum + m
    | Term(n, m)::Term(c, v)::t -> sumDegreeTimes t (m+v)
    | _ -> sum
(* 
  Compute degree of a polynomial expression.
*)
and 
degree (_e:pExp): int = 
  match _e with
    | Term(n, m) -> m
    | Plus (lst) -> maxDegreePlus lst
    | Times(lst) -> sumDegreeTimes lst 0
    | Expo(e, i) -> i

(* 
  Comparison function useful for sorting of Plus[..] args 
  to "normalize them". This way, terms that need to be reduced
  show up one after another.
  *)
let compare (e1: pExp) (e2: pExp) : int =
  if degree e1 > degree e2 then 1
  else if degree e1 < degree e2 then -1
  else 0

let append l1 l2 =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
    in
    loop [] l1 l2

(* Expands an expression exp raised to some power n
by transforming it to a Times with n number of exp's in the list*)

let expandExpo (exp: pExp) (n: int): pExp list = 
  let rec helper (exp1: pExp) (n1: int) (l1: pExp list): pExp list =
    if n1 = 0 then l1 else helper (exp1)(n1-1)(exp1::l1)
    in List.rev(helper exp n [])


let match_exp_print(const: int)(exp: int): unit =
  match exp with
      | 0 -> Printf.printf "%d" const 
      | 1 -> Printf.printf "%dx" const
      | _ -> Printf.printf "%d^%d" const exp

let rec print_term(const: int)(exp: int): unit =
  match const with
  | 0 -> Printf.printf "" 
  | 1 -> (match exp with
        | 0 -> Printf.printf "%d" const
        | 1 -> Printf.printf "x"
        | _ -> Printf.printf "x^%d" exp)
  | _ -> (match exp with
        | 0 -> Printf.printf "%d" const 
        | 1 -> Printf.printf "%dx" const 
        | _ -> Printf.printf "%dx^%d" const exp)
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
    | Term(c,v) ->  print_term c v; 
    | Times(lst) -> Printf.printf "(";  print_op_list lst "*"; Printf.printf ")";
    | Expo(e, i) -> print_pExp e; Printf.printf "^"; Printf.printf "%d" i;
    | Plus(lst) -> Printf.printf "(";  print_op_list lst "+"; Printf.printf ")";
    | _ -> Printf.printf("Not implemented")

(* 
  Function to simplify (one pass) pExpr
*)

let rec simplify1 (e:pExp): pExp =
    match e with
    | Expo (exp, i) -> Times(expandExpo exp i)
    | Times pExpLst -> let flatLst = flattenTimes pExpLst in
                          (match (List.rev(evalTimes(flatLst))) with
                            | [Term(u, v)] -> Term(u, v)
                            | h::t -> Plus(h::t)
                            | _ -> Plus [])
    | Plus pExpLst -> let flatLst = flattenPlus pExpLst in
                        let sortedLst = List.sort compare flatLst in
                          Plus(List.rev(addTerms sortedLst))
    | _ -> e
and
  flattenPlus list =
    let rec aux acc = function
    | [] -> acc
    | Term (c, x) :: t -> aux(Term(c, x)::acc) t
    | Times lst :: t -> aux(Times lst::acc) t
    | Expo (c, x) :: t -> aux(simplify1 (Expo(c, x))::acc) t
    | Plus lst :: t -> aux(aux acc lst) t in
          List.rev(aux [] list)
and
  flattenTimes list =
    let rec aux acc = function
    | [] -> acc
    | Term (c, x) :: t -> aux(Term(c, x)::acc) t
    | Plus lst :: t -> aux(simplify1 (Plus lst)::acc) t
    | Expo (c, x) :: t -> aux(simplify1 (Expo(c, x))::acc) t
    | Times lst :: t -> aux(aux acc lst) t in
          List.rev(aux [] list)
and
 addTerms lst = 
  let rec aux acc = function
    | [] -> acc
    | [Term(n, m)] -> Term(n, m)::acc
    | Term(n, m)::Term(c, v)::t -> 
      if m = v then aux (Term(n+c, m)::acc) t else aux (Term(n, m)::acc) (Term(c, v)::t)
    | Term(n, m)::t -> aux (Term(n, m)::acc) t
    | [Times(l)] -> (match (simplify1 (Times(l))) with (* simplify times so there's only terms and recurse*)
                        | Times l1 -> (append l1 acc)
                        | Plus l1 -> (append l1 acc)
                        | x -> x::acc)
    | Times(l)::t -> (match (simplify1 (Times(l))) with 
                        | Plus l1 -> aux (append l1 acc) t
                        | x -> aux (x::acc) t)
    | [Plus(l)] -> (match (simplify1 (Plus(l))) with
                      | Times l1 -> aux acc l1
                      | Plus l1 -> aux acc l1
                      | x -> x::acc)
    | Plus(l)::t -> (match (simplify1 (Plus(l))) with
                      | Times l1 -> aux acc (append l1 t)
                      | Plus l1 -> aux (append l1 acc) t
                      | x -> aux (x::acc) t)
    | x::t -> x::acc
    in List.rev(aux [] lst)
and
  evalTimes lst =
  let rec aux acc = function
    | [] -> acc
    | [Term(n, m)] -> Term(n, m)::acc
    | Term (n, m)::Term(u, v)::[] -> Term(n*u, m+v)::acc
    | Term(n, m)::Term(u, v)::t -> aux ((Term(n*u, m+v))::acc) t
    | Term(n, m)::Plus l::t -> aux ((append ((List.fold_left (fun acc1 a -> Times(a::Term(n, m)::[])::acc1) [] l)) (acc))) t
    | [Plus l] -> [Times[Plus(acc);Plus l]]
    | Plus l::t -> append((List.fold_left (fun acc1 a -> Times(a::t)::acc1) [] l))(acc)
    | _ -> acc
      in
        aux [] lst
  
(* 
  Compute if two pExp are the same 
  Make sure this code works before you work on simplify1  
*)
let equal_pExp (_e1: pExp) (_e2: pExp) :bool =
  if _e1 = _e2 then true else false
 

(* Fixed point version of simplify1 
  i.e. Apply simplify1 until no 
  progress is made
*)    
let rec simplify (e:pExp): pExp =
  let rE = simplify1(e) in
      print_pExp rE;
      print_endline "";
      if (equal_pExp e rE) then
        e
      else  
        simplify(rE)
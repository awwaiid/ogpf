
(* Define some primitives *)

open Lambda

let succ = function
  | Num(n) -> Num(n+1)
  | _ as ex -> ex

let randnum = function
  | Num(n) -> Num(Random.int n)
  | _ as ex -> ex

let rec churchize = function
  | 0 -> Apply(Var("",1),Var("",0))
  | _ as n -> Apply(Var("",1),churchize (n-1))

let to_church = function
  | Num(n) -> Lambda("",Lambda("",churchize n))
  | _ as ex -> ex

let depth ex = Num(depth ex - 1)

let mul2 a b =
  match b with
  | Num(c) -> Num(a * c)
  | _ -> b

let mul = function
  | Num(n) -> Prim("MUL2",mul2 n)
  | _ as ex -> ex

let add2 a b =
  match b with
  | Num(c) -> Num(a + c)
  | _ -> b

let add = function
  | Num(n) -> Prim("ADD2",add2 n)
  | _ as ex -> ex

let div2 a b =
  match b with
  | Num(c) -> Num(a / c)
  | _ -> b

let div = function
  | Num(n) -> Prim("DIV2",div2 n)
  | _ as ex -> ex

let sub2 a b =
  match b with
  | Num(c) -> Num(a - c)
  | _ -> b

let sub = function
  | Num(n) -> Prim("SUB2",sub2 n)
  | _ as ex -> ex

let eq2 a b =
  match b with
  | Num(c) -> if(a = c) then
      Lambda("",Lambda("",Var("",1)))
    else
      Lambda("",Lambda("",Var("",0)))
  | _ -> b

let eq = function
  | Num(n) -> Prim("EQ2",eq2 n)
  | _ as ex -> ex

let primpair2 a b = make_pair a b

let primpair a = Prim("PRIMPAIR2",primpair2 a)

let primhd a = fst (get_pair a)

let primtl a = snd (get_pair a)

(* Install the primitives *)

let install () =
  Lambda.add_func "succ" succ;
  Lambda.add_func "rand" randnum;
  Lambda.add_func "to_church" to_church;
  Lambda.add_func "depth" depth;
  Lambda.add_func "mul" mul;
  Lambda.add_func "add" add;
  Lambda.add_func "div" div;
  Lambda.add_func "sub" sub;
  Lambda.add_func "eq" eq;
  Lambda.add_func "primpair" primpair;
  Lambda.add_func "primhd" primhd;
  Lambda.add_func "primtl" primtl;
  Lambda.add_var "id" (Lambda.Lambda("",(Lambda.Var("",0))))



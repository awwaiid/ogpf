
module Make ( Genotype: Fgenotype.Sig ) = struct

  type t = Genotype.t
  open Genotype
  exception Error of string

  let rec eval x = function
    | Terminal(t) -> begin
      match t with
      | "1" -> 1.0
      | "x" -> x
      | _ -> raise (Error "invalid genome!")
      end
    | NonTerminal(v,l) ->
      match v with
      | "+" ->
        let (a, b) = (List.nth l 0, List.nth l 1) in
        (eval x a) +. (eval x b)
      | "-" ->
        let (a, b) = (List.nth l 0, List.nth l 1) in
        (eval x a) -. (eval x b)
      | "*" ->
        let (a, b) = (List.nth l 0, List.nth l 1) in
        (eval x a) *. (eval x b)
      | "%" ->
        let (a, b) = (List.nth l 0, List.nth l 1) in
        let b = eval x b in
        if b = 0.0 then 0.0 else
          (eval x a) /. b
        | "sin" ->
        let a = List.nth l 0 in
        sin (eval x a)
      | _ -> raise (Error "invalid genome!")


  let getFitness (g:t) =
    (* print_string "Evaluating...\n";
    Genotype.print g;
    print_newline(); *)
    let total = ref 0 in
    let pi = 4.0 *. atan 1.0 in
    let n = 100 in
    for i = 0 to n do
      let x = Random.float (2.0 *. pi) in
      (* let target = cos (2.0 *. x) in *)
      let target = cos x in
      let result = eval x g in
      let diff = abs_float (target -. result) in
      let hit = (diff <= 0.001) in
      if hit then begin
        (*print_string "HIT!";
        print_string "  x: \t";
        print_float x;
        print_string "\n  target: \t";
        print_float target;
        print_string "\n  result: \t";
        print_float result;
        print_string "\n  diff: \t";
        print_float diff;
        print_newline(); *)
        total := !total + 1
      end
    done;
    (n - !total)
    (* print_string "TOTAL: ";
    print_int !total;
    print_string "\t";
    print_int (truncate !total);
    print_newline();
    print_newline();

    truncate !total *)

end


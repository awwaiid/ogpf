
module Make ( Genotype: Lgenotype.Sig ) = struct

  type t = Genotype.t
  open Genotype
  exception Error of string

  let rec fact n =
    if n < 2 then 1 else n * (fact (n - 1))

  let getFitness (g:t) =
    let n = 1 in
    print_string "Evaluating...\n";
    Genotype.print g;
    print_newline();
    let total = ref 0 in
    for i = 0 to n do
      (* let x = Random.int 13 in *)
      let x = 5 in
      let target = fact x in 
      let result = Genotype.eval x g in
      let diff = abs (target - result) in
      total := !total + diff;
    done;
    let subtotal = !total in
    subtotal + String.length (Genotype.to_string g)

    (* print_string "TOTAL: ";
    print_int !total;
    print_string "\t";
    print_int (truncate !total);
    print_newline();
    print_newline();
    truncate !total *)

end



module Make ( Genotype: Fgenotype.Sig ) = struct

  type t = Genotype.t
  open Genotype
  exception Error of string

  let getFitness (g:t) =
    (* print_string "Evaluating...\n";
    Genotype.print g;
    print_newline(); *)
    let total = ref 0.0 in
    let pi = 4.0 *. atan 1.0 in
    let n = 100 in
    for i = 0 to n do
      let x = Random.float (2.0 *. pi) in
      let target = cos (2.0 *. x) in 
      (*let target = (x *. 3.0) +. x in *)
      let result = Genotype.eval x g in
      let diff = abs_float (target -. result) in
      total := !total +. diff;
    done;
    let subtotal = int_of_float (!total *. 100.0) in
    subtotal + String.length (Genotype.to_string g)
    (* print_string "TOTAL: ";
    print_int !total;
    print_string "\t";
    print_int (truncate !total);
    print_newline();
    print_newline();

    truncate !total *)

end


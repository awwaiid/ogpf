
(* pop.ml - Population of genomes *)

(* Holds the population.
 * 
 * This is a container for a bunch of genomes. It allows you to pull members
 * from the population and add them back in. Stuff like that.
 *
 * Maybe it will also keep history of the current members... I'm not sure if
 * that is appropriate for this location or not.
 *)

(* Nice little exception *)
exception Error of string

(* Here we actually create a population of the given genotype *)
module Make ( Genotype: Genotype.Sig ) = struct

  (* For now we'll just use a list *)
  type m = Genotype.t
  type 'a t = 'a GrabBag.t
  (*type t = Genotype.t GrabBag.t
  and m = Genotype.t *)

  (* Return an empty population *)
  let empty = GrabBag.empty

  (* Generate a new population from random genomes *)
  let rec populate size : 'a t =
(*    if (size == 42) then GrabBag.add (populate (size - 1)) (Genotype.of_string "Roses") else *)
    if (size == 0) then empty
    else GrabBag.add (populate (size - 1)) (Genotype.randInstance 5)

  (* Calculate the size of the population *)
  let size pop = GrabBag.size pop

(*  let m_to_raw (x:m) : Genotype.t = x
  let m_of_raw (x: Genotype.t) : m = x *)

  (* Grab a random member of the population *)
  let pull_rand_member pop =
    GrabBag.grab pop

  let add_member pop member =
    GrabBag.add pop member
 
  (* Auxillary printing function. Prints a numbered list of all the genomes *)
  let rec print_aux pop n =
    if pop == GrabBag.empty then begin
      print_string "END";
      print_newline();
      (n, 0)
    end else begin
      let h, pop = GrabBag.grab pop in
      print_int n;
      print_string ": \"";
      print_string (Genotype.to_string h);
      print_string "\"\t";
      (*let h_val = Genotype.evaluate h in
      print_int h_val; *)
      print_newline();
      let total, sum = print_aux pop (n + 1) in
      (* (total, sum + h_val) *)
      (total, sum)
    end

  (* Print all the genomes *)
  let print g =
    let total, sum = print_aux g 0 in
    print_string "Total: ";
    print_int total;
    print_string "\tSum: ";
    print_int sum;
    print_string "\t\tAverage: ";
    print_float ((float sum) /. (float total));
    print_newline()


end



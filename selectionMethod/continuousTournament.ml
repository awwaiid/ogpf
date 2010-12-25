
(* ContinuousTournament
 *
 * Continuously selects individuals, pulling them out of the population pool,
 * processing them, and then putting the back. We may also add offspring or
 * choose not to let an individual re-enter the population.
 *
 * History / Notes
 *   2003.12.17
 *     - Notes added
 *     - Moving to new infrastructure
 *     - Adapted from old main.ml
 *)


module Make
  ( Genotype: Genotype.Sig )
  ( Population: Population.Sig with type m = Genotype.t)
  ( FitnessTest: FitnessTest.Sig with type t = Genotype.t)
  = struct

  open Parameters
  let popSize = parameter.max_population

  let best_fitness = ref 1000000
  let best_g = ref (Genotype.randInstance 2)

  module StringSet = Set.Make(
    struct type t = string let compare = compare end
  )

  let rec unique_members' pop  =
    if pop == Population.empty then StringSet.empty
    else
      let a, pop = Population.pull_rand_member pop in
      let a = Genotype.to_string a in
      StringSet.add a (unique_members' pop)

  let unique_members pop =
    let set = unique_members' pop in
    StringSet.cardinal set

  let one_iteration pop =
    (*try *)
      (* First we pull two random members of the population *)
      let a, pop = Population.pull_rand_member pop in
      let b, pop = Population.pull_rand_member pop in

      (* Then we evaluate them both *)
      let a_val = FitnessTest.getFitness a in
      let b_val = FitnessTest.getFitness b in

      (* Check to see if the first one is the winner *)
      if a_val == 0 then begin
        print_string "Found Winner!\nWinner: ";
        Genotype.print a;
        print_string "\nValue: ";
        print_int a_val;
        print_newline();
        exit 0
      end;

      if (a_val < !best_fitness) then begin
        best_fitness := a_val;
        best_g := a;
      end;

      if (b_val < !best_fitness) then begin
        best_fitness := b_val;
        best_g := b;
      end;

      (* Check to see if our population is too small. If it is, BREED TIME! *)
      if Population.size pop < (popSize / 2) then
        let c = Genotype.combine a b in
        let pop = Population.add_member pop a in
        let pop = Population.add_member pop b in
        Population.add_member pop c

      (* Check to see if our population is too big. If it is, DEATH TIME! *)
      else if Population.size pop > popSize then begin
        (* let c = Genotype.combine a (Genotype.randInstance 6) in
        Population.add_member pop c *)
        Population.add_member pop a

      (* If member 'a' is too crappy, kill it *)
      (*
      else if a_val > 50 then
        Population.add_member pop b
      *)

      (* If 'a' is better than 'b' then lets just keep 'a' *)
      end else if a_val < b_val && Random.int 100 < 25 then begin
      (*else if a_val < b_val then begin *)
          Population.add_member pop a

      (* if 'a' is worse than 'b' then breed them, maybe 'b's goodness will wear
         off on 'a' *)
      end else if a_val > b_val then
        let c = Genotype.combine a b in
        (*let pop = Population.add_member pop a in *)
        let pop = Population.add_member pop b in
        let pop = Population.add_member pop (Genotype.combine b a) in
        Population.add_member pop c

      (* Well... all things being equal... lets do something at random! *)
      else (* if a_val == b_val then *)
        if Random.bool() then begin
          Population.add_member pop a
        end else
          let c = Genotype.combine a b in
          let pop = Population.add_member pop a in
          let pop = Population.add_member pop b in
          let c = Genotype.combine c (Genotype.randInstance 2) in
          Population.add_member pop c
    (* with _ -> pop *)

  let rec best_val pop =
    if pop == Population.empty then (Genotype.randInstance 6), 1000000
    else
      let g1, pop = Population.pull_rand_member pop in
      let v1 = FitnessTest.getFitness g1 in
      let g2, v2 = best_val pop in
      if v1 < v2 then g1, v1 else g2, v2

  let rec n_iterations pop n =
    if n = 0 then pop
    else begin
      if n mod popSize == 0 then begin
        let n = parameter.generations - n in
        print_string "  generation #";
        let size = Population.size pop in
        print_int (n / popSize);
        print_string ":";
        print_int n;
        print_string "\tsize: ";
        print_int size;
        print_string "\tUnique: ";
        print_int (unique_members pop);
        print_string "\tBest: ";
  (*      let g, v = best_val pop in *)
        print_int (!best_fitness);
        print_string "\t\t";
        Genotype.print !best_g;
        print_newline()
        (* ignore (Unix.system (
          "echo \""
          ^ (string_of_int n)
          ^ "\t"
          ^ (string_of_int size)
          ^ "\t"
          ^ (string_of_int v)
          ^ "\" >> stats.txt"
        )) *)
      end;
      let pop = one_iteration pop in
      n_iterations pop (n - 1)
    end

  (* Main routine *)
  let run () =
    let p = Population.populate popSize in
    print_int (Population.size p);
    print_newline();
    Population.print p;
    Printf.printf "\nDoing %d iterations...\n" parameter.generations;
    let p = n_iterations p parameter.generations in
    print_string "New population:\n";
    Population.print p;
    print_newline()

end (* end of struct *)


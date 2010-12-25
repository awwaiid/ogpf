
(* rosesProblem.ml - Evolve from randomness the string "Roses are red."
 * 
 * This is an example problem specification. The only thing it really needs is
 * the fitnessTest and the terminateTest. I'm not even very sure that this
 * should go here as opposed to in the runSpecification file.
 *
 * This particular specification calls for a string-like genome
 *
 * History / Notes
 *   2003.12.16
 *     - Created
 *)

module Make ( Genotype: Genotype.Sig ) = struct

  type t = Genotype.t

  let target = Genotype.of_string "Roses are red."

  let min a b c =
    min a (min b c)

  (*
  let rec diff' a b cur_a cur_b =
    if cur_a == 0 then cur_b
    else if cur_b == 0 then cur_a
    else begin
      let r = if a.[cur_a] == b.[cur_b] then 0 else 1 in
      let left = (diff' a b (cur_a - 1) cur_b) + 1 in
      let up = (diff' a b cur_a (cur_b - 1)) + 1 in
      let diag = (diff' a b (cur_a - 1) (cur_b - 1)) + r in
      min left up diag
    end
  *)

  let diff' a b len_a len_b =
    let m = Array.make_matrix (len_a + 1) (len_b + 1) 0 in
    for i = 1 to len_a do
      m.(i).(0) <- i
    done;
    for j = 1 to len_b do
      m.(0).(j) <- j
    done;
    for i = 1 to len_a do
      for j = 1 to len_b do
        let r = if
          a.[i - 1] == b.[j - 1] then 0 else 1 in
        let left = m.(i - 1).(j) + 1 in
        let right = m.(i).(j - 1) + 1 in
        let diag = m.(i - 1).(j - 1) + r in
        m.(i).(j) <- min left right diag
      done
    done;
    m.(len_a).(len_b)



  (* Calculate the edit-distance between two strings *)
  (* Including transpositions, insertions, and deletions *)
  let diff a b =
    let len_a = String.length (Genotype.to_string a) in
    let len_b = String.length (Genotype.to_string b) in
    diff' (Genotype.to_string a) (Genotype.to_string b) len_a len_b


  let getFitness (g:t) =
    diff g target

(*  let getFitness (g:t) =
    Soundex.compare (Genotype.to_string g) (Genotype.to_string target) *)
  
end


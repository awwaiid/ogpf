
(* StringGenotype - a simple string-based genotype
 *
 * History / Notes
 *   2003.12.16
 *     - Pushed around a bit for new structure
 *)

(* The core type, this must be named "t" *)
type t = string

open Printf

let first_half_of a =
  let l = String.length a in
  if l > 1 then
    let l = l / 2 in
    String.sub a 0 l
  else a

let second_half_of a =
  let l = String.length a in
  if l > 1 then
    let l2 = l / 2 in
    String.sub a l2 (l - 2)
  else a


let rand_char () =
  Char.chr ((Random.int 0x5b) + 0x20)

(* This generates a complete randome genome *)
let randInstance depth =
  let len  = (Random.int depth) + 1 in
  let rec rstring n =
    if (n = 0) then ""
    else (String.make 1 (rand_char())) ^ (rstring (n - 1))
  in rstring len

(* This prints out the genome *)
let print g =
  print_string g;
  print_newline()

(* This converts the genome to a string. In this case, since it is already a
   string, this just returns the genome. *)
let to_string g =
  g

let point_mutate g =
  while Random.int 100 < 60 do
    let len = String.length g in
    let new_char = rand_char() in
    let loc = Random.int len in
    g.[loc] <- new_char
  done;
  g


let crossover' a b =
  try
    let cut_len = Random.int ((String.length a) + 1) in
    let sub_a = String.sub a 0 cut_len in
    let cut_len = Random.int ((String.length b) + 1) in
    let sub_b = String.sub b cut_len (String.length b) in
    let new_gene = sub_a ^ sub_b in
    if String.length new_gene == 0 then a
    else (point_mutate new_gene)
  with _ -> a

let crossover a b =
  if Random.bool() then
    crossover' a b
  else crossover' b a

(* This takes two genomes, a and b, and splices them together *)
(*let combine a b = (first_half_of a) ^ (second_half_of b) *)
let combine a b =
  if Random.bool() then
    crossover' a b
  else crossover' b a

let length a = String.length a

let at a i = a.[i]

let of_string s : t = s

let to_string (s:t) : string = s

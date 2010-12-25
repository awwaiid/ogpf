
type ptype = {
  mutable max_population: int;
  mutable min_population: int;
  mutable generations: int;
}

(* Defaults *) 
let parameter = {
  max_population = 100;
  min_population = 100;
  generations    = 100 * 500;
}


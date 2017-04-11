
type ptype = {
  mutable max_population: int;
  mutable min_population: int;
  mutable generations: int;
  mutable point_mutate_percent: int;
}

(* Defaults *) 
let parameter = {
  max_population = 100;
  min_population = 100;
  generations    = 100 * 500;
  point_mutate_percent = 60;
}


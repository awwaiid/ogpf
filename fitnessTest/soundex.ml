
(* Soundex algorithm *)

let values = 
(* ABCDEFGHIJKLMNOPQRSTUVWXYZ *)
  "01230120022455012623010202"

type exc = 
  | StringError of string * string
  | Other

exception Soundex_exc of exc

open Printf

let lth = 10

let soundex input = 
  let input = String.uppercase input in 
  let result = String.create lth in
  try
    let first = match input.[0] with
      | 'A' .. 'Z' -> result.[0] <- input.[0] 
      | _ -> raise (Invalid_argument "") in
    let rec other ri i =
      if i >= min (String.length result) (String.length input) then
        ri
      else
        let code = int_of_char input.[i] - int_of_char 'A' in
        if code < 0 || code > 25 then
          other ri (i+1)
        else
          let code = values.[int_of_char input.[i] - int_of_char 'A'] in
          if result.[ri - 1] = code || code = '0' then
            other ri (i+1)
          else (
            result.[ri] <- code;
            other (ri+1) (i+1)
          )
    in
    let end_ri = other 1 1 in
    for ri = end_ri to String.length result - 1 do
      result.[ri] <- '0'
    done;
    result
  with Invalid_argument x -> raise (Soundex_exc (StringError (input, result)))

    

let len_com_sub s s' = 
  let (ls, ls') = (fun f x x' -> f x, f x') String.length s s' in
  let mat = ArrayLabels.create_matrix ~dimx:(ls+1) ~dimy:(ls'+1) 0 in
  for i = 1 to ls   do 
    for j = 1 to ls'  do   
      mat.(i).(j) <- 
        if s.[i-1] = s'.[j-1] then
          mat.(i-1).(j-1) + 1
        else max mat.(i).(j-1) mat.(i-1).(j)
    done;
  done;
  mat.(ls).(ls')

open Printf

let compare a b =
  let a = soundex a in
  let b = soundex b in
  len_com_sub a b


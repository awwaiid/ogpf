
(* grabBag.ml - Provide a GrabBag data structure
 *
 * The purpose of this particular data structure is that of a Grab Bag. When
 * you put something in you don't particularly care where it goes (so long as
 * its in there somewhere), and when you pull one out you don't particularly
 * care which one comes out (so long as its no longer in).
 *
 * This particular implementation is based on a binary tree which keeps track
 * of how many leaves it has on both its left and right side. This makes it
 * easy to find the size -- you just add those two numbers from the root of the
 * tree. To grab something at random we take the size (total number of things
 * we have), pick a random number up to that. Then we start at the root... if
 * the number is bigger than the left, we go right and so on until we find the
 * node and pull it out. We make a note of the removal by subtraction on the
 * way back out. 2^n nodes can thus be accessed in 2*n operations. Adding works
 * similarly -- we move down the tree finding out where it is lopsided and add
 * the new thingie there.
 *
 * Fun, eh? Just a wild guess, but the random number is probably (pun intended)
 * the slowest part of the whole thing.
 *
 * History/Notes
 *   2003.04.01 (rbw)
 *     - Concieved and Created
 *)

(* Nice little exception for error handling *)
exception Error of string

(* Our GrabBag type *)
type 'a t =
    (* Leaves just contain a value *)
    Leaf of 'a
    (* Nodes contain the number of leaves on the left and right, in addition to
       the left and right nodes themselves *)
  | Node of int * int * 'a t * 'a t
    (* Empty nodes *)
  | Empty

(* Give back an empty bag *)
let empty = Empty

(* Find the size of a bag *)
let size = function
    (* If it is a leaf then there is just one *)
  | Leaf(_) -> 1
    (* If it is a node then we can short-cut by summing the left & right *)
  | Node(left_sum, right_sum, _, _) -> left_sum + right_sum
    (* Empty is empty of course *)
  | Empty -> 0

(* Given the offset of the current node, grab the nth node (removing it) *)
let rec grab' bag offset n =
  match bag with
    (* With just a leaf we give back its value and an empty bag *)
  | Leaf(v) -> v, Empty
    (* Trickiest case -- an inner node. *)
  | Node(left_sum, right_sum, left, right) ->
    (* Check to see if our value falls to the left of here *)
    if n < (offset + left_sum) then
      (* Now we just go trapsing through the left side looking for it *)
      let v, left = grab' left offset n in
      (* Now that we have it, lets make sure we have some children ourself *)
      if ((left_sum - 1) == 0) && (right_sum == 0) then
        (* No kids, our life is meaningless -- lets now be empty. *)
        v, Empty
      else
        (* We have kids, but one less of them on the left side *)
        v, Node(left_sum - 1, right_sum, left, right)
    else
      (* Well it must be on our right, so lets find it *)
      let v, right = grab' right (offset + left_sum) n in
      (* Okay, again we figure out if we have any remaining children *)
      if left_sum == 0 && right_sum - 1 == 0 then
        (* no kids - life meaningless - we become empty *)
        v, Empty
      else
        (* Got kids, but one less on the right side *)
        v, Node(left_sum, right_sum - 1, left, right)
    (* There were no nodes at all, so theres a problem for whoever wanted one *)
  | Empty -> raise (Error "No nodes available")

(* Grab the nth node (removing it from the bag *)
let grab bag =
  let n = size bag in
  let n = Random.int n in
  grab' bag 0 n


(* Add an item to a bag *)
let rec add' bag v =
  match bag with
    (* If we are down to a leaf lets split it to the right and add the new one
       to the left, becomming an inner-node *)
  | Leaf(_) as original ->
    Node(1, 1, Leaf(v), original)
    (* We're an inner node, lo lets add this new one to whichever side needs it
       the most *)
  | Node(left_sum, right_sum, left, right) ->
    if left_sum <= right_sum then
      (* The left side needs it, lets add it there *)
      let left = add' left v in
      (* Oh -- best not forget to keep count! One more on the left *)
      Node(left_sum + 1, right_sum, left, right)
    else
      (* The right side needs it, lets give it to them *)
      let right = add' right v in
      (* And again not losing count, add one to the right side *)
      Node(left_sum, right_sum + 1, left, right)
    (* Well we were nothing -- now we are something. What more can be said? *)
  | Empty -> Leaf(v)


(* Add an item to a bag *)
let add bag v =
  add' bag v



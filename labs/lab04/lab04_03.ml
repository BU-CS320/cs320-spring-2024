(* No Collisions on a Line

   Consider the following "game". X and Y stand on integers on the
   number line.  Each step they both move either left or right one
   integer, independently of one another.  They are not allowed to
   move to any integer that would force them to collide.  For example
   if X is on 1 and Y is on 2, then it is not allowed for X to move
   right and Y to move left (they cannot pass through each other).
   Likewise, if X is on 4 and Y is on 2, then it is not allowed for X
   to move left and Y to move right (they cannot occupy the same
   space).

   A sequence of moves is represent as a list of pairs of directions
   (`dir` below) where the first element of the pair is a move for X
   and the second element of the pair is a move for Y.

   Implement a function `no_collisions` which given

     start1 : an integer
     start2 : an integer
     num_steps: a nonnegative integer

   returns the list of all possible sequences of moves (without
   collisions) of length `num_steps` with X starting at `start1` and Y
   starting at `start2`

   Note to TA/TFs (and myself): It may be useful to draw a picture.

   Examples:
   let _ = assert (List.mem [(Left, Right)] (no_collisions 0 2 1))
   let _ = assert (not (List.mem [(Right, Left)] (no_collisions 0 2 1)))

*)

type dir = Left | Right

let no_collisions (start1 : int) (start2 : int) (num_steps : int) : ((dir * dir) list) list =
  assert false (* TODO *)

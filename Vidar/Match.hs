module Vidar.Match
(
) where

import Vidar

{-
  match :: Vidar -> Vidar -> ???

  Takes two Vidar structures. The first is the one we're testing
  against, the second is the one we made out of the parsed program.

  The result should contain the following information:
  - Did it match?
  - If not, what is the first thing that went wrong?
    - Probably the highest-level place in the syntax tree that went
      wrong
-}

data Result = Match
            | Fail -- ?? what should be here?

match :: Vidar a -- structure we want
      -> Vidar a -- structure we actually have
      -> Result
match v0 v1 = undefined

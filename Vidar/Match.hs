module Vidar.Match
( match
) where

import Vidar
import qualified Data.Map as M
import Control.Monad.State

{-
  match :: Vidar -> Vidar -> VidarMatch

  Takes two Vidar structures. The first is the one we're testing
  against, the second is the one we made out of the parsed program.

  The result should contain the following information:
  - Did it match?
  - If yes, have any SomeNames been associated with exact names?
  - If not, what is the first thing that went wrong?
    - Probably the highest-level place in the syntax tree that went
      wrong
-}

type VidarMatch a = StateT Bindings (Either Fail) a

data Fail = MismatchedNames String String
          | BadInput -- the right-hand Vidar is weird
    deriving Show

type Bindings = M.Map String String

eval :: VidarMatch a -> Either Fail a
eval v = evalStateT v $ M.empty

err :: Fail -> VidarMatch ()
err = lift . Left

match :: Eq a
      => Vidar a -- structure we want
      -> Vidar a -- structure we actually have
      -> VidarMatch ()
match _ _ = undefined

matchBlocks :: Eq a
            => Block a
            -> Block a
            -> VidarMatch ()
matchBlocks = undefined

matchNames :: Name -> Name -> VidarMatch ()
matchNames AnyName _ = return ()
matchNames (ExactName s1) (ExactName s2) =
  if s1 == s2
  then return ()
  else err $ MismatchedNames s1 s2
matchNames (SomeName n) (ExactName s) = do
  bs <- get
  case M.lookup n bs of
    Nothing -> (put $ M.insert n s bs) >> return ()
    Just s' -> if s == s' then return ()
               else err $ MismatchedNames s' s
matchNames _ _ = err BadInput


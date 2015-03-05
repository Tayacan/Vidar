module Vidar.Match
( match
, eval
) where

import Vidar
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative ((<$>))

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
          | BlockSize
          | MissingElement Element
          | BadInput String -- One of the Vidars contain stuff we can't handle (yet?)
          | NotFail Element Element
    deriving Show

type Bindings = M.Map String String

eval :: VidarMatch a -> Either Fail a
eval v = evalStateT v $ M.empty

err :: Fail -> VidarMatch a
err = lift . Left

match :: Vidar -- structure we want
      -> Vidar -- structure we actually have
      -> VidarMatch ()
match = matchElem

matchBlocks :: Block
            -> Block
            -> VidarMatch ()
matchBlocks (StrictBlock a) (StrictBlock b)    = matchStrict a b
matchBlocks (UnorderedBlock a) (StrictBlock b) = matchUnorderedStrict a b
matchBlocks _ _ = err $ BadInput "matchBlocks"

-- Match an actual strict block against the desired unordered block.
-- Each element in the unordered block must have a match in the strict block.
-- Each Not-element in the unordered block must not have any matches in the
-- strict block.
matchUnorderedStrict :: [Element] -> [Element] -> VidarMatch ()
matchUnorderedStrict a b = sequence_ $ map (matchAgainstBlock b) a

matchAgainstBlock :: [Element] -> Element -> VidarMatch Element
matchAgainstBlock block (Not e)  = case eval (matchAgainstBlock block e) of
    Right e'                -> err $ NotFail e e'
    Left (MissingElement _) -> return (Not e)
    Left (BadInput s)       -> err $ BadInput s
    Left e                  -> err e --return ()
matchAgainstBlock [] e           = err $ MissingElement e
matchAgainstBlock block Anything = return Anything
matchAgainstBlock block e = foldl f (err $ MissingElement e) $ map (\x -> (e, x)) block
  where f s (a, b) = case eval (matchElem a b) of
                           Left e -> case eval s of
                                       Right e -> return e
                                       Left _  -> err e
                           Right () -> return b
--matchAgainstBlock block e        = err $ BadInput $ "matchAgainstBlock: " ++ show block ++ ", " ++ show e

matchStrict :: [Element] -> [Element] -> VidarMatch ()
matchStrict a b = case zipWith' matchElem a b of
    Nothing -> err BlockSize
    Just xs -> sequence_ xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
zipWith' f (x:xs) (y:ys) = (f x y :) <$> zipWith' f xs ys
zipWith' f [] []         = Just []
zipWith' _ _ _           = Nothing

matchElem :: Element -> Element -> VidarMatch ()
matchElem Anything  _         = return ()
matchElem (Name n1) (Name n2) = matchNames n1 n2
matchElem (Not e) e' = do
    bs <- get
    case evalStateT (matchElem e e') bs of
        Right ()          -> err $ NotFail e e'
        Left (BadInput s) -> err $ BadInput s
        Left e            -> return ()
matchElem (Binding n e) (Binding n' e') = do
    matchNames n n'
    matchElem e e'
matchElem (Block n b) (Block n' b') = do
    matchNames n n'
    matchBlocks b b'
matchElem (SubBlock b) (SubBlock b') = matchBlocks b b'
matchElem _ _ = err $ BadInput "matchElem"

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
matchNames _ _ = err $ BadInput "matchNames"


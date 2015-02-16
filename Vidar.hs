module Vidar where

import qualified Data.Map as M
import Data.List (intercalate)

data Name = SomeName String
          | ExactName String
          | AnyName

type Nametable = M.Map String String

data Block v = UnorderedBlock [Element v]
             | OrderedBlock [Element v]
             | StrictBlock [Element v]

data Element value = Block Name (Block value)
                   | SubBlock (Block value)
                   | Binding Name (Element value) -- Variable binding
                   | Expr value                   -- Expression
                   | Anything

type Vidar value = Element value

ppName :: Name -> String
ppName AnyName = "_"
ppName (ExactName s) = "`" ++ s ++ "`"
ppName (SomeName s)  = s

ppBlock :: Show v => Int -> Int -> (Block v) -> String
ppBlock d0 d1 (UnorderedBlock elems) =
    replicate d0 ' ' ++ "{" ++ "\n" ++
    concatMap (ppElem (d1 + 2)) elems ++
    replicate d1 ' ' ++ "}\n"
ppBlock d0 d1 (OrderedBlock elems) =
    replicate d0 ' ' ++ "[" ++ "\n" ++
    concatMap (ppElem (d1 + 2)) elems ++
    replicate d1 ' ' ++ "]\n"
ppBlock d0 d1 (StrictBlock elems) =
    replicate d0 ' ' ++ "(" ++ "\n" ++
    concatMap (ppElem (d1 + 2)) elems ++
    replicate d1 ' ' ++ ")\n"

ppElem :: Show a => Int -> Element a -> String
ppElem d (Block n b) =
    replicate d ' ' ++
    ppName n ++
    ppBlock 1 d b
ppElem d (SubBlock b) =
    ppBlock d d b
ppElem d (Binding n e) =
    replicate d ' ' ++
    ppName n ++ " = \n" ++
    ppElem (d + 2) e
ppElem d (Expr value) =
    replicate d ' ' ++ show value ++ "\n"
ppElem d Anything = replicate d ' ' ++ "_\n"

ppVidar :: Show a => [Element a] -> String
ppVidar elems = intercalate "\n\n" $ map (ppElem 0) elems

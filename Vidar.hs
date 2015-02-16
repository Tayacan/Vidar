module Vidar where

import Data.List (intercalate)

data Name = SomeName String
          | ExactName String
          | AnyName

data Block = UnorderedBlock [Element]
           | OrderedBlock [Element]
           | StrictBlock [Element]

data Element = Block Name Block
             | SubBlock Block
             | Binding Name Element -- Variable binding
             | Name Name
             | Anything
             | Not Element

type Vidar = Element

ppName :: Name -> String
ppName AnyName = "_"
ppName (ExactName s) = "`" ++ s ++ "`"
ppName (SomeName s)  = s

ppBlock :: Int -> Int -> Block -> String
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

ppElem :: Int -> Element -> String
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
ppElem d Anything = replicate d ' ' ++ "_\n"
ppElem d (Name n) = replicate d ' ' ++ ppName n ++ "\n"
ppElem d (Not x) = replicate d ' ' ++ "~ " ++ ppElem 0 x

ppVidar :: [Element] -> String
ppVidar elems = intercalate "\n\n" $ map (ppElem 0) elems

module Vidar where

import Data.List (intercalate, intersperse)

data Name = SomeName String
          | ExactName String
          | AnyName
          deriving Show

data Block = UnorderedBlock [Element]
           | OrderedBlock [Element]
           | StrictBlock [Element]
           deriving Show

data Element = Block Name Block
             | SubBlock Block
             | Binding Name Element -- Variable binding
             | Name Name
             | Anything
             | Not Element
             deriving Show

type Vidar = Element

ppName :: Name -> String
ppName AnyName = "_"
ppName (ExactName s) = "`" ++ s ++ "`"
ppName (SomeName s)  = s

ppBlock :: Int -> Int -> Block -> String
ppBlock d0 d1 (UnorderedBlock elems) =
    replicate d0 ' ' ++ "{" ++ "\n" ++
    (concat . intersperse ",\n" . map (ppElem $ d1 + 2) $ elems) ++ "\n" ++
    replicate d1 ' ' ++ "}"
ppBlock d0 d1 (OrderedBlock elems) =
    replicate d0 ' ' ++ "[" ++ "\n" ++
    (concat . intersperse ",\n" . map (ppElem $ d1 + 2) $ elems) ++ "\n" ++
    replicate d1 ' ' ++ "]"
ppBlock d0 d1 (StrictBlock elems) =
    replicate d0 ' ' ++ "(" ++ "\n" ++
    (concat . intersperse ",\n" . map (ppElem $ d1 + 2) $ elems) ++ "\n" ++
    replicate d1 ' ' ++ ")"

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
ppElem d Anything = replicate d ' ' ++ "_"
ppElem d (Name n) = replicate d ' ' ++ ppName n
ppElem d (Not x) = replicate d ' ' ++ "~ " ++ dropWhile (== ' ') (ppElem d x)

ppVidar :: [Vidar] -> String
ppVidar elems = intercalate "\n\n" $ map (ppElem 0) elems

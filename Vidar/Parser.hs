module Vidar.Parser
( parse
) where

import Vidar
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative ((<$>))

parse :: String -> Either ParseError [Vidar]
parse s = case readP_to_S (many1 element) s of
    [(e, "")] -> Right e
    _         -> Left ParseError

data ParseError = ParseError
    deriving Show

token :: ReadP a -> ReadP a
token p = skipSpaces >> p

symbol :: Char -> ReadP Char
symbol = token . char

element :: ReadP Element
element = (anything
      +++ (SubBlock <$> block)
      +++ namedblock
      +++ binding
      +++ notElement)
      <++ (Name <$> name)

notElement :: ReadP Element
notElement = do
    _ <- symbol '~'
    e <- element
    return $ Not e

namedblock :: ReadP Element
namedblock = do
    n <- name
    b <- block
    return $ Block n b

binding :: ReadP Element
binding = do
    n <- name
    _ <- symbol '='
    e <- element
    return $ Binding n e

block :: ReadP Block
block = ordered +++ unordered +++ strict

ordered :: ReadP Block
ordered = do
    _ <- symbol '['
    contents <- sepBy element (symbol ',')
    _ <- symbol ']'
    return $ OrderedBlock contents

unordered :: ReadP Block
unordered = do
    _ <- symbol '{'
    contents <- sepBy element (symbol ',')
    _ <- symbol '}'
    return $ UnorderedBlock contents

strict :: ReadP Block
strict = do
    _ <- symbol '('
    contents <- sepBy element (symbol ',')
    _ <- symbol ')'
    return $ StrictBlock contents

anything :: ReadP Element
anything = token $ char '_' >> return Anything

name :: ReadP Name
name = anyname +++ exactname +++ somename

anyname :: ReadP Name
anyname = token $ char '_' >> return AnyName

exactname :: ReadP Name
exactname = token $ do
    _ <- char '"'
    n <- validname
    _ <- char '"'
    return $ ExactName n

somename :: ReadP Name
somename = token $ do
    n <- validname
    return $ SomeName n

validname :: ReadP String
validname = munch1 isAlphaNum

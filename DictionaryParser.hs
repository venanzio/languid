-- READING DICTIONARY IN XML FORMAT

module DictionaryParser where

import Dictionary

import Text.ParserCombinators.Parsec
-- import System.Random (randomRIO)

{- Parser for translations

   translations: gives the list of translations for the entry
   different translations are separated by "," or ";"
   ignores spaces and text in parentheses
-}

translations :: DEntry -> [String]
translations e =
  let (Right ts) = parse translationsP "translation error" (translation e)
  in ts

translationsP :: Parser [String]
translationsP = sepBy translationP (oneOf ",;")

filler :: Parser String
filler = many (space <|> newline)

translationP :: Parser String
translationP = do filler
                  ts <- sepBy trtext sppar
                  return (shaveSpaces (concat ts))
  where shaveSpaces = reverse . shvsp . reverse . shvsp
        shvsp [] = ""
        shvsp (' ':ts) = shvsp ts
        shvsp (c:ts) = c:shvsp ts

sppar = between (char '(') (char ')') (many1 (noneOf ")")) 
trtext = many (noneOf ",;()")





{- PARSER FOR DICTIONARY
   Dictionary is in XML format
-}


readDictionary :: String -> IO Dictionary
readDictionary file =
  do contents <- readFile file
     return (parseDictionary contents)

parseDictionary :: String -> Dictionary
parseDictionary dicTxt =
  case (parse dicP "dictionary"  dicTxt) of
    Left  m -> error "Not a correct XML dictionary."
    Right d -> d

openTag :: GenParser Char st String
openTag = try (do char '<'
                  t <- many1 (alphaNum)
                  char '>'
                  return t)

closeTag :: GenParser Char st String
closeTag = do string "</"
              t <- many1 (alphaNum)
              char '>'
              return t

dicP :: Parser Dictionary
dicP = do filler
          string "<dictionary>"
          d <- many (try entryP)
          filler
          string "</dictionary>"
          return d

entryP :: Parser DEntry
entryP = do filler
            string "<entry>"
            w <- wordP
            efs <- many (try entryFieldP)
            filler
            string "</entry>"
            return (DEntry w efs)

wordP :: Parser String
wordP = do  filler
            string "<word>" 
            w <- many (noneOf "<")
            string "</word>"
            return w

entryFieldP :: Parser DEntryField
entryFieldP =
  foldr (\(tag,constr) p -> (try (genericField tag constr)) <|> p)
        (fail "no entry field")
        [("pronunciation", Pronunciation),
         ("usage", Usage),
         ("translation", Translation),
         ("phrase", Phrase),
         ("note", Note),
         ("rchecks", \x -> RChecks (read x)),
         ("wchecks", \x -> WChecks (read x))]
  where genericField tag constr =
          do filler
             string ('<':tag++">")
             filler
             x <- many (noneOf "<")
             string ("</"++tag++">")
             return (constr x)

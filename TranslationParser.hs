-- Divides a translation string into separate translations

module TranslationParser where

{-
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

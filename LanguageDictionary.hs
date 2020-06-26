-- Module with definition and functions on dictionaries
-- Venanzio Capretta, August 2011
-- version 0.5

module LanguageDictionary       
  where

import Text.ParserCombinators.Parsec
import System.Random (randomRIO)

type Dictionary = [DEntry]
data DEntry = DEntry String [DEntryField]
data DEntryField = 
  Pronunciation String | Usage String | Translation String |
  Phrase String | Note String |
  RChecks Int | WChecks Int 

checksNum :: Int
checksNum = 5  -- initial number of reading checks

wchecksNum :: Int
wchecksNum = 0 -- initial number of writing checks
               -- used for the Italian dictionary

totalChecks :: Int
totalChecks = 300  -- ideal total number of checks

getWord :: DEntry -> String
getWord (DEntry w _) = w

readChecks :: DEntry -> Int
readChecks (DEntry _ l) =
  foldr (\field continue -> 
          case field of RChecks n -> n
                        _ -> continue)
        0 l
  
writeChecks :: DEntry -> Int
writeChecks (DEntry _ l) = 
  foldr (\field continue -> 
          case field of WChecks n -> n
                        _ -> continue)
        0 l

translation :: DEntry -> String
translation (DEntry _ l) = 
  foldr (\field continue -> 
          case field of Translation t -> t
                        _ -> continue)
        "" l  

-- translations: gives the list of translations for the entry
--   different translations are separated by "," or ";"
--   ignores spaces and text in parentheses

-- REVIEWED UP TO HERE
-- Use applicative style to define the parsers

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

pronunciation :: DEntry -> String
pronunciation (DEntry _ l) = findPronunciation l
  where findPronunciation (Pronunciation p : efs) = p
        findPronunciation (_ : efs) = findPronunciation efs
        findPronunciation [] = ""

usage :: DEntry -> String
usage (DEntry _ l) = findUsage l
  where findUsage (Usage u : efs) = u
        findUsage (_ : efs) = findUsage efs
        findUsage [] = ""

phrases :: DEntry -> String
phrases (DEntry _ l) = foldl phraseapp "" l
  where phraseapp acc (Phrase ph) = acc++ph
        phraseapp acc _ = acc    

note :: DEntry -> String
note (DEntry _ l) = findNote l
  where findNote (Note u : efs) = u
        findNote (_ : efs) = findNote efs
        findNote [] = ""

totalRChecks :: Dictionary -> Int
totalRChecks [] = 0
totalRChecks (e:es) = readChecks e + totalRChecks es

totalRWords :: Dictionary -> Int
totalRWords [] = 0
totalRWords (e:es) = if readChecks e == 0
                     then totalRWords es
                     else totalRWords es + 1

totalWChecks :: Dictionary -> Int
totalWChecks [] = 0
totalWChecks (e:es) = writeChecks e + totalWChecks es

totalWWords :: Dictionary -> Int
totalWWords [] = 0
totalWWords (e:es) = if writeChecks e == 0
                     then totalWWords es
                     else totalWWords es + 1

-- PRINTING OF DICTIONARY ENTRIES

instance Show DEntry where
  show = renderDEntry

renderDEntry :: DEntry -> String
renderDEntry (DEntry w l) = "WORD: "++w++'\n':
                          brklines (map renderDEntryField l)

brklines :: [String] -> String
brklines [] = ""
brklines [f] = f
brklines (f:fs) = f++'\n':brklines fs

renderDEntryField :: DEntryField -> String
renderDEntryField (Pronunciation p) = "\n> Pronunciation: "++p
renderDEntryField (Usage u)         = "\n> Grammar: "++u
renderDEntryField (Translation t)   = "\n> Translation:\n"++t
renderDEntryField (Phrase p)        = "\n> Example phrases:\n"++p
renderDEntryField (Note n)          = "\n> Note:\n"++n
renderDEntryField (RChecks k)       = "\n> Reading checks: "++show k
renderDEntryField (WChecks k)       = "\n> Writing checks: "++show k

-- SEARCHING FOR A WORD IN A DICTIONARY

dicLookup :: String -> Dictionary -> Maybe DEntry
dicLookup w [] = Nothing
dicLookup w (e:es) | w == getWord e = Just e
                   | otherwise      = dicLookup w es

consultDictionary :: Dictionary -> IO ()
consultDictionary d = do
  putStrLn "SEARCH WORD ('Enter' to stop):"
  word <- getLine
  if word==""
   then return ()
   else do 
     putStrLn "---------------------------"
     d' <- case dicLookup word d of
             Nothing -> putStrLn ("The word \""++word++"\""++" is not in the dictionary.")
             Just e -> putStrLn (renderDEntry e)
     putStrLn "---------------------------"
     consultDictionary d

-- WRITING DICTIONARY IN XML FORMAT

xmlDEntry :: DEntry -> String
xmlDEntry (DEntry w l) = unlines
  (["<entry>","<word>"++w++"</word>"] ++ map xmlDEntryField l ++
   ["</entry>"])
  where xmlDEntryField (Usage u) = if u=="" then "" else "<usage>"++u++"</usage>"
        xmlDEntryField (Pronunciation p) = if p=="" then "" else "<pronunciation>"++p++"</pronunciation>"
        xmlDEntryField (Translation t) = "<translation>"++t++"</translation>"
        xmlDEntryField (Phrase p) = if p=="" then "" else "<phrase>"++p++"</phrase>"
        xmlDEntryField (Note n) = if n=="" then "" else "<note>"++n++"</note>"
        xmlDEntryField (RChecks k) = if k==0 then "" else "<rchecks>"++(show k)++"</rchecks>"
        xmlDEntryField (WChecks k) = if k==0 then "" else "<wchecks>"++(show k)++"</wchecks>"

xmlDictionary :: Dictionary -> String
xmlDictionary d = "<dictionary>\n"++
                  (brklines (map xmlDEntry d)) ++
                  "\n</dictionary>"

writeDictionary :: String -> Dictionary -> IO ()
writeDictionary file d = do
  writeFile file (xmlDictionary d)

-- READING DICTIONARY IN XML FORMAT

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

-- Selecting entries

randomEntry :: Dictionary -> IO (DEntry)
randomEntry d = do j <- randomRIO (0::Int,length d -1)
                   return (d!!j)

-- Selecting an entry, using checks as weights

nthReadEntry :: Dictionary -> Int -> DEntry
nthReadEntry (e : es) i =
  if i<=c then e else nthReadEntry es (i-c)
    where c = readChecks e
          
nthWriteEntry :: Dictionary -> Int -> DEntry
nthWriteEntry (e : es) i =
  if i<=c then e else nthWriteEntry es (i-c)
    where c = writeChecks e


checkTranslation :: DEntry -> String -> Bool
checkTranslation e w = elem (noSpaces w) (translations e)
  where noSpaces "" = ""
        noSpaces (' ':w) = noSpaces w
        noSpaces (c:w) = c:noSpaces w


-- Replace an entry with a new one for the same word

updateDictionary :: Dictionary -> DEntry -> Dictionary
updateDictionary [] e = [e]
updateDictionary (e':es) e = if getWord e' == getWord e
                             then e : es
                             else e' : updateDictionary es e

updateRChecks :: DEntry -> Int -> DEntry
updateRChecks (DEntry w l) c = DEntry w (upEF l)
  where upEF (RChecks _ : efs) = upEF efs
        upEF (ef : efs) = ef : upEF efs
        upEF [] = [RChecks c]

updateWChecks :: DEntry -> Int -> DEntry
updateWChecks (DEntry w l) c = DEntry w (upEF l)
  where upEF (WChecks _ : efs) = upEF efs
        upEF (ef : efs) = ef : upEF efs
        upEF [] = [WChecks c]


-- select a random entry, using read checks as weights

randREntry :: Dictionary -> IO DEntry
randREntry dic = do
  let rchecks = totalRChecks dic
  ne <- randomRIO (0, max rchecks totalChecks)
  if ne > rchecks then do putStrLn "jolly word"
                          randomEntry dic
                  else return (nthReadEntry dic ne)

-- Reading test

rTestEntry :: DEntry -> IO Bool
rTestEntry e = do
  putStrLn ("Word:          " ++ getWord e)
  putStrLn ("Pronunciation: " ++ pronunciation e)
  putStr ("Translation: ")
  tr <- getLine
  let b = checkTranslation e tr
  if b then putStrLn "--------\nCORRECT!\n--------"
       else putStrLn "------\nWRONG!\n------"
  return b

rTest :: Dictionary -> IO Dictionary
rTest dic = do
  e <- randREntry dic
  -- putStrLn ("Read Checks: " ++ show (readChecks e))
  b <- rTestEntry e
  let rch = readChecks e + if b then (-1) else 1
      e' = (updateRChecks e rch)
      dic' = updateDictionary dic e'
  putStrLn ("\n\n" ++ show e')
  return dic'

readTest :: Dictionary -> IO Dictionary
readTest dic = do
  dic' <- rTest dic
  putStrLn "\nContinue? (Enter = Continue, N = Stop)"
  cont <- getLine
  if cont == "N" then return dic'
                 else readTest dic'


-- Writing test

randWEntry :: Dictionary -> IO DEntry
randWEntry dic = do
  let wchecks = totalWChecks dic
  ne <- randomRIO (0, max wchecks totalChecks)
  if ne > wchecks then do putStrLn "jolly word"
                          randomEntry dic
                  else return (nthWriteEntry dic ne)

wTestEntry :: DEntry -> IO Bool
wTestEntry e = do
  putStrLn ("Translation: " ++ translation e)
  putStr ("Word: ")
  w <- getLine
  let b = w == getWord e
  if b then putStrLn "--------\nCORRECT!\n--------"
       else putStrLn "------\nWRONG!\n------"
  return b

wTest :: Dictionary -> IO Dictionary
wTest dic = do
  e <- randWEntry dic
  -- putStrLn ("Writing Checks: " ++ show (writeChecks e))
  b <- wTestEntry e
  let rch = writeChecks e + if b then (-1) else 1
      e' = (updateWChecks e rch)
      dic' = updateDictionary dic e'
  putStrLn ("\n\n" ++ show e')
  return dic'

writeTest :: Dictionary -> IO Dictionary
writeTest dic = do
  dic' <- wTest dic
  putStrLn "\nContinue? (Enter = Continue, N = Stop)"
  cont <- getLine
  if cont == "N" then return dic'
                 else writeTest dic'



-- Look up a word, if not in dictionary, offer the chance to add

lookUpChange :: Dictionary -> String -> IO Dictionary
lookUpChange dic word =
  case dicLookup word dic of
    Just e  ->
      do putStrLn (show e)
         return dic
    Nothing -> do
      putStrLn "The word is not in the dictionary."
      putStr "Do you want to add it (Y or Yes) ? "
      ch <- getLine
      if ch=="Y"
        then do
          e <- getEntry word
          return (updateDictionary dic e)
        else return dic
                 
getEntry :: String -> IO DEntry
getEntry word = do
  putStrLn ("Enter dictionary data for word: " ++ word)
  putStr "Pronunciation: "
  pr <- getLine
  putStr "Grammar: "
  gr <- getLine
  putStr "Translation(s): "
  tr <- getLine
  putStrLn "\nExample phrases:"
  ph <- getPhrases
  putStrLn "\nNote:"
  nt <- getLine
  return (DEntry word [Pronunciation pr,
                       Usage gr,
                       Translation tr,
                       Phrase ph,
                       Note nt,
                       RChecks checksNum,
                       WChecks checksNum
                      ])

getPhrases :: IO String
getPhrases = do
  putStrLn "Example phrase (Enter to stop): "
  ph <- getLine
  if ph == "" then return ""
              else do
    putStrLn "Translation:"
    tr <- getLine
    let phr = ph ++ if tr == "" then ""
                                else ("\n"++tr++"\n")
    phrs <- getPhrases
    return (phr ++ "\n" ++ phrs)
  
-- A version of consult that allows addition of entries

consultDic :: Dictionary -> IO Dictionary
consultDic dic = do
  putStr ("Looking for word (Enter to stop): ")
  word <- getLine
  if word == ""
    then return dic
    else do dic' <- lookUpChange dic word
            consultDic dic'

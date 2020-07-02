-- Definition of Dictionary data structure

module Dictionary where

{- A dictionary is a list of entries
   Each entry is a word and a list of entry fields
   the fields contain translations, pronunciation, grammar, etc
   and number of checks for training

   There can be several fields with the same constructor
   for example several translations, several example phrases, etc.
-}

type Dictionary = [DEntry]
data DEntry = DEntry String [DEntryField]
data DEntryField = Pronunciation String
                 | Usage String         -- grammar information
                 | Translation String
                 | Phrase String
                 | Note String
                 | RChecks Int          -- number of checks for reading test
                 | WChecks Int          -- number of checks for writing test


-- Some constants for training

checksNum :: Int
checksNum = 5  -- initial number of reading/writing checks

{-
wchecksNum :: Int
wchecksNum = 5 -- initial number of writing checks
-}

totalChecks :: Int
totalChecks = 300  -- ideal total number of checks


-- Selecting components of an entry

deWord :: DEntry -> String
deWord (DEntry w _) = w

getWord = deWord  -- for compatibility with older version

dePronunciation :: DEntry -> String
dePronunciation (DEntry _ l) = findPronunciation l
  where findPronunciation (Pronunciation p : efs) = p
        findPronunciation (_ : efs) = findPronunciation efs
        findPronunciation [] = ""

pronunciation = dePronunciation  -- for compatibility

deUsage :: DEntry -> String
deUsage (DEntry _ l) = findUsage l
  where findUsage (Usage u : efs) = u
        findUsage (_ : efs) = findUsage efs
        findUsage [] = ""

usage = deUsage -- for compatibility

deTranslation :: DEntry -> String
deTranslation (DEntry _ l) = 
  foldr (\field continue -> 
          case field of Translation t -> t
                        _ -> continue)
        "" l  

translation = deTranslation

dePhrase :: DEntry -> String
dePhrase (DEntry _ l) = foldl phraseapp "" l
  where phraseapp acc (Phrase ph) = acc++ph
        phraseapp acc _ = acc    

phrases = dePhrase -- for compatibility

deNote :: DEntry -> String
deNote (DEntry _ l) = findNote l
  where findNote (Note u : efs) = u
        findNote (_ : efs) = findNote efs
        findNote [] = ""

note = deNote -- for compatibility

deRChecks :: DEntry -> Int
deRChecks (DEntry _ l) =
  foldr (\field continue -> 
          case field of RChecks n -> n
                        _ -> continue)
        0 l

readChecks = deRChecks -- for compatibility with older version

deWChecks :: DEntry -> Int
deWChecks (DEntry _ l) = 
  foldr (\field continue -> 
          case field of WChecks n -> n
                        _ -> continue)
        0 l

writeChecks = deWChecks  -- for compatibility with older version

-- 

--------------------------------------------------------------------


  


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

-- Replace an entry with a new one for the same word

updateDictionary :: Dictionary -> DEntry -> Dictionary
updateDictionary [] e = [e]
updateDictionary (e':es) e = if getWord e' == getWord e
                             then e : es
                             else e' : updateDictionary es e

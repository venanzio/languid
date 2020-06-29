module LookUp where

import Dictionary

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

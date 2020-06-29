module Tests where

import Dictionary
import Parser

import System.Random (randomRIO)


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


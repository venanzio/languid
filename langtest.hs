-- Word training with a language dictionary

import LanguageDictionary
import Data.List
import System.IO

readTestDic :: String -> IO ()
readTestDic fileName = do
  dic <- readDictionary (fileName++".dic")
  writeDictionary (fileName++".dic.bak") dic  -- back up
  dic' <- readTest dic
  writeDictionary (fileName++".dic") dic'
  
writeTestDic :: String -> IO ()
writeTestDic fileName = do
  dic <- readDictionary (fileName++".dic")
  writeDictionary (fileName++".dic.bak") dic  -- back up
  dic' <- readTest dic
  writeDictionary (fileName++".dic") dic'

lookupAddDic :: String -> IO ()
lookupAddDic fileName = do
  dic <- readDictionary (fileName++".dic")
  writeDictionary (fileName++".dic.bak") dic  -- back up
  dic' <- consultDic dic
  writeDictionary (fileName++".dic") dic'


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Vocabulary Training\n\n"
  putStr "Dictionary name: "
  fileName <- getLine
  dic <- readDictionary (fileName++".dic")
  writeDictionary (fileName++".dic.bak") dic  -- back up

  putStrLn("\nWords in the dictionary : " ++ show (length dic))

  putStrLn ("\nRead training words  : " ++ show (totalRWords dic))
  putStrLn ("Total read checks    : " ++ show (totalRChecks dic))

  putStrLn ("\nWrite training words : " ++ show (totalWWords dic))
  putStrLn ("Total read checks    : " ++ show (totalWChecks dic))

  putStr "\nRead test (R), write test (W), or look-up (L)? "
  choice <- getLine
  dic' <- case choice of    
    "R" -> readTest dic 
    "W" -> writeTest dic
    "L" -> consultDic dic
  
  writeDictionary (fileName++".dic") dic'

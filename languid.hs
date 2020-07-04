{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as Text
import System.Environment

import Dictionary
import Parser
import WriteDictionary
import LookUp
import EasyWidgets
import LangWidget
import LookUpGUI

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
      l = length fileName
      dictFile = if (drop (l - 4) fileName == ".dic")
                   then (take (l - 4) fileName)
                   else fileName
  dic <- readDictionary (dictFile++".dic")
  writeDictionary (dictFile++".dic.bak") dic  -- back up

  showStats dic

  dic' <- lookUpGUI dic

  showStats dic'

  writeDictionary (dictFile++".dic") dic'


showStats dic = do
  putStrLn("\nWords in the dictionary : " ++ show (length dic))
  putStrLn ("\nRead training words  : " ++ show (totalRWords dic))
  putStrLn ("Total read checks    : " ++ show (totalRChecks dic))
  putStrLn ("\nWrite training words : " ++ show (totalWWords dic))
  putStrLn ("Total read checks    : " ++ show (totalWChecks dic))




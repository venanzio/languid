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
  let dictFile = head args
  dic <- readDictionary (dictFile++".dic")
  writeDictionary (dictFile++".dic.bak") dic  -- back up

  dic' <- lookUpGUI dic

  writeDictionary (dictFile++".dic") dic'




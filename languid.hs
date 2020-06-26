{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{-
 The simplest possible text editor
Invoke it with a text file name
It displays the text on a TextView widgets
The user modifies it
  when they close the window, the content of
  the text buffer is saved back into the file
-}

module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as Text
import System.Environment

import LanguageDictionary

-- A data structure containing all the widgets needed by languid
data LangWin = LangWin { everything :: Gtk.TextBuffer }
  

main :: IO ()
main = do
  args <- getArgs
  let dictFile = head args
  dic <- readDictionary (dictFile++".dic")
  writeDictionary (dictFile++".dic.bak") dic  -- back up

  dic' <- dictGUI dic

  writeDictionary (dictFile++".dic") dic'

dictGUI :: Dictionary -> IO Dictionary
dictGUI dic = do
  Gtk.init Nothing  -- initializes the Gtk windowing system

  -- create a new window
  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit
    -- when closing the window, end everything

  #showAll win

  Gtk.main
  
  return dic

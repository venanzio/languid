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


-- Look Up
dictGUI :: Dictionary -> IO Dictionary
dictGUI dic = do
  Gtk.init Nothing 

  (lwWidget, lw) <- newLangWidget

  win <- new Gtk.Window [ #title := "Dictionary Look-up" ]
  #setDefaultSize win 400 (-1)
  on win #destroy Gtk.mainQuit

  searchEntry <- new Gtk.Entry [ #text := "search word" ]

  searchButton <- new Gtk.Button [ #label := "look-up" ]
  on searchButton #clicked (do
    word <- readEntry searchEntry
    let luWord = dicLookup word dic
    case luWord of
      Nothing -> do
        clearLW lw
        writeEntry (lwWord lw) "not in dictionary"
        return ()
      Just entry -> displayEntry lw entry
    )

  luBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  #add luBox searchEntry
  #add luBox searchButton

  sep <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal ]

  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #setSpacing box 20

  #add box luBox
  #add box sep
  #add box lwWidget

  #add win box

  #showAll win

  Gtk.main
  
  return dic


main :: IO ()
main = do
  args <- getArgs
  let dictFile = head args
  dic <- readDictionary (dictFile++".dic")
  writeDictionary (dictFile++".dic.bak") dic  -- back up

  dic' <- dictGUI dic

  writeDictionary (dictFile++".dic") dic'

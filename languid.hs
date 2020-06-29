{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as Text
import System.Environment

import LanguageDictionary

-- A data structure containing all the widgets needed by languid
data LangWin = LangWin {
    lwWord :: Gtk.EntryBuffer
  , lwTranslation :: Gtk.EntryBuffer
  }

{-
data LanguageGUI = LanguageGUI {
  window             :: Window,
  inputEntry         :: Entry,
  messageLabel       :: Label,
  wordEntry          :: Entry,
  pronunciationEntry :: Entry,
  grammarEntry       :: Entry,
  translationBuffer  :: TextBuffer,
  phraseBuffer       :: TextBuffer,
  noteBuffer         :: TextBuffer,
  wordnumLabel       :: Label,
  readnumLabel       :: Label,
  writenumLabel      :: Label,
  checksRSpin        :: SpinButton,
  checksWSpin        :: SpinButton,
  okButton           :: Button,
  modifyButton       :: Button,
  clearButton        :: Button,
  saveButton         :: Button,
  modeBox            :: ComboBox,
  dicref             :: IORef Dictionary,
  entryref           :: IORef DEntry,
  moderef            :: IORef LanguageMode
  }
-}
  
-- Creates a new LangWin, puts it inside a grid and returns both
newLangWin :: IO (Gtk.Grid, LangWin)
newLangWin = do
  wordEntry <- new Gtk.Entry [ #text := "word" ]
  wordBuff <- Gtk.entryGetBuffer wordEntry
  
  transEntry <- new Gtk.Entry [ #text := "translation" ]
  transBuff <- Gtk.entryGetBuffer transEntry

  -- putting all the widgets on a grid
  grid <- new Gtk.Grid [ #orientation := Gtk.OrientationHorizontal ]
  Gtk.gridAttach grid wordEntry  1 1 1 1
  Gtk.gridAttach grid transEntry 1 2 1 2

  return (grid, LangWin { lwWord = wordBuff, lwTranslation = transBuff })


dictGUI :: Dictionary -> IO Dictionary
dictGUI dic = do
  Gtk.init Nothing 

  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit

  (lwWidget, lw) <- newLangWin
  #add win lwWidget

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

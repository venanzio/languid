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

-- delete all content in a LangWin structure
clearLW :: LangWin -> IO ()
clearLW lw = do
  #deleteText (lwWord lw) 0 (-1)
  #deleteText (lwTranslation lw) 0 (-1)
  return ()  

-- puts an entry into a LangWin structure (displaying all the fields)
displayEntry :: LangWin -> DEntry -> IO ()
displayEntry lw entry = do
  clearLW lw
  #insertText (lwWord lw) 0 (Text.pack (getWord entry)) (-1)
  #insertText (lwTranslation lw) 0 (Text.pack (translation entry)) (-1)
  return ()

-- Look up
dictGUI :: Dictionary -> IO Dictionary
dictGUI dic = do
  Gtk.init Nothing 

  (lwWidget, lw) <- newLangWin

  win <- new Gtk.Window [ #title := "Dictionary Look-up" ]
  on win #destroy Gtk.mainQuit

  searchEntry <- new Gtk.Entry [ #text := "search word" ]

  searchButton <- new Gtk.Button [ #label := "look-up" ]
  on searchButton #clicked (do
    wText <- #getText searchEntry
    let word = Text.unpack wText
    let luWord = dicLookup word dic
    case luWord of
      Nothing -> do
        clearLW lw
        #insertText (lwWord lw) 0 (Text.pack "not in dictionary") (-1)
        return ()
      Just entry -> displayEntry lw entry
    )

  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]

  #add box searchEntry
  #add box searchButton
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

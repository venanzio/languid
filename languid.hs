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

-- A data structure containing all the widgets needed by languid
data LangWin = LangWin {
    lwWord :: Gtk.Entry
  , lwPronunciation :: Gtk.Entry
  , lwUsage :: Gtk.TextView
  , lwTranslation :: Gtk.TextView
  , lwPhrase :: Gtk.TextView
  , lwNote :: Gtk.TextView
  , lwRChecks :: Gtk.SpinButton
  , lwWChecks :: Gtk.SpinButton  
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
newLangWin :: IO (Gtk.Box, LangWin)
newLangWin = do
  wordEntry <- new Gtk.Entry [ #text := "word" ]

  pronEntry <- new Gtk.Entry [ #text := "pronunciation" ]

  usageText <- simpleTextView "grammar and usage"

  transText <- simpleTextView "translation"

  phraseText <- simpleTextView "example phrases"

  noteText <- simpleTextView "note"

  -- putting all widgets inside a box
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #setSpacing box 10
  #setMarginStart box 10
  #setMarginEnd box 10
  #setMarginTop box 10
  #setMarginBottom box 10
  
  #add box wordEntry
  #add box pronEntry

  usageFrame <- simpleFrameTV usageText "usage"
  #add box usageFrame

  transFrame <- simpleFrameTV transText "translation"
  #add box transFrame

  phraseFrame <- simpleFrameTV phraseText "example phrases"
  #add box phraseFrame

  noteFrame <- simpleFrameTV noteText "notes"
  #add box noteFrame

  rchecksButton <- simpleSB checksNum
  #add box rchecksButton

  wchecksButton <- simpleSB checksNum
  #add box wchecksButton

  return (box, LangWin { lwWord = wordEntry
                       , lwPronunciation = pronEntry
                       , lwTranslation = transText
                       , lwUsage = usageText
                       , lwPhrase = phraseText
                       , lwNote = noteText
                       , lwRChecks = rchecksButton
                       , lwWChecks = wchecksButton
                       })

-- delete all content in a LangWin structure
clearLW :: LangWin -> IO ()
clearLW lw = do
  clearEntry (lwWord lw)
  clearEntry (lwPronunciation lw)
  clearVText (lwUsage lw)
  clearVText (lwTranslation lw)
  clearVText (lwPhrase lw)
  clearVText (lwNote lw)
  setSimpleSB (lwRChecks lw) checksNum
  setSimpleSB (lwWChecks lw) checksNum

-- puts an entry into a LangWin structure (displaying all the fields)
displayEntry :: LangWin -> DEntry -> IO ()
displayEntry lw entry = do
  clearLW lw
  writeEntry (lwWord lw)          (deWord entry)
  writeEntry (lwPronunciation lw) (dePronunciation entry)
  writeVText (lwUsage lw)         (deUsage entry)
  writeVText (lwTranslation lw)   (deTranslation entry)
  writeVText (lwPhrase lw)        (dePhrase entry)
  writeVText (lwNote lw)          (deNote entry)
  setSimpleSB (lwRChecks lw)      (deRChecks entry)
  setSimpleSB (lwWChecks lw)      (deWChecks entry)

-- Look Up
dictGUI :: Dictionary -> IO Dictionary
dictGUI dic = do
  Gtk.init Nothing 

  (lwWidget, lw) <- newLangWin

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

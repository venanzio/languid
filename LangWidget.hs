{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{- Definition of a data structure LangWidget that contains
   elements to display and input all fields of a DEntry.
-}

module LangWidget where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as Text
import System.Environment

import Dictionary
import EasyWidgets

-- A data structure containing all the widgets needed by languid
data LangWidget = LangWidget {
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
  
-- Creates a new LangWidget, puts it inside a grid and returns both
newLangWidget :: IO (Gtk.Box, LangWidget)
newLangWidget = do
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

  rchecksBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  #setSpacing rchecksBox 10
  rchecksLabel <- new Gtk.Label [ #label := "read checks: " ]
  rchecksButton <- simpleSB checksNum
  #add rchecksBox rchecksLabel
  #add rchecksBox rchecksButton

  wchecksBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  #setSpacing wchecksBox 10
  wchecksLabel <- new Gtk.Label [ #label := "write checks:" ]
  wchecksButton <- simpleSB checksNum
  #add wchecksBox wchecksLabel
  #add wchecksBox wchecksButton

  #add box rchecksBox 
  #add box wchecksBox 

  return (box,
          LangWidget { lwWord = wordEntry
                     , lwPronunciation = pronEntry
                     , lwTranslation = transText
                     , lwUsage = usageText
                     , lwPhrase = phraseText
                     , lwNote = noteText
                     , lwRChecks = rchecksButton
                     , lwWChecks = wchecksButton
                     })

-- delete all content in a LangWidget structure
clearLW :: LangWidget -> IO ()
clearLW lw = do
  clearEntry (lwWord lw)
  clearEntry (lwPronunciation lw)
  clearVText (lwUsage lw)
  clearVText (lwTranslation lw)
  clearVText (lwPhrase lw)
  clearVText (lwNote lw)
  setSimpleSB (lwRChecks lw) checksNum
  setSimpleSB (lwWChecks lw) checksNum

-- puts an entry into a LangWidget structure (displaying all the fields)
displayEntry :: LangWidget -> DEntry -> IO ()
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


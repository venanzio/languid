{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

{- Interactive GUI to lookup words in a dictionary,
   modify them, add new words.
-}

module LookUpGUI where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as Text
import System.Environment
import Data.IORef

import Dictionary
import LookUp
import EasyWidgets
import LangWidget

data LanguageMode = LookUp | ReadTest | WriteTest

data LookUpWidget = LookUpWidget {
    luwDictionary :: IORef Dictionary
  , luwMode :: LanguageMode
  , luwMessage :: Gtk.Label
  , luwInput :: Gtk.Entry
  , luwSubmit :: Gtk.Button
  , luwChange :: Gtk.Button  -- button to modify/add an entry
  }

newLUW :: Dictionary -> IO (Gtk.Box, LookUpWidget)
newLUW dictionary = do
  (lwWidget, lw) <- newLangWidget
  
  dicRef <- newIORef dictionary

  let mode = LookUp  -- TO DO: add a radio button to choose the mode

  msgLabel <- new Gtk.Label [ #label := "enter a word to search" ]
  
  searchEntry <- new Gtk.Entry [ #text := "search word" ]

  searchButton <- new Gtk.Button [ #label := "look-up" ]

  changeButton <- new Gtk.Button [ #label := "Enter/Modify Entry in Dictionary" ]


  on searchButton #clicked (do
    dic <- readIORef dicRef
    word <- readEntry searchEntry
    let luWord = dicLookup word dic
    case luWord of
      Nothing -> do
        set msgLabel [ #label := "not in dictionary"]
        clearLW lw
      Just entry -> do
        set msgLabel [ #label := "found in dictionary"]
        lwDisplayEntry lw entry
    )

  on changeButton #clicked (do
    entry <- lwReadEntry lw
    modifyIORef dicRef (\dic -> updateDictionary dic entry)
    )

  -- layout

  searchBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ] 
  #setSpacing searchBox 10
  #setMarginStart searchBox 10
  #setMarginEnd searchBox 10
  #setMarginTop searchBox 10
  #setMarginBottom searchBox 10

  #add searchBox searchEntry
  #add searchBox searchButton


  luBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #setSpacing luBox 10
  #setMarginTop luBox 10
  #add luBox msgLabel
  #add luBox searchBox

  sep <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal ]

  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #setSpacing box 20

  #add box luBox
  #add box sep
  #add box lwWidget


  sep' <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal ]
  #add box sep'
  #add box changeButton

  return (box,
          LookUpWidget { luwDictionary = dicRef
                       , luwMode = mode
                       , luwMessage = msgLabel
                       , luwInput = searchEntry
                       , luwSubmit = searchButton
                       , luwChange = changeButton
                       }
         )

  
-- Look Up
lookUpGUI :: Dictionary -> IO Dictionary
lookUpGUI dictionary = do
  Gtk.init Nothing 

  (luWidget, lu) <- newLUW dictionary

  win <- new Gtk.Window [ #title := "Dictionary Look-up" ]
  #setDefaultSize win 400 (-1)
  on win #destroy Gtk.mainQuit

  #add win luWidget

  #showAll win

  Gtk.main

  dic <- readIORef (luwDictionary lu)
  return dic


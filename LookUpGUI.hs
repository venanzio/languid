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

data LookUpWidget = LookUpWidget {
    luwInput :: Gtk.Entry
  , luwSubmit :: Gtk.Button
  , luwMessage :: Gtk.Label
  }

newLUW :: IORef Dictionary -> LangWidget -> IO (Gtk.Box, LookUpWidget)
newLUW dicRef lw = do
  msgLabel <- new Gtk.Label [ #label := "enter a word to search" ]
  
  searchEntry <- new Gtk.Entry [ #text := "search word" ]

  searchButton <- new Gtk.Button [ #label := "look-up" ]
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

  return (luBox,
          LookUpWidget { luwInput = searchEntry
                       , luwSubmit = searchButton
                       , luwMessage = msgLabel
                       }
         )

  
-- Look Up
lookUpGUI :: Dictionary -> IO Dictionary
lookUpGUI dictionary = do
  dicRef <- newIORef dictionary
  
  Gtk.init Nothing 

  (lwWidget, lw) <- newLangWidget
  (luWidget, lu) <- newLUW dicRef lw

  win <- new Gtk.Window [ #title := "Dictionary Look-up" ]
  #setDefaultSize win 400 (-1)
  on win #destroy Gtk.mainQuit

  sep <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal ]

  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  #setSpacing box 20

  #add box luWidget
  #add box sep
  #add box lwWidget

  -- Button to enter a new or modified entry into the dictionary
  changeButton <- new Gtk.Button [ #label := "Enter/Modify Entry in Dictionary" ]
  on changeButton #clicked (do
    entry <- lwReadEntry lw
    modifyIORef dicRef (\dic -> updateDictionary dic entry)
    )

  sep' <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal ]
  #add box sep'
  #add box changeButton

  #add win box

  #showAll win

  Gtk.main

  dic <- readIORef dicRef
  return dic


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
import Tests
import EasyWidgets
import LangWidget


data LanguageMode = LookUp | ReadTest | WriteTest

data LookUpWidget = LookUpWidget {
    luwDictionary :: IORef Dictionary
  , luwMode :: IORef LanguageMode
  , luwChangeMode :: Gtk.Button
  , luwMessage :: Gtk.Label
  , luwInput :: Gtk.Entry
  , luwSubmit :: Gtk.Button
  , luwChange :: Gtk.Button  -- button to modify/add an entry
  }

newLUW :: Dictionary -> IO (Gtk.Box, LookUpWidget)
newLUW dictionary = do
  (lwWidget, lw) <- newLangWidget
  
  dicRef <- newIORef dictionary

  modeRef <- newIORef LookUp

  chModeButton <- new Gtk.Button [ #label := "mode: look-up" ]
  
  msgLabel <- new Gtk.Label [ #label := "enter a word to search" ]
  
  searchEntry <- new Gtk.Entry [ #text := "search word" ]

  searchButton <- new Gtk.Button [ #label := "look-up" ]

  changeButton <- new Gtk.Button [ #label := "Enter/Modify Entry in Dictionary" ]

  let lu = LookUpWidget { luwDictionary = dicRef
                        , luwMode = modeRef
                        , luwChangeMode = chModeButton
                        , luwMessage = msgLabel
                        , luwInput = searchEntry
                        , luwSubmit = searchButton
                        , luwChange = changeButton
                        }

  luActions lw lu

  on chModeButton #clicked (do
    mode <- readIORef modeRef
    case mode of
      LookUp -> rtActions lw lu
      ReadTest -> wtActions lw lu
      WriteTest -> luActions lw lu
    )
    

  on changeButton #clicked (do
    entry <- lwReadEntry lw
    modifyIORef dicRef (\dic -> updateDictionary dic entry)
    )

  -- layout

  modeBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ] 
  #setSpacing modeBox 10
  #setMarginStart modeBox 10
  #setMarginEnd modeBox 10
  #setMarginTop modeBox 10
  #setMarginBottom modeBox 10

  #add modeBox chModeButton
  #add modeBox msgLabel

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
  #add luBox modeBox
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

  return (box,lu)

luActions :: LangWidget -> LookUpWidget -> IO ()
luActions lw lu = do
  set (luwChangeMode lu) [ #label := "mode: look-up" ]
  writeIORef (luwMode lu) LookUp
  
  on (luwSubmit lu) #clicked (do
    dic <- readIORef (luwDictionary lu)
    word <- readEntry (luwInput lu)
    let luEntry = dicLookup word dic
    case luEntry of
      Nothing -> do
        set (luwMessage lu) [ #label := "not in dictionary"]
        clearLW lw
      Just entry -> do
        set (luwMessage lu) [ #label := "found in dictionary"]
        lwDisplayEntry lw entry
    )
  return ()

rtActions :: LangWidget -> LookUpWidget -> IO ()
rtActions lw lu = do
  set (luwChangeMode lu) [ #label := "mode: read test" ]
  writeIORef (luwMode lu) ReadTest

  dic <- readIORef (luwDictionary lu)
  e <- randREntry dic

  writeEntry (lwWord lw) (deWord e)
  writeEntry (lwPronunciation lw) (dePronunciation e)

  set (luwMessage lu) [ #label := "Guess the translation" ]

  on (luwSubmit lu) #clicked (do
    tr <- readEntry (luwInput lu)
    if (checkTranslation e tr)
      then set (luwMessage lu) [ #label := "Correct!" ]
      else set (luwMessage lu) [ #label := "Wrong!" ]
    )

  return ()

wtActions :: LangWidget -> LookUpWidget -> IO ()
wtActions lw lu = do
  set (luwChangeMode lu) [ #label := "mode: write test" ]
  writeIORef (luwMode lu) WriteTest
  return ()

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


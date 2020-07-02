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


data LanguageMode = LookUp
                   | ReadTest DEntry | NextRTest
                   | WriteTest DEntry | NextWTest

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
  
  searchEntry <- new Gtk.Entry [ #text := "" ]

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
  
  on chModeButton #clicked (do
    clearLW lw
    mode <- readIORef modeRef
    case mode of
      LookUp      -> setRTMode lu
      ReadTest  _ -> setWTMode lu
      NextRTest   -> setWTMode lu
      WriteTest _ -> setLUMode lu
      NextWTest   -> setLUMode lu
    )

  -- pressing enter on the input entry or clicking the search button are equivalent
  on searchEntry #activate (do
    mode <- readIORef modeRef
    case mode of
      LookUp      -> luAction lw lu
      NextRTest   -> nextRTAction lw lu
      ReadTest  e -> rtAction lw lu e
      NextWTest   -> nextWTAction lw lu
      WriteTest e -> wtAction lw lu e
    )

  on searchButton #clicked (do
    mode <- readIORef modeRef
    case mode of
      LookUp      -> luAction lw lu
      NextRTest   -> nextRTAction lw lu
      ReadTest  e -> rtAction lw lu e
      NextWTest   -> nextWTAction lw lu
      WriteTest e -> wtAction lw lu e
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
  
setLUMode lu = do
  writeIORef (luwMode lu) LookUp
  set (luwChangeMode lu) [ #label := "mode: look-up" ]
  writeEntry (luwInput lu) ""
  set (luwMessage lu) [ #label := "enter a word to search" ]
  set (luwSubmit lu) [ #label := "look-up" ]
  return ()
    
setRTMode lu = do
  writeIORef (luwMode lu) NextRTest
  set (luwChangeMode lu) [ #label := "mode: read test" ]
  set (luwMessage lu) [ #label := "guess the translation" ]
  writeEntry (luwInput lu) ""
  set (luwSubmit lu) [ #label := "next word" ]
  return ()

setWTMode lu = do
  writeIORef (luwMode lu) NextWTest
  set (luwChangeMode lu) [ #label := "mode: write test" ]
  set (luwMessage lu) [ #label := "guess the word" ]
  writeEntry (luwInput lu) ""
  set (luwSubmit lu) [ #label := "next word" ]
  return ()

luAction :: LangWidget -> LookUpWidget -> IO ()
luAction lw lu = do
  dic <- readIORef (luwDictionary lu)
  word <- readEntry (luwInput lu)
  clearEntry (luwInput lu)
  let luEntry = dicLookup word dic
  case luEntry of
    Nothing -> do
      set (luwMessage lu) [ #label := "not in dictionary"]
      clearLW lw
      writeEntry (lwWord lw) word
    Just entry -> do
      set (luwMessage lu) [ #label := "found in dictionary"]
      lwDisplayEntry lw entry

nextRTAction :: LangWidget -> LookUpWidget -> IO ()
nextRTAction lw lu = do
  set (luwChangeMode lu) [ #label := "mode: read test" ]

  dic <- readIORef (luwDictionary lu)
  e <- randREntry dic
  writeIORef (luwMode lu) (ReadTest e)

  clearLW lw
  clearEntry (luwInput lu)
  
  writeEntry (lwWord lw) (deWord e)
  writeEntry (lwPronunciation lw) (dePronunciation e)
  setSimpleSB (lwRChecks lw) (deRChecks e)
  setSimpleSB (lwWChecks lw) (deWChecks e)

  set (luwMessage lu) [ #label := "Guess the translation" ]

  set (luwSubmit lu) [ #label := "submit" ]

rtAction :: LangWidget -> LookUpWidget -> DEntry -> IO ()
rtAction lw lu e = do
  tr <- readEntry (luwInput lu)

  newRChecks <- if (checkTranslation e tr)
    then (set (luwMessage lu) [ #label := "Correct!" ] >> return (deRChecks e - 1))
    else (set (luwMessage lu) [ #label := "Wrong!" ] >> return (deRChecks e + 1))

  let e' = updateRChecks e newRChecks
  lwDisplayEntry lw e'
  modifyIORef (luwDictionary lu) (\dic -> updateDictionary dic e')

  set (luwSubmit lu) [ #label := "next word" ]
  writeIORef (luwMode lu) NextRTest

nextWTAction :: LangWidget -> LookUpWidget -> IO ()
nextWTAction lw lu = do
  set (luwChangeMode lu) [ #label := "mode: write test" ]

  dic <- readIORef (luwDictionary lu)
  e <- randWEntry dic
  writeIORef (luwMode lu) (WriteTest e)

  clearLW lw
  clearEntry (luwInput lu)
  
  writeVText (lwTranslation lw) (deTranslation e)
  setSimpleSB (lwRChecks lw) (deRChecks e)
  setSimpleSB (lwWChecks lw) (deWChecks e)

  set (luwMessage lu) [ #label := "Guess word" ]

  set (luwSubmit lu) [ #label := "submit" ]

wtAction :: LangWidget -> LookUpWidget -> DEntry -> IO ()
wtAction lw lu e = do
  tr <- readEntry (luwInput lu)

  newWChecks <- if (tr == deWord e)
    then (set (luwMessage lu) [ #label := "Correct!" ] >> return (deWChecks e - 1))
    else (set (luwMessage lu) [ #label := "Wrong!" ] >> return (deWChecks e + 1))

  let e' = updateWChecks e newWChecks
  lwDisplayEntry lw e'
  modifyIORef (luwDictionary lu) (\dic -> updateDictionary dic e')

  set (luwSubmit lu) [ #label := "next word" ]
  writeIORef (luwMode lu) NextWTest


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


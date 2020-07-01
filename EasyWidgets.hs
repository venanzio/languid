{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

-- Simplified functions on widgets

module EasyWidgets where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import qualified Data.Text as Text

-- Delete the content of an Entry

clearEntryBuffer :: Gtk.EntryBuffer -> IO ()
clearEntryBuffer buff = do
  #deleteText buff 0 (-1)
  return ()

clearEntry :: Gtk.Entry -> IO ()
clearEntry e = Gtk.entryGetBuffer e >>= clearEntryBuffer

-- Read the content of an Entry

readEntryBuffer :: Gtk.EntryBuffer -> IO String
readEntryBuffer buff = do
   txt <- #getText buff
   return (Text.unpack txt)

readEntry :: Gtk.Entry -> IO String
readEntry e = Gtk.entryGetBuffer e >>= readEntryBuffer

-- Write a string inside an Entry

writeEntryBuffer :: Gtk.EntryBuffer -> String -> IO ()
writeEntryBuffer buff s = do
  #insertText buff 0 (Text.pack s) (-1)
  return ()

writeEntry :: Gtk.Entry -> String -> IO ()
writeEntry e s = do
  buff <- Gtk.entryGetBuffer e
  clearEntryBuffer buff
  writeEntryBuffer buff s

-- delete the content of a Text View

clearTextBuffer :: Gtk.TextBuffer -> IO ()
clearTextBuffer buff = do
  (start,end) <- Gtk.textBufferGetBounds buff
  #delete buff start end
  return ()

clearVText :: Gtk.TextView -> IO ()
clearVText tv = do
  buff <- get tv #buffer
  clearTextBuffer buff


-- get the whole text contained in a text buffer

readBText :: Gtk.TextBuffer -> IO String
readBText buff = do
    (start,end) <- Gtk.textBufferGetBounds buff
    s <- Gtk.textBufferGetText buff start end False
    return (Text.unpack s)

readVText :: Gtk.TextView -> IO String
readVText tv = do
  buff <- get tv #buffer
  readBText buff
    
writeBText :: Gtk.TextBuffer -> String -> IO ()
writeBText buff s =
  set buff [ #text := Text.pack s ]

writeVText :: Gtk.TextView -> String -> IO ()
writeVText tv s = do
  buff <- get tv #buffer
  writeBText buff s

-- Simple text display and input area, with initialization string

simpleTextView :: String -> IO Gtk.TextView
simpleTextView s = do 
  tv <- new Gtk.TextView [ ]
  writeVText tv s
  #setLeftMargin tv 5
  #setTopMargin tv 5
  #setSizeRequest tv 100 50
  #setWrapMode tv Gtk.WrapModeChar
  return tv

-- simple frame around a text view

simpleFrameTV :: Gtk.TextView -> String -> IO Gtk.Frame
simpleFrameTV tv s = do
  fr <- new Gtk.Frame [ #label := Text.pack s ]
  #add fr tv
  return fr

-- Simple positive Int-reading sping button (values in range 0-100)

simpleSB :: Int -> IO Gtk.SpinButton
simpleSB n = do
  spinB <- new Gtk.SpinButton [ ]
  adjust <- Gtk.adjustmentNew (fromIntegral n) 0 100 1 5 10
  Gtk.spinButtonConfigure spinB (Just adjust) 1 0
  return spinB

setSimpleSB :: Gtk.SpinButton -> Int -> IO ()
setSimpleSB sb n = #setValue sb (fromIntegral n)

readSimpleSB :: Gtk.SpinButton -> IO Int
readSimpleSB sb = do
  x <- #getValueAsInt sb
  return (fromIntegral x)

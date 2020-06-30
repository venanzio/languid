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

getBText :: Gtk.TextBuffer -> IO String
getBText buff = do
    (start,end) <- Gtk.textBufferGetBounds buff
    s <- Gtk.textBufferGetText buff start end False
    return (Text.unpack s)

getVText :: Gtk.TextView -> IO String
getVText tv = do
  buff <- get tv #buffer
  getBText buff
    
writeBText :: Gtk.TextBuffer -> String -> IO ()
writeBText buff s =
  set buff [ #text := Text.pack s ]

writeVText :: Gtk.TextView -> String -> IO ()
writeVText tv s = do
  buff <- get tv #buffer
  writeBText buff s

-- Simple positive Int-reading sping button (values in range 0-100)

simpleSB :: Int -> IO Gtk.SpinButton
simpleSB n = do
  spinB <- new Gtk.SpinButton [ ]
  adjust <- Gtk.adjustmentNew (fromIntegral n) 0 100 1 5 10
  Gtk.spinButtonConfigure spinB (Just adjust) 1 0
  return spinB

setSimpleSB :: Gtk.SpinButton -> Int -> IO ()
setSimpleSB sb n = #setValue sb (fromIntegral n)

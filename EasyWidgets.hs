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

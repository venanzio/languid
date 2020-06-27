-- Dictionary Drill Program, GUI version
-- Venanzio Capretta, August 2011
-- version 0.8

-- Remaining bugs

-- When modifying and entry, if the word part is changed a new entry is created, instead of the old one being modified.
-- In testing modes, after answering a question we can answer it again as many times as we want before pressing Clear to change the word. 
-- To do: select dictionary file.
-- Update statistics continuously, not just on save

import LanguageDictionary
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Graphics.UI.Gtk.Abstract.Widget
-- import Graphics.UI.Gtk.Glade
import Data.IORef
import System.IO
import System.Environment
import Control.Monad
import System.Random (randomRIO)

red    = Color 65535 50000 50000
green  = Color 50000 65535 50000
grey   = Color 60000 60000 60000
blue   = Color 50000 60000 65535
yellow = Color 65535 65535 50000

data LanguageMode = SearchMode | ReadingTest | WritingTest

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

-- Get all the widgets from the glade file

getLanguageGUI :: FilePath -> Dictionary -> IO (LanguageGUI)
getLanguageGUI gladefile dictionary = do
  windowXml <- builderNew
  builderAddFromFile windowXml gladefile

  window <- builderGetObject windowXml castToWindow "window"
  inputEntry <- builderGetObject windowXml castToEntry "input"
  messageLabel <- builderGetObject windowXml castToLabel "message"    
  wordEntry <- builderGetObject windowXml castToEntry "word"
  pronunciationEntry <- builderGetObject windowXml castToEntry "pronunciation"
  grammarEntry <- builderGetObject windowXml castToEntry "grammar"  
  translationTextView <- builderGetObject windowXml castToTextView "translation"
  translationBuffer <- textViewGetBuffer translationTextView
  phraseTextView <- builderGetObject windowXml castToTextView "phrase"
  phraseBuffer <- textViewGetBuffer phraseTextView
  noteTextView <- builderGetObject windowXml castToTextView "note"
  noteBuffer <- textViewGetBuffer noteTextView
  wordnumLabel <- builderGetObject windowXml castToLabel "wordnum"
  readnumLabel <- builderGetObject windowXml castToLabel "readnum"
  writenumLabel <- builderGetObject windowXml castToLabel "writenum"
  checksRSpin <- builderGetObject windowXml castToSpinButton "rchecks"
  checksWSpin <- builderGetObject windowXml castToSpinButton "wchecks"
  okButton <- builderGetObject windowXml castToButton "ok"
  modifyButton <- builderGetObject windowXml castToButton "modify"
  clearButton <- builderGetObject windowXml castToButton "clear"
  saveButton <- builderGetObject windowXml castToButton  "save"
  modeBox <- builderGetObject windowXml castToComboBox "mode"
  -- Set the fonts
  let font = "文泉驿等宽微米黑" 
      -- "文泉驿微米黑", "文泉驛等寬微米黑", "東風ゴシック" 
      -- "TakaoPMincho",  "TakaoPGothic", "TakaoExGothic" 
      -- "Sazanami Mincho", "Droid Sans Japanese"
      -- "TakaoMincho", "Japan", "Kochi Gothic"
  wordFont <- fontDescriptionFromString (font++" 22")
  widgetModifyFont wordEntry (Just wordFont)
  widgetModifyFont pronunciationEntry (Just wordFont)
  widgetModifyFont inputEntry (Just wordFont)
  textFont <- fontDescriptionFromString (font++" 18")
  widgetModifyFont grammarEntry (Just textFont)
  widgetModifyFont translationTextView (Just textFont)
  widgetModifyFont phraseTextView (Just textFont)
  widgetModifyFont noteTextView (Just textFont)
  -- mutable references for dictionary and entry
  dicref <- newIORef dictionary
  entryref <- newIORef (head dictionary)
  moderef <- newIORef SearchMode
  -- return the whole thing
  return (LanguageGUI window inputEntry messageLabel
            wordEntry pronunciationEntry grammarEntry 
            translationBuffer phraseBuffer noteBuffer
            wordnumLabel readnumLabel writenumLabel
            checksRSpin checksWSpin okButton modifyButton
            clearButton saveButton modeBox
            dicref entryref moderef)

showDicStatistics :: LanguageGUI -> Dictionary -> IO ()
showDicStatistics langui dictionary = do
  labelSetText (wordnumLabel langui) ("T: " ++ show (length dictionary))
  labelSetText (readnumLabel langui) ("R: " ++ show (totalRWords dictionary))
  labelSetText (writenumLabel langui) ("W: " ++ show (totalWWords dictionary))

clearDEntry :: LanguageGUI -> IO ()
clearDEntry langui = do      
  widgetModifyBg (window langui) StateNormal grey
  labelSetText (messageLabel langui) "enter a word above" 
  entrySetText (wordEntry langui) ""
  entrySetText (pronunciationEntry langui) ""
  entrySetText (grammarEntry langui) ""
  textBufferSetText (translationBuffer langui) ""
  textBufferSetText (phraseBuffer langui) ""
  textBufferSetText (noteBuffer langui) ""
  spinButtonSetValue (checksRSpin langui) 5
  spinButtonSetValue (checksWSpin langui) 0
  widgetGrabFocus (inputEntry langui)
  
updateDicRef :: LanguageGUI -> IO ()
updateDicRef langui = do
  e <- readIORef (entryref langui)
  modifyIORef (dicref langui) (flip updateDictionary e)
  
showDEntry :: LanguageGUI -> DEntry -> IO () 
showDEntry langui e = do
  entrySetText (wordEntry langui) (getWord e)
  entrySetText (pronunciationEntry langui) (pronunciation e)
  entrySetText (grammarEntry langui) (usage e)
  textBufferSetText (translationBuffer langui) (translation e)
  textBufferSetText (phraseBuffer langui) (phrases e)
  textBufferSetText (noteBuffer langui) (note e)
  spinButtonSetValue (checksRSpin langui) (fromIntegral $ readChecks e)
  spinButtonSetValue (checksWSpin langui) (fromIntegral $ writeChecks e)  

searchDEntry :: LanguageGUI -> IO () 
searchDEntry langui = do
  dic <- readIORef (dicref langui)
  clearDEntry langui
  searchWord <- entryGetText (inputEntry langui)
  entrySetText (inputEntry langui) ""
  let mentry = dicLookup searchWord dic
  case mentry of
    Nothing -> do labelSetText (messageLabel langui) "NOT FOUND" 
                  entrySetText (wordEntry langui) searchWord
                  widgetModifyBg (window langui) StateNormal yellow
    Just e  -> do labelSetText (messageLabel langui) "FOUND" 
                  showDEntry langui e
                  widgetModifyBg (window langui) StateNormal blue

getDEntry :: LanguageGUI -> IO (DEntry)
getDEntry langui = do
  word <- entryGetText (wordEntry langui)
  pronunciation <- entryGetText (pronunciationEntry langui)
  usage <- entryGetText (grammarEntry langui)
  start <- textBufferGetStartIter (translationBuffer langui)
  end <- textBufferGetEndIter (translationBuffer langui)
  translation <- textBufferGetText (translationBuffer langui) start end True
  start <- textBufferGetStartIter (phraseBuffer langui)
  end <- textBufferGetEndIter (phraseBuffer langui)
  phrases <- textBufferGetText (phraseBuffer langui) start end True
  start <- textBufferGetStartIter (noteBuffer langui)
  end <- textBufferGetEndIter (noteBuffer langui)
  note <- textBufferGetText (noteBuffer langui) start end True
  rchecks <- spinButtonGetValueAsInt (checksRSpin langui)
  wchecks <- spinButtonGetValueAsInt (checksWSpin langui)
  return (DEntry word [Pronunciation pronunciation,
                       Usage usage,
                       Translation translation,
                       Phrase phrases,
                       Note note,
                       RChecks rchecks,
                       WChecks wchecks])

saveDictionary :: FilePath -> LanguageGUI -> IO ()
saveDictionary dictionaryFile langui = do
  dic <- readIORef (dicref langui)
  writeDictionary dictionaryFile dic
  labelSetText (messageLabel langui) "dictionary saved"
  showDicStatistics langui dic

-- Chooses a random entry for reading test (with weights from RChecks)
randREntry :: LanguageGUI -> IO ()
randREntry langui = do
  dic <- readIORef (dicref langui)
  clearDEntry langui
  entrySetText (inputEntry langui) ""
  let rchecks = totalRChecks dic
  ne <- randomRIO (1::Int,max rchecks totalChecks)
  e <- if ne > rchecks
       then do labelSetText (messageLabel langui) "jolly word"
               randomEntry dic
       else do labelSetText (messageLabel langui) "guess word"
               return (nthReadEntry dic ne)
  writeIORef (entryref langui) e 
  entrySetText (wordEntry langui) (getWord e)
  entrySetText (pronunciationEntry langui) (pronunciation e)
  entrySetText (grammarEntry langui) (usage e)
  spinButtonSetValue (checksRSpin langui) (fromIntegral $ readChecks e)
  spinButtonSetValue (checksWSpin langui) (fromIntegral $ writeChecks e)
  widgetGrabFocus (inputEntry langui)

correctMsg :: LanguageGUI -> Bool -> IO ()
correctMsg langui b = do
  widgetModifyBg (window langui) StateNormal (if b then green else red)
  labelSetText (messageLabel langui) (if b then "CORRECT" else "WRONG")

checkRAnswer :: LanguageGUI -> IO ()
checkRAnswer langui = do
  e <- readIORef (entryref langui)
  answer <- entryGetText (inputEntry langui)
  let b = checkTranslation e answer
  correctMsg langui b
  let c = readChecks e
      wc = writeChecks e
      c'  = if b then if c==0 then 0 else c-1
                 else if c==0 then checksNum
                              else c+1
      wc' = if c==0 || c'>0 then wc
                            else wc + checksNum
      e' = updateWChecks (updateRChecks e c') wc'
  writeIORef (entryref langui) e'
  updateDicRef langui
  showDEntry langui e'
  widgetGrabFocus (clearButton langui)

randWEntry :: LanguageGUI -> IO ()
randWEntry langui = do
  dic <- readIORef (dicref langui)
  (clearDEntry langui)
  entrySetText (inputEntry langui) ""
  let wchecks = totalWChecks dic
  ne <- randomRIO (1::Int, wchecks)
  e <- return (nthWriteEntry dic ne)
  labelSetText (messageLabel langui) 
               ("guess "++(if pronunciation e=="" then "  kana" else "  kanji"))
  writeIORef (entryref langui) e 
  textBufferSetText (translationBuffer langui) (translation e)
  entrySetText (grammarEntry langui) (usage e)
  spinButtonSetValue (checksRSpin langui) (fromIntegral $ readChecks e)
  spinButtonSetValue (checksWSpin langui) (fromIntegral $ writeChecks e)      
  widgetGrabFocus (inputEntry langui)
                   
checkWAnswer :: LanguageGUI -> IO ()
checkWAnswer langui = do
  e <- readIORef (entryref langui)
  answer <- entryGetText (inputEntry langui)
  let b = answer == (getWord e)
  correctMsg langui b
  let c = writeChecks e
      c' = if b then c-1 else c+1
      e' = updateWChecks e c'
  writeIORef (entryref langui) e'
  updateDicRef langui
  showDEntry langui e'
  widgetGrabFocus (clearButton langui)
 
setSearchMode :: LanguageGUI -> IO ()
setSearchMode langui = do
  labelSetText (messageLabel langui) "search mode"
  writeIORef (moderef langui) SearchMode  
  return ()

setRTestMode :: LanguageGUI -> IO ()      
setRTestMode langui = do
  labelSetText (messageLabel langui) "reading test"
  writeIORef (moderef langui) ReadingTest  
  return ()

setWTestMode :: LanguageGUI -> IO ()
setWTestMode langui = do
  labelSetText (messageLabel langui) "writing test"
  writeIORef (moderef langui) WritingTest
  return ()

setMode :: FilePath -> LanguageGUI -> IO ()
setMode dictionaryFile langui = do
  m <- comboBoxGetActive (modeBox langui)
  entrySetText (inputEntry langui) ""
  clearDEntry langui  
  saveDictionary dictionaryFile langui
  case m of
    0 -> setRTestMode langui
    1 -> setWTestMode langui 
    2 -> setSearchMode langui
    _ -> labelSetText (messageLabel langui) "UNKNOWN MODE SELECTION"     
  
main :: IO()
main = do
  -- read the dictionary and store it in a mutable variable
  dictionaryFile <- liftM head getArgs
  let dictionaryFileTemp = dictionaryFile++".bak"
  dictionary <- readDictionary dictionaryFile
  writeDictionary dictionaryFileTemp dictionary
  
  -- start the GUI
  initGUI
  
  -- Get all the widgets from the glade file
  langui <- getLanguageGUI "language.glade" dictionary
  showDicStatistics langui dictionary

  -- Define the actions
  onDestroy (window langui) $ do
    saveDictionary dictionaryFile langui   
    mainQuit
  
  onClicked (modifyButton langui) $ do
    e <- getDEntry langui
    modifyIORef (dicref langui) (flip updateDictionary e)
    labelSetText (messageLabel langui) "entry added/modified"
  
  onClicked (clearButton langui) $ do
    mode <- readIORef (moderef langui)
    case mode of
      SearchMode  -> clearDEntry langui
      ReadingTest -> randREntry langui
      WritingTest -> randWEntry langui

  onClicked (okButton langui) $ do
    mode <- readIORef (moderef langui)
    case mode of
      SearchMode  -> searchDEntry langui
      ReadingTest -> checkRAnswer langui
      WritingTest -> checkWAnswer langui 
  
  onEntryActivate (inputEntry langui) (buttonClicked $ okButton langui)

  on (modeBox langui) changed (setMode dictionaryFile langui)  
  
  onClicked (saveButton langui) (saveDictionary dictionaryFile langui)

  -- activate everything and start the main GUI
  widgetShowAll (window langui)
  mainGUI

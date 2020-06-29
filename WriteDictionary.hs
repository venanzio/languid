module WriteDictionary where

import Dictionary

-- WRITING DICTIONARY IN XML FORMAT

xmlDEntry :: DEntry -> String
xmlDEntry (DEntry w l) = unlines
  (["<entry>","<word>"++w++"</word>"] ++ map xmlDEntryField l ++
   ["</entry>"])
  where xmlDEntryField (Usage u) = if u=="" then "" else "<usage>"++u++"</usage>"
        xmlDEntryField (Pronunciation p) = if p=="" then "" else "<pronunciation>"++p++"</pronunciation>"
        xmlDEntryField (Translation t) = "<translation>"++t++"</translation>"
        xmlDEntryField (Phrase p) = if p=="" then "" else "<phrase>"++p++"</phrase>"
        xmlDEntryField (Note n) = if n=="" then "" else "<note>"++n++"</note>"
        xmlDEntryField (RChecks k) = if k==0 then "" else "<rchecks>"++(show k)++"</rchecks>"
        xmlDEntryField (WChecks k) = if k==0 then "" else "<wchecks>"++(show k)++"</wchecks>"

xmlDictionary :: Dictionary -> String
xmlDictionary d = "<dictionary>\n"++
                  (brklines (map xmlDEntry d)) ++
                  "\n</dictionary>"

writeDictionary :: String -> Dictionary -> IO ()
writeDictionary file d = do
  writeFile file (xmlDictionary d)





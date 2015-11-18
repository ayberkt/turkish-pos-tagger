{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Parse where

import Prelude hiding (words)
import Text.XML.HXT.Core
-- | These are the possible parts-of-speech described in the
-- | METU-Sabanci treebank paper.
data POS = Noun
         | Adj
         | Adv
         | Verb
         | Pron
         | Conj
         | Det
         | Postp
         | Ques
         | Interj
         | Num
         | Dup
         | Punc
         deriving (Eq, Show, Read, Ord, Enum)

extractPOS :: String -> POS
extractPOS x = read $ takeWhile notPlusNorQuote . tail $ dropWhile notPlus x
  where notPlus = (/= '+')
        notPlusNorQuote = \n -> notPlus n && n /= '"'

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

words :: ArrowXml cat => cat XmlTree (String, String)
words = atTag "S" >>>
  proc x -> do
    w       <- atTag "W" -< x
    word    <- getText <<< getChildren -< w
    tagStr  <- getAttrValue "IG" -< w
    returnA -< (word, tagStr)

printTuple :: (String, String) -> IO ()
printTuple (a, b)  = putStrLn (a ++ " " ++ (show $ extractPOS b))

-- | Given number of files `numFiles` takes out all word-tag pairs
-- | from each file indexed from 0 .. `numFiles`, and concatenates
-- | them.
getWords :: FilePath -> IOSLA (XIOState s) a (String, String)
getWords f = readDocument [withValidate no] f >>> words

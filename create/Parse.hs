{-# LANGUAGE Arrows #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}

module Parse where

import Prelude hiding (words)
import Data.Char (isSpace)
import Text.XML.HXT.Core
-- | These are the possible parts-of-speech described in the
-- | METU-Sabanci treebank paper.
data POS = Start
         | Noun
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
         | Unknown
         deriving (Eq, Show, Read, Ord, Enum)

extractPOS :: String -> POS
extractPOS x = case filter (not . isSpace) wordString of
                    "Noun"   → Noun
                    "Adj"    → Adj
                    "Adv"    → Adv
                    "Verb"   → Verb
                    "Pron"   → Pron
                    "Conj"   → Conj
                    "Det"    → Det
                    "Postp"  → Postp
                    "Ques"   → Ques
                    "Interj" → Interj
                    "Num"    → Num
                    "Dup"    → Dup
                    "Punc"   → Punc
                    _        → Unknown
  where notPlus = (/= '+')
        notPlusNorQuote = \n -> notPlus n && n /= '"'
        wordString = takeWhile notPlusNorQuote . tail $ dropWhile notPlus x

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

parseTupleList ∷ [(String, String)] → [(String, POS)]
parseTupleList xs = map parseTuple xs
  where parseTuple (word, info) = (filter (not . isSpace) word, extractPOS info)

-- | Given number of files `numFiles` takes out all word-tag pairs
-- | from each file indexed from 0 .. `numFiles`, and concatenates
-- | them.
getWords :: FilePath -> IOSLA (XIOState s) a (String, String)
getWords f = readDocument [withValidate no] f >>> words

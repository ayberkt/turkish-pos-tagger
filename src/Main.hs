{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Data.HMM
import System.IO hiding (putStr)
import System.Directory
-- import qualified Data.Vector.Storable as VS
import Text.XML.HXT.Core
import Prelude hiding (words, putStr)

type TaggedWord = (String, POS)

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
         deriving (Eq, Show, Read)

allTags :: [POS]
allTags = [Noun,Adj,Adv,Verb,Pron,Conj,Det,Postp,Ques,Interj,Num,Dup,Punc]

extractPOS :: String -> POS
extractPOS x = read $ takeWhile notPlusNorQuote . tail $ dropWhile notPlus x
  where notPlus = (/= '+')
        notPlusNorQuote = \n -> notPlus n && n /= '"'

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

-- | For all children of <S> (i.e., the words) return a tuple containing
-- | the word and the string containing part-of-speech and morpheme
-- | glossing of the word. We are primarily interested in the part-of-speech
-- | contained by this `tagStr` so we will eventually extract it out using
-- | `extractPOS`.
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
-- getWords :: ArrowXml cat => FilePath -> cat XmlTree (String, String)
getWords f = readDocument [withValidate no] f >>> words

main :: IO ()
main = do
  files <- getDirectoryContents "tb_uni"
  let file = "tb_uni/" ++ files !! 4
      fileNames = drop 2 . take 100 $ map ("tb_uni/" ++) files
  -- pairs <- runX $ readDocument [withValidate no] file >>> words
  pairList <- mapM runX $ map getWords fileNames
  let pairs      = foldr (++) [] pairList
      words      = map fst pairs
      tags       = map (extractPOS . snd) pairs
      myHMM      = simpleHMM allTags []
      -- trainedHmm = baumWelch myHMM words 10
  _ <- print trainedHmm
  return ()

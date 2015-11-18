{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

import Data.HMM
import System.IO hiding (putStr)
import System.Directory
-- import qualified Data.Vector.Storable as VS
import Text.XML.HXT.Core
import Data.Array
import Data.List (elemIndex)
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
         deriving (Eq, Show, Read, Ord, Enum)

allTags :: [POS]
allTags = [Noun,Adj,Adv,Verb,Pron,Conj,Det,Postp,Ques,Interj,Num,Dup,Punc]

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
      -- Now let us "index" each of the words with a
      -- tag index i.e. (fromEnum t) where t ∈ POS.
      pairs'     = zip (map fromEnum tags) words
      pairsArr   = array (0, (length pairs')-1) pairs'
      myHMM      = simpleHMM [0..12] words
      trainedHmm = baumWelch myHMM pairsArr 10
  print trainedHmm
  -- print $ zip words tags
  -- print $ [Noun .. Punc]
  return ()

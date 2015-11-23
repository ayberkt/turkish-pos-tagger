{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Data.HMM (viterbi, HMM(..), saveHMM)
import System.IO
import System.Directory
import Data.Array (listArray, Array)
import Text.XML.HXT.Core
import Prelude hiding (words, putStr)
import qualified Data.Map as M
import Data.Number.LogFloat
import Parse

type ℤ = Int    -- The integer type `Int` will be denoted by ℤ.
type ℚ = Double -- The double type will be denoted by ℚ.

freqMap ∷ Ord a ⇒ [a] → M.Map a ℤ
freqMap unigrams = populate M.empty unigrams
  where populate ∷ Ord a ⇒ M.Map a ℤ → [a] → M.Map a ℤ
        populate m [] = m
        populate m (x:xs) = let freq = (M.findWithDefault 0 x m) ∷ ℤ
                            in populate (M.insert x (freq + 1) m) xs

probability ∷ (Ord a, Ord b) ⇒ (a, b) → M.Map a ℤ → M.Map (a, b) ℤ → ℚ
probability (x, y) c₁ c₂ = if xCount == 0 || yCount == 0
                           then 0.0001
                           else yCount / xCount
  where yCount = fromIntegral (M.findWithDefault 0 (x, y) c₂) ∷ ℚ
        xCount = fromIntegral (M.findWithDefault 0 x c₁) ∷ ℚ

main ∷ IO ()
main = do
  files ← getDirectoryContents "tb_uni"
  let fileNames = drop 2 . take 1500 $ map ("tb_uni/" ++) files
  pairList ← mapM runX $ map getWords fileNames
  let taggedWordsList  = map parseTupleList pairList
      taggedWords      = concat taggedWordsList
      (ws, ts)         = unzip taggedWords
      -- For computing the probability of word-tag pair.
      taggedWordFreqs  = freqMap taggedWords
      wordFreqs        = freqMap ws
      tagFreqs         = freqMap ts
      -- For computing the probability of tag bigram
      tagBigrams       = [(ts !! i, ts !! (i+1)) | i ← [0 .. (length ts)-2]]
      tagBigramFreqs   = freqMap tagBigrams
      transFn s₁ s₂     = logFloat $ probability (s₁, s₂) tagFreqs tagBigramFreqs
      outFn s e        = logFloat $ probability (e, s) wordFreqs taggedWordFreqs
      possibleTags     = [Noun .. Unknown]
      newHMM           = HMM { states      = possibleTags ∷ [POS]
                             , events      = ws ∷ [String]
                             -- TODO: FIX THIS
                             , initProbs   = \_ → logFloat 0.1
                             , transMatrix = transFn
                             , outMatrix   = outFn}
      sampleSentence   ∷ Array Int String
      sampleSentence   = listArray (0, 4) [ "gözleri"
                                          , "kor"
                                          , "gibi"
                                          , "yanıyordu"
                                          , "."]
      sampleSentence₂   = listArray (0, 3) [ "adam"
                                           , "yine"
                                           , "geldi"
                                           , "."
                                           ]
      sampleSentence₃   = listArray (0, 4) [ "güzel"
                                           , "kız"
                                           , "mutlu"
                                           , "gözüküyordu"
                                           , "."]
      sampleSentence₄   = listArray (0, 5) [ "renksiz"
                                           , "yeşil"
                                           , "fikirler"
                                           , "sessizce"
                                           , "uyuyor"
                                           , "."]
  print $ viterbi newHMM sampleSentence₃
  return ()

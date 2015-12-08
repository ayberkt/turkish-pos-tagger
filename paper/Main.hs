{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns  #-}

module Main where

import qualified Data.Map             as M
import           Data.Number.LogFloat
import           Parse
import           Prelude              hiding (putStr, words, lines)
import           System.Directory
import           Text.XML.HXT.Core

type ℤ = Int      -- The integer type `Int` will be denoted by ℤ.
type ℚ = Double   -- The double type will be denoted by ℚ.
type 𝓛 = LogFloat -- Log-domain numbers to prevent underflow.

ε ∷ Double
ε = encodeFloat 1 $ fst  r - ds
  where r  = floatRange (0.1 :: Double)
        ds = floatDigits (0.1 :: Double)

freqMap ∷ Ord a ⇒ [a] → M.Map a ℤ
freqMap unigrams = populate M.empty unigrams
  where populate ∷ Ord a ⇒ M.Map a ℤ → [a] → M.Map a ℤ
        populate m [] = m
        populate m (x:xs) = let freq = (M.findWithDefault 0 x m) ∷ ℤ
                            in populate (M.insert x (freq + 1) m) xs

--  | Takes in a tuple (x, y) (i.e., a bigram), and takes in two frequency maps
--    c₁ and c₂, c₁ for counting all the occurrences of x and the other for
--    counting the number of bigrams.

main ∷ IO ()
main = do
  files ← getDirectoryContents "tb_uni"
  let fileNames = drop 2 . take 1500 $ map ("tb_uni/" ++) files
  pairList ← mapM runX $ map getWords fileNames
  let taggedWordsList  = map parseTupleList pairList
      taggedWords ∷ [(String, POS)]
      taggedWords      = concat taggedWordsList
      (ws, ts)         = unzip taggedWords
      -- For computing the probability of word-tag pair.
      taggedWordFreqs  = freqMap taggedWords
      wordFreqs        = freqMap ws
      tagFreqs         = freqMap ts
      -- For computing the probability of tag bigram
      tagBigrams       = [(ts !! i, ts !! (i+1)) | i ← [0 .. (length ts)-2]]
      tagBigramFreqs   = freqMap tagBigrams
      -- We will use these to create our HMM.
      initStatesFreqs  = freqMap  $ map (head . map snd) taggedWordsList
  createDirectoryIfMissing False "model"
  writeFile "model/tagFreqs.hs"        (show tagFreqs)
  writeFile "model/wordFreqs.hs"       (show wordFreqs)
  writeFile "model/tagBigramFreqs.hs"  (show tagBigramFreqs)
  writeFile "model/taggedWordFreqs.hs" (show taggedWordFreqs)
  writeFile "model/initStatesFreqs.hs" (show initStatesFreqs)
  writeFile "model/words.hs" (show ws)
  putStrLn "Successfully saved model in model."

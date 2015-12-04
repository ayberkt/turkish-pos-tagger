{-# LANGUAGE UnicodeSyntax #-}

module Main where

import           Data.Array           (Array, listArray)
import           Data.HMM             (HMM (..), viterbi)
import qualified Data.Map             as M
import           Data.List            (intercalate)
import           Data.Number.LogFloat
import           Parse
import           Prelude              hiding (putStr, words, lines)
import           System.Directory
import           System.IO (putStr)
import           Text.XML.HXT.Core

type ℤ = Int      -- The integer type `Int` will be denoted by ℤ.
type ℚ = Double   -- The double type will be denoted by ℚ.
type 𝓛 = LogFloat -- Log-domain numbers to prevent underflow.

ε ∷ Double
ε = encodeFloat 1 $ fst  r - ds
  where r  = floatRange (0.1 :: Double)
        ds = floatDigits (0.1 :: Double)

sampleSentence₁ ∷ Array Int String
sampleSentence₁ = listArray (0, 4) [ "gözleri"
                                    , "kor"
                                    , "gibi"
                                    , "yanıyordu"
                                    , "."]

sampleSentence₂ ∷ Array Int String
sampleSentence₂ = listArray (0, 3) [ "adam"
                                    , "yine"
                                    , "geldi"
                                    , "."
                                    ]

sampleSentence₃ ∷ Array Int String
sampleSentence₃ = listArray (0, 4) [ "güzel"
                                    , "kız"
                                    , "mutlu"
                                    , "gözüküyordu"
                                    , "."]

sampleSentence₄ ∷ Array Int String
sampleSentence₄   = listArray (0, 5) [ "renksiz"
                                    , "yeşil"
                                    , "fikirler"
                                    , "sessizce"
                                    , "uyuyor"
                                    , "."]

sampleSentence₅ ∷ Array Int String
sampleSentence₅   = listArray (0, 3) [ "dostlar"
                                    , "beni"
                                    , "hatırlasın"
                                    , "."]

printTaggedSent ∷ Array Int String → [POS] → IO ()
printTaggedSent ws ps = let lines = (\n → replicate n '-') <$> (length <$> ws)
                        in do putStrLn $ foldr (++) " "   $ (++ " ") <$> ws
                              putStrLn $ foldr (++) " "   $ (++ " ") <$> lines
                              putStrLn $ intercalate "  " $  map show ps

freqMap ∷ Ord a ⇒ [a] → M.Map a ℤ
freqMap unigrams = populate M.empty unigrams
  where populate ∷ Ord a ⇒ M.Map a ℤ → [a] → M.Map a ℤ
        populate m [] = m
        populate m (x:xs) = let freq = (M.findWithDefault 0 x m) ∷ ℤ
                            in populate (M.insert x (freq + 1) m) xs

--  | Takes in a tuple (x, y) (i.e., a bigram), and takes in two frequency maps
--    c₁ and c₂, c₁ for counting all the occurrences of x and the other for
--    counting the number of bigrams.
probability ∷ (Ord a, Ord b) ⇒ (a, b) → M.Map a ℤ → M.Map (a, b) ℤ → 𝓛
probability (x, y) c₁ c₂ = if xCount == 0 || yCount == 0
                           then logFloat ε
                           else (logFloat yCount) / (logFloat xCount)
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
      -- We will use these to create our HMM.
      transFn s₁ s₂     = probability (s₁, s₂) tagFreqs tagBigramFreqs
      outFn s e        = probability (e, s) wordFreqs taggedWordFreqs
      initStatesFreqs  = freqMap  $ map (head . map snd) taggedWordsList
      initProbFn s     = let count  = M.findWithDefault 0 s initStatesFreqs
                             count' = fromIntegral count
                             n      = fromIntegral $ length taggedWordsList
                         in logFloat $ count' / n
      possibleTags     = [Noun .. Unknown]
      newHMM           = HMM { states      = possibleTags ∷ [POS]
                             , events      = ws ∷ [String]
                             , initProbs   = initProbFn
                             , transMatrix = transFn
                             , outMatrix   = outFn}
  writeFile "model.hmm" (show newHMM)
  putStrLn "Creating the model..."
  printTaggedSent sampleSentence₁ $ viterbi newHMM sampleSentence₁
  putStr "\n"
  printTaggedSent sampleSentence₂ $ viterbi newHMM sampleSentence₂
  putStr "\n"
  printTaggedSent sampleSentence₃ $ viterbi newHMM sampleSentence₃
  putStr "\n"
  printTaggedSent sampleSentence₄ $ viterbi newHMM sampleSentence₄
  putStr "\n"
  printTaggedSent sampleSentence₅ $ viterbi newHMM sampleSentence₅

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE UnicodeSyntax      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}

module Main where

import Prelude
import Data.HMM (loadHMM, viterbi, HMM(..))
import System.IO
import Data.Array
import Parse (POS(..))
import Data.Map.Strict (findWithDefault, Map(..))
import Data.List (intercalate)
import Data.Number.LogFloat

type ℤ = Int
type ℚ = Double
type 𝓛 = LogFloat -- Log-domain numbers to prevent underflow.

probability ∷ (Ord a, Ord b) ⇒ (a, b) → Map a ℤ → Map (a, b) ℤ → 𝓛
probability (x, y) c₁ c₂ = if xCount == 0 || yCount == 0
                           then logFloat ε
                           else (logFloat yCount) / (logFloat xCount)
  where yCount = fromIntegral (findWithDefault 0 (x, y) c₂) ∷ ℚ
        xCount = fromIntegral (findWithDefault 0 x c₁) ∷ ℚ

ε ∷ Double
ε = encodeFloat 1 $ fst  r - ds
  where r  = floatRange (0.1 :: Double)
        ds = floatDigits (0.1 :: Double)


sample₁ ∷ Array Int String
sample₁ = listArray (0, 4) [ "gözleri"
                           , "kor"
                           , "gibi"
                           , "yanıyordu"
                           , "."]

sample₂ ∷ Array Int String
sample₂ = listArray (0, 3) [ "adam"
                           , "yine"
                           , "geldi"
                           , "."
                           ]

sample₃ ∷ Array Int String
sample₃ = listArray (0, 4) [ "güzel"
                           , "kız"
                           , "mutlu"
                           , "gözüküyordu"
                           , "."]

sample₄ ∷ Array Int String
sample₄   = listArray (0, 5) [ "renksiz"
                             , "yeşil"
                             , "fikirler"
                             , "sessizce"
                             , "uyuyor"
                             , "."
                             ]

sample₅ ∷ Array Int String
sample₅   = listArray (0, 3) [ "dostlar"
                             , "beni"
                             , "hatırlasın"
                             , "."
                             ]

sample₆ ∷ Array Int String
sample₆ = listArray (0, 25) [ "Cebren"
                            , "ve"
                            , "hile"
                            , "ile"
                            , "aziz"
                            , "vatanın"
                            , ","
                            , "bütün"
                            , "kaleleri"
                            , "zaptedilmiş"
                            , "bütün"
                            , "tersanelerine"
                            , "girilmiş"
                            , ","
                            , "bütün"
                            ,"orduları"
                            ,"dağıtılmış"
                            , "ve"
                            , "memleketin"
                            ,"her"
                            ,"köşesi"
                            ,"bilfiil"
                            ,"işgal"
                            ,"edilmiş"
                            ,"olabilir"
                            ,"."
                            ]

sample₇ ∷ Array Int String
sample₇ = listArray (0, 4) [ "Adam"
                           , "sırıta"
                           , "sırıta"
                           , "yürüyordu"
                           , "."]

pretty ∷ Array Int String → [POS] → IO ()
pretty ws ps = let lines = (\n → replicate n '-') <$> (length <$> ws)
                   align ∷ String → String → String
                   align w t = let
                     n      = max 0 (length w - (length t)) `div` 2
                     spaces = replicate n ' '
                     in spaces ++ t ++ spaces
                   wsList   = foldr (:) [] ws
                   pStrings = map show ps
               in do putStrLn $ intercalate " " wsList
                     putStrLn $ foldr (++) " "   $ (++ " ") <$> lines
                     putStrLn $ intercalate "  " $
                       zipWith align wsList pStrings

main :: IO ()
main = do
  tagFreqsStr        ← readFile "model/tagFreqs.hs"
  wordFreqsStr       ← readFile "model/wordFreqs.hs"
  tagBigramFreqsStr  ← readFile "model/tagBigramFreqs.hs"
  taggedWordFreqsStr ← readFile "model/taggedWordFreqs.hs"
  initStatesFreqsStr ← readFile "model/initStatesFreqs.hs"
  wsStr              ← readFile "model/words.hs"
  let tagFreqs        = (read tagFreqsStr) ∷ Map POS Int
      wordFreqs       = (read wordFreqsStr) ∷ Map String Int
      tagBigramFreqs  = (read tagBigramFreqsStr) ∷ Map (POS, POS) Int
      taggedWordFreqs = (read taggedWordFreqsStr) ∷ Map (String, POS) Int
      initStatesFreqs = (read initStatesFreqsStr) ∷ Map POS Int
      ws              = (read wsStr) ∷ [String]
      initProbFn s    = let count  = findWithDefault 0 s initStatesFreqs
                            count' = fromIntegral count
                            n      = fromIntegral $ length taggedWordFreqs
                        in logFloat $ count' / n
      transFn s₁ s₂    = probability (s₁, s₂) tagFreqs tagBigramFreqs
      outFn s e       = probability (e, s) wordFreqs taggedWordFreqs
      possibleTags    = [Noun .. Unknown]
      !newHMM         = HMM { states      = possibleTags ∷ [POS]
                            , events      = ws ∷ [String]
                            , initProbs   = initProbFn
                            , transMatrix = transFn
                            , outMatrix   = outFn
                            }
    in pretty sample₁ $ viterbi newHMM sample₁

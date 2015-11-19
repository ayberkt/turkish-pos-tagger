{-# LANGUAGE BangPatterns #-}

module Main where
import Data.HMM (loadHMM, viterbi, HMM(..))
import System.IO
import Data.Array

main :: IO ()
main = do
  trainedHMM <- (loadHMM "model.hmm") :: IO (HMM Int String)
  putStr "> "
  hFlush stdout
  input <- getLine
  let ws       = words input
      !wsArray = listArray (0, (length ws)-1) ws
      !out      = viterbi trainedHMM wsArray
  print $ head out
  return ()

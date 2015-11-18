
module Main where

import Data.HMM
import System.IO hiding (putStr)
import System.Directory
import Data.Array
import Text.XML.HXT.Core
import Data.List (elemIndex)
import Prelude hiding (words, putStr)
import Parse


main :: IO ()
main = do
  files <- getDirectoryContents "tb_uni"
  let file = "tb_uni/" ++ files !! 4
      fileNames = drop 2 . take 100 $ map ("tb_uni/" ++) files
  pairList <- mapM runX $ map getWords fileNames
  let pairs      = foldr (++) [] pairList
      ws         = map fst pairs
      tags       = map (extractPOS . snd) pairs
                   -- Now let us "index" each of the words with a
                   -- tag index i.e. (fromEnum t) where t ∈ POS.
      pairs'     = zip (map fromEnum tags) ws
      pairsArr   = array (0, 1386) $ zip [0..1386] ws
      myHMM      = simpleHMM [0..12] ws
      trainedHMM = baumWelch myHMM pairsArr 30
      sampleSent = array (0, 5) $ zip [0..5] [ "yılının"
                                             , "ilk"
                                             , "ayına"
                                             , "iliskin"]
  saveHMM "model.hmm" trainedHMM
  -- _ <- mapM putStrLn [pairsArr ! x | x <- [0..15]]
  return ()

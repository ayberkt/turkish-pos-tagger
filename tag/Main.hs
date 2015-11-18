module Main where
import Data.HMM (loadHMM, viterbi, HMM(..))
import System.IO
import Data.Array

main :: IO ()
main = do
  -- trainedHMM <- (loadHMM "model.hmm") :: IO (HMM Int String)
  putStr "> "
  hFlush stdout
  input      <- getLine
  let ws      = words input
      wsArray = array (0, )
  return ()

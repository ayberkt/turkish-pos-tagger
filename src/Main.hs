{-# LANGUAGE Arrows #-}

module Main where

import Data.HMM
import Control.Monad
import System.IO
import System.Directory
import qualified Data.Vector.Storable as VS
import qualified Text.XML.HXT.Core as HXT

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
         deriving (Eq, Show)

main :: IO ()
main = do
  files <- getDirectoryContents "tb_corrected"
  print $ files !! 2

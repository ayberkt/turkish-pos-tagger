module Main where

import Data.HMM
import Control.Monad
import System.IO
import qualified Data.Vector.Storable as VS

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

main = putStrLn "Hi"

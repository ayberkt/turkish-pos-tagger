module Main where

import Data.HMM
import Control.Monad
import Data.Array
import System.IO

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

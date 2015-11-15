{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

-- import Data.HMM
import System.IO hiding (putStr)
import System.Directory
-- import qualified Data.Vector.Storable as VS
import Text.XML.HXT.Core
import Prelude hiding (words, putStr)

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

parseFile :: FilePath -> IOStateArrow a b XmlTree
parseFile f = readDocument [ withValidate no
                           , withRemoveWS yes
                           ] f

words = deep (isElem >>> hasName "S") >>>
  proc x -> do
    word <- getText <<< getChildren <<< deep (hasName "W") -< x
    returnA -< word


main :: IO ()
main = do
  files <- getDirectoryContents "tb_uni"
  let file = "tb_uni/" ++ files !! 2
  ws <- runX (readDocument [withValidate no] file >>> words)
  putStrLn $ unwords ws

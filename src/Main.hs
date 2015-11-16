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
         deriving (Eq, Show, Read)

parseFile :: FilePath -> IOStateArrow a b XmlTree
parseFile f = readDocument [ withValidate no
                           , withRemoveWS yes
                           ] f

getPOS :: String -> POS
getPOS x = read (takeWhile (\n -> n /= '+' && n /= '"') $ tail $ dropWhile (/= '+') x)

atTag tag = deep (isElem >>> hasName tag)

words = atTag "S" >>>
  proc x -> do
    w    <- (atTag "W") -< x
    word <- getText <<< getChildren -< w
    tag  <- getAttrValue "IG" -< w
    returnA -< (word, tag)

printTuple :: (String, String) -> IO ()
printTuple (a, b)  = putStrLn (a ++ " " ++ (show $ getPOS b))

main :: IO ()
main = do
  files <- getDirectoryContents "tb_uni"
  let file = "tb_uni/" ++ files !! 2
  ws <- runX (readDocument [withValidate no] file >>> words)
  _  <- mapM printTuple ws
  return ()

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Main where

-- import Data.HMM
import System.IO hiding (putStr)
import System.Directory
-- import qualified Data.Vector.Storable as VS
import Text.XML.HXT.Core
import Prelude hiding (words, putStr)

type TaggedWord = (String, POS)

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

extractPOS :: String -> POS
extractPOS x = read $ takeWhile notPlusNorQuote . tail $ dropWhile notPlus x
  where notPlus = (/= '+')
        notPlusNorQuote = \n -> notPlus n && n /= '"'

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

words :: ArrowXml cat => cat XmlTree (String, String)
words = atTag "S" >>>
  proc x -> do
    w       <- (atTag "W") -< x
    word    <- getText <<< getChildren -< w
    tagStr  <- getAttrValue "IG" -< w
    returnA -< (word, tagStr)

printTuple :: (String, String) -> IO ()
printTuple (a, b)  = putStrLn (a ++ " " ++ (show $ extractPOS b))

main :: IO ()
main = do
  files <- getDirectoryContents "tb_uni"
  let file = "tb_uni/" ++ files !! 3
  ws <- runX (readDocument [withValidate no] file >>> words)
  _  <- mapM printTuple ws
  return ()

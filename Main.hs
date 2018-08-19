{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad
import Data.Either
import Data.Maybe
import Data.Text (Text, pack, unpack, unlines)
import Prelude (IO, putStrLn, print,(.), ($), (<$>), error, concat)
import System.IO
import Text.Diff.Parse
import Text.Diff.Parse.Types
import Text.Highlighting.Pygments

parseDiff' (Right r) = r
parseDiff' (Left err) = error err

parseContent (Hunks xs) = (fmap . fmap ) lineContent  (hunkLines <$> xs)

main :: IO ()
main = do

   Just hsLexer <- getLexerByName "hs"
   let hl = highlight hsLexer terminalFormatter [("encoding", "utf-8")]
   contents <- readFile "test.patch"

   print
      $  unpack
     <$> fileDeltaSourceFile
     <$> (parseDiff' . parseDiff $ pack contents)

   (mapM_ . mapM_) (\x -> hl (concat x) >>= putStr)
      $  (fmap . fmap . fmap) unpack (parseContent
     <$> (fileDeltaContent <$> (parseDiff' $ parseDiff $ pack contents)))

   print $ parseDiff' $ parseDiff $ pack contents
   putStrLn "Hello, Haskell!"

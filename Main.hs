{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main where

import Control.Monad
import Data.Either
import Data.Maybe
import Data.Text (Text, pack, unpack, unlines)
import Prelude (IO, putStrLn, print,(.), ($), (<$>), error, concat, Show, show)
import System.IO
import Text.Diff.Parse
import Text.Diff.Parse.Types
import Text.Highlighting.Pygments


diffErrorHandler (Right r) = r
diffErrorHandler (Left err) = error err

parseContent (Hunks xs) = (fmap . fmap ) lineContent  (hunkLines <$> xs)

unparseLine :: Line -> Text
unparseLine = lineContent

unparseHunk :: Hunk -> Text
unparseHunk x = unlines $ unparseLine <$> (hunkLines x)

unparseContent :: Content -> Text
unparseContent Binary = pack ""
unparseContent (Hunks xs) = unlines $ unparseHunk <$> xs

unparseDiff :: FileDelta -> Text
unparseDiff model@FileDelta {..} = unparseContent fileDeltaContent

main :: IO ()
main = do

   Just hsLexer <- getLexerByName "hs"
   let hl = highlight hsLexer terminalFormatter [("encoding", "utf-8")]
   contents <- readFile "test.patch"

   {- print -}
      {- $  unpack -}
     {- <$> fileDeltaSourceFile -}
     {- <$> (diffErrorHandler . parseDiff $ pack contents) -}

   {- (mapM_ . mapM_) (\x -> hl (concat x) >>= putStr) -}
      {- $  (fmap . fmap . fmap) unpack (parseContent -}
     {- <$> (fileDeltaContent <$> (diffErrorHandler $ parseDiff $ pack contents))) -}

   let a = diffErrorHandler $ parseDiff $ pack contents
   print $ unparseDiff <$> a
   {- print $ unparseDiff -}
   putStrLn "Hello, Haskell!"

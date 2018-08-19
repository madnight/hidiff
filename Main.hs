{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main where

import Control.Monad
import Data.Either
import Data.Maybe
import Data.Text (Text, pack, unpack, unlines)
import Prelude (IO, print,(.), ($), (<$>), error, concat, Show, show)
import Text.Diff.Parse
import Text.Diff.Parse.Types
import Text.Highlighting.Pygments
import Data.Text.IO
import System.IO.Unsafe

infixl 0 |>
x |> f = f x

diffErrorHandler (Right r) = r
diffErrorHandler (Left err) = error err

parseContent (Hunks xs) = (fmap . fmap) lineContent (hunkLines <$> xs)

unparseLine :: Line -> Text
unparseLine = lineContent

unparseHunk :: Hunk ->  Text
unparseHunk x =  unparseLine <$> hunkLines x
              |> unlines

unparseContent :: Content -> Text
unparseContent Binary = pack ""
unparseContent (Hunks xs) = hl "hs" $ unlines $ unparseHunk <$> xs

unparseDiff :: FileDelta -> Text
unparseDiff model@FileDelta {..} = unparseContent fileDeltaContent

hl :: Text -> Text -> Text
hl name str =  unpack str
            |> highlight hsLexer terminalFormatter [("encoding", "utf-8")]
            |> unsafePerformIO
            |> pack
   where
    hsLexer =  unpack name
            |> getLexerByName
            |> unsafePerformIO
            |> fromJust


main :: IO ()
main = do

   contents <- readFile "test.patch"

   {- print -}
      {- $  unpack -}
     {- <$> fileDeltaSourceFile -}
     {- <$> (diffErrorHandler . parseDiff $ pack contents) -}

   {- (mapM_ . mapM_) (\x -> hl (concat x) >>= putStr) -}
      {- $  (fmap . fmap . fmap) unpack (parseContent -}
     {- <$> (fileDeltaContent <$> (diffErrorHandler $ parseDiff $ pack contents))) -}

   let a = diffErrorHandler $ parseDiff $ contents
   mapM_ putStrLn (unparseDiff <$> a)
   {- print $ unparseDiff -}
   putStrLn "Hello, Haskell!"

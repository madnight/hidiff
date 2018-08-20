{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Arrow
import Control.Lens hiding ((|>))
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Text (Text, pack, unpack, unlines)
import Data.Text.IO
import Prelude hiding (unlines, putStrLn, readFile)
import System.IO.Unsafe
import Text.Diff.Parse
import Text.Diff.Parse.Types
import Text.Highlighting.Pygments

makeLensesFor [("fileDeltaContent", "fD")] ''FileDelta
makeLensesFor [("hunkLines", "hL")]        ''Hunk
makeLensesFor [("lineContent", "lC")]      ''Line
makePrisms                                 ''Content

diffErrorHandler :: Either String p -> p
diffErrorHandler (Right r) = r
diffErrorHandler (Left err) = error err

hl :: Text -> Text -> IO Text
hl name str = do
      Just hsLexer <- getLexerByName (unpack name)
      x <- highlight hsLexer
         terminalFormatter [("encoding", "utf-8")] (unpack str)
      pure $ pack x

setContentFileDelta :: Text -> FileDelta -> FileDelta
setContentFileDelta str fileDelta = fileDelta
     & fD . _Hunks . ix 0 . hL . ix 0 . lC .~ str

main :: IO ()
main = do
   let getter = fD . _Hunks . ix 0 . hL . ix 0 . lC

   contents <- readFile "test.patch"

   let b = head $ diffErrorHandler $ parseDiff contents
   let x = b ^. getter

   p <- hl "hs" x

   putStrLn $ setContentFileDelta p b ^. getter

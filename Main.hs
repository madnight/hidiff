{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where

import Control.Lens
import Data.Text (Text, pack, unlines, unpack)
import Data.Text.IO (putStrLn, readFile)
import Prelude hiding (putStrLn, readFile, unlines)
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types
import qualified Text.Highlighting.Pygments as P

makeLensesFor [("fileDeltaContent", "fD")] ''FileDelta
makeLensesFor [("hunkLines", "hL")] ''Hunk
makeLensesFor [("lineContent", "lC")] ''Line
makePrisms ''Content

highlight :: Text -> Text -> IO Text
highlight name str = do
  Just lexer <- P.getLexerByName $ unpack name
  pack <$> P.highlight lexer P.terminalFormatter encoding (unpack str)
  where
    encoding = [("encoding", "utf-8")]

contentLens :: Applicative f => (Text -> f Text) -> FileDelta -> f FileDelta
contentLens = fD . _Hunks . ix 0 . hL . ix 0 . lC

setContent :: Text -> FileDelta -> FileDelta
setContent str fileDelta = fileDelta & contentLens .~ str

main :: IO ()
main = do
  contents <- readFile "test.patch"
  let diff = head $ diffError $ parseDiff contents
  hiContent <- highlight "hs" $ diff ^. contentLens
  putStrLn $ setContent hiContent diff ^. contentLens
  where
    diffError (Right r) = r
    diffError (Left _)  = error "diff parsing exception: no a valid git diff"

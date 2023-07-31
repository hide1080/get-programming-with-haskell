module Main where

import qualified Q34_2_Glitcher as G

import System.Environment
import qualified Data.ByteString.Char8 as BC
import Control.Monad


main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <- foldM (\ bytes f -> f bytes) imageFile G.glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

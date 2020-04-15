module Main where

import Lib
import System.Environment

getFile :: IO FilePath
getFile = do
  args <- getArgs
  case args of
    [file] -> pure file
    _ -> error "Please provided an image file"

main :: IO ()
main = do
    file <- getFile
    ansiImage file

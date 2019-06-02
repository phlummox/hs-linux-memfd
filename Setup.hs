module Main where

import Distribution.Extra.Doctest (defaultMainWithDoctests)

main :: IO ()
main = do
  defaultMainWithDoctests "linux-memfd-doctests"
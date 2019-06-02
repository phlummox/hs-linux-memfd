
module Main where

--import System.FilePath.Glob
import Test.DocTest   (doctest)
import Build_doctests (flags, pkgs, module_sources)

main :: IO ()
main = 
  do
    putStrLn $ "\n\nrunning doctest with args: " ++ show args
    doctest args
  where
    args = ["-fobject-code"] ++ flags ++ pkgs ++ module_sources


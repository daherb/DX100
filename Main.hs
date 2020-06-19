module Main where

import Test.QuickCheck
import System.Environment
import DX100.Parameters

main :: IO ()
main =
  do
    v <- generate (arbitrary :: Gen Voice)
    let s = writeSysex (Sysex 0 v)
    args <- getArgs
    if length args > 0 then
      writeSyx (args !! 0) s
      else
      putStrLn "Please give a file name for the new voice"

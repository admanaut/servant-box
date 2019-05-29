module Main where

import Options
import Options.Applicative
import Mock

main :: IO ()
main
  = do
      opts <- execParser (info options fullDesc)
      run (oPort opts)

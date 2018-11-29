module Main where

import Options
import Options.Applicative
import Network.Wai.Handler.Warp
import Server

main :: IO ()
main
  = do
      opts <- execParser (info options fullDesc)
      run (oPort opts) app

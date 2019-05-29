module Options
  ( Options (..)
  , options
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Options
  = Options
      { oPort :: !Int
      }

options :: Parser Options
options
  =   Options
  <$> option auto
      (  long "port"
      <> help "Port"
      <> metavar "PORT"
      )

module Main where

import Options.Applicative

import ClassyPrelude hiding ((<>))
import Kyarigwo.AddModule

main :: IO ()
main = execParser opts >>= addModule
  where
    opts = info (helper <*> args)
      (fullDesc
      <> progDesc "Adds a Haskell module named MODULE_NAME based on TEMPLATE"
      <> header "kyaddmodule - a program to add haskell modules")

args :: Parser Args
args = Args
  <$> argument str (metavar "TEMPLATE")
  <*> argument (pack <$> str) (metavar "MODULE_NAME")

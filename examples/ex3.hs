{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import System.Environment

import System.Console.YAOP
import System.Console.YAOP.TH

import Data.Monoid

import qualified Data.Map as M

data Options = Options { optFoo :: Product Int
                       , optBar :: Maybe Int
                       , optDict :: M.Map String Int
                       , optSources  :: [String]
                       } deriving (Show)

makeSetters ''Options

instance Configurable Options where
    defOptions = Options { optFoo = mempty
                         , optBar = Nothing
                         , optDict = M.empty
                         , optSources = []
                         }
    descOptions = do
      _optFoo    =: append ['F'] ["foo"] "MVAR" "Set foo"
      _optDict   =: append ['a'] ["add"] "K=INT" "Add environment variable"
      _optSources=: append ['S'] []      "PATH" "Include file"
      _optBar    =: set ['B'] ["bar"] "MVAR" "Set bar"

test args = withArgs args main


main = do
  (opts :: Options, args :: [String]) <- parseOptions =<< getArgs
  print opts
  print args

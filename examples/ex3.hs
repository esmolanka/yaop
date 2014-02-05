{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import System.Environment
import System.Directory

import System.Console.YAOP
import System.Console.YAOP.TH

import Data.Monoid

import qualified Data.Map as M

data Options = Options { optFoo :: Product Int
                       , optBar :: Maybe Int
                       , optDict :: M.Map String Int
                       , optSources  :: [String]
                       , optThings :: First String
                       } deriving (Show)

makeSetters ''Options

instance Configurable Options where
    defOptions = Options { optFoo = mempty
                         , optBar = Nothing
                         , optDict = M.empty
                         , optSources = []
                         , optThings = First Nothing
                         }
    descOptions = do
      _optFoo    =: append ['F'] ["foo"] Nothing "Set foo"
      _optDict   =: append ['a'] ["add"] Nothing "Add environment variable"
      _optSources=: append ['S'] []      Nothing "Include file"
      _optBar    =: set ['B'] ["bar"] Nothing "Set bar"
      _optThings =: appendMap ['i'] [] Nothing "Add include path" (\(path :: String) -> doesFileExist path >>=
                                                                      \x -> return $ if x then First (Just path) else First Nothing)

test args = withArgs args main

main = do
  (opts :: Options, args :: [String]) <- parseOptions =<< getArgs
  print opts
  print args

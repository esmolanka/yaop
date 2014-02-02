{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import System.Environment
import System.Console.YAOP
import System.Console.YAOP.Selector

import Data.List
import Data.Maybe

-- | Options data structure. Should use record syntax, may have more than one constructor
data Options = Options { optFileName :: FilePath
                       , optCount :: Int
                       , optStuff :: [Either Int String]
                       , optDisableTheThing :: Bool
                       } deriving (Show)

-- | This triggers YAOP's accessors generator, e.g.
-- @modM_optFileName :: Monad m => (FilePath -> m FilePath) -> Options -> m Options@
makeSetters ''Options

instance Configurable Options where
    -- | Default options
    defOptions = Options { optFileName = "default.txt"
                         , optCount = 0
                         , optStuff = []
                         , optDisableTheThing = False
                         }
    -- | Here we define a list of options that are mapped to Options
    descOptions = do
     _optFileName =: option ['f'] ["filename"] "FN" "Set some filename" (\arg x -> print arg >> return arg)
     _optCount    =: option ['c'] ["count"] "N" "Set some count" (\arg x -> return $ fromMaybe 100 (arg))
     _optStuff    =: option ['s'] ["stuff"] "S" "Push \"foo\" to a list" (\() x -> return (Right "foo" : x))
     _optDisableTheThing =: set ['d'] ["disable"] "BOOL" "Disable the thing"

main = do
  (opts :: Options, args :: [String]) <- parseOptions' ( defaultParsingConf { pcUsageHeader = "USAGE: ./ex1 [OPTIONS]" } ) =<< getArgs
  print opts
  print args

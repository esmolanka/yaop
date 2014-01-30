{-# LANGUAGE TemplateHaskell #-}

import System.Environment
import System.Console.YAOP

import Data.List
import Data.Maybe

-- | Options that are not mapped to data
withoutData = dummy =: action [] ["action"] "Do some action" (\_ -> putStrLn "IO Action")

-- | Options data structure. Should use record syntax, may have more than one constructor
data Options = Options { optFileName :: FilePath
                       , optCount :: Int
                       , optStuff :: [Either Int String]
                       , optDisableTheThing :: Bool
                       } deriving (Show)

-- | Default options
defOptions = Options { optFileName = "default.txt"
                     , optCount = 0
                     , optStuff = []
                     , optDisableTheThing = False
                     }

-- | This triggers YAOP's accessors generator, e.g.
-- @modM_optFileName :: Monad m => (FilePath -> m FilePath) -> Options -> m Options@
$(deriveModM ''Options)

-- | Here we define a list of options that are mapped to Options
optDesc = do
  modM_optFileName =: param ['f'] ["filename"] "FN" "Set some filename" (\arg x -> print arg >> return arg)
  modM_optCount    =: param ['c'] ["count"] "N" "Set some count" (\arg x -> return $ fromMaybe 100 (arg))
  modM_optStuff    =: action ['s'] ["stuff"] "Push \"foo\" to a list" (\x -> return (Right "foo" : x))
  modM_optDisableTheThing =: flag ['d'] ["disable"] "BOOL" "Disable the thing"

bothDesc = withoutData >> optDesc

main = do
  (opts,args) <- parseOptions bothDesc defOptions ( defaultParsingConf { pcUsageHeader = "USAGE: ./ex1 [OPTIONS]" } ) =<< getArgs
  print opts
  print args

{-# LANGUAGE TemplateHaskell #-}

import System
import System.Environment
import System.Console.YAOP

import Data.List
import Data.Maybe

-- | Options that are not mapped to data
withoutData = [ dummy =: option [] ["action"] (NoA) "Do some action" (\_ _ -> putStrLn "IO Action") ]

-- | Options data structure. Should use record syntax, may have more than one constructor
data Options = Options { optFileName :: FilePath
                       , optCount :: Int
                       , optStuff :: [Either Int String]
                       } deriving (Show)

-- | Default options
defOptions = Options {optFileName = "default.txt", optCount = 0, optStuff = []}

-- | This triggers YAOP's accessors generator, e.g.
-- @modM_optFileName :: Monad m => (FilePath -> m FilePath) -> Options -> m Options@
$(deriveModM ''Options)

-- | Here we define a list of options that are mapped to Options
optDesc = [
  modM_optFileName =: option ['f'] ["filename"] (ReqA "FN") "Set some filename" (\arg x -> print arg >> return (fromMaybe "" arg)),
  modM_optCount    =: option ['c'] ["count"] (OptA "N") "Set some count" (\arg x -> return $ fromMaybe 100 (read `fmap` arg)),
  modM_optStuff    =: option ['s'] ["stuff"] (NoA) "Push \"foo\" to a list" (\arg x -> return $ (Right "foo" : x))
  ]

bothDesc = withoutData ++ optDesc

main = do
  (opts,args) <- parseOptions bothDesc defOptions ( defaultParsingConf { pcUsageHeader = "USAGE: ./ex1 [OPTIONS]" } ) =<< getArgs
  print opts
  print args

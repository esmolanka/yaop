{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, DeriveDataTypeable #-}

import System.Console.YAOP

import Data.Maybe
import Data.Typeable

-- | Options data structure. Should use record syntax, may have more than one constructor
data Options = Options { optFileName :: FilePath
                       , optCount :: Int
                       , optStuff :: [Either Int String]
                       , optDisableTheThing :: Bool
                       } deriving (Show, Typeable)

-- | This triggers YAOP's accessors generator, e.g.
-- @modM_optFileName :: Monad m => (FilePath -> m FilePath) -> Options -> m Options@
makeSetters ''Options

instance Configurable Options where
    -- | Default options
    defConf = Options { optFileName = "default.txt"
                      , optCount = 0
                      , optStuff = []
                      , optDisableTheThing = False
                      }
    -- | Here we define a list of options that are mapped to Options
    parseOpts = do
     _optFileName <=: setter (\arg _ -> print arg >> return arg)
                      <> short 'f' <> long "filename"
                      <> metavar "FN" <> help "Set some filename"
     _optCount    <=: setter (\arg _ -> return $ fromMaybe 100 arg)
                      <> short 'c' <> long "count" <> help "Set some count"
     _optStuff    <=: tweak (\() x -> Right "foo" : x)
                      <> short 's' <> long "stuff" <> help "Push \"foo\" to a list"
     _optDisableTheThing <=: set <> short 'd' <> long "disable" <> help "Disable the thing"

main :: IO ()
main = do
  (opts :: Options, args :: [String]) <- getOptions
  print opts
  print args

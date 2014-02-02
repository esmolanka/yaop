{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import System.Exit
import System.Environment
import System.Console.YAOP

import Control.Monad

import Data.Maybe

usageStr = "USAGE: ./ex2 [OPTIONS]"

instance Configurable () where
    defOptions = ()
    descOptions = do
        dummy =: option [] ["usage"] "" "Print usage string" (\() _ -> putStrLn usageStr >> exitWith ExitSuccess)

-- | Options data structure. Should use record syntax, may have more than one constructor
data Options = Options { optFileName :: FilePath
                       , optCount :: Int
                       , optStuff :: [Either Int String]
                       } deriving (Show)

-- | This triggers YAOP's accessors generator, e.g.
-- @_optFileName :: Monad m => (FilePath -> m FilePath) -> Options -> m Options@
makeSetters ''Options


instance Configurable Options where
    -- | Default options
    defOptions = Options {optFileName = "default.txt", optCount = 0, optStuff = []}

    -- | Here we define a list of options that are mapped to Options
    descOptions = do
      _optFileName =: option ['f'] ["filename"] "FN" "Set some filename" (\arg _ -> print arg >> return arg)
      _optCount    =: option ['c'] ["count"] "N" "Set some count" (\arg _ -> return $ fromMaybe 100 (read `fmap` arg))
      _optStuff    =: option ['s'] ["stuff"] "" "Push \"foo\" to a list" (\() x -> return $ (Right "foo" : x))

-- | Some additional options
data OtherOpts = OtherOpts { otherServer :: String
                           , otherPort :: Int
                           , otherUsername :: String
                           , otherPassword :: Maybe String
                           , otherVerbose :: Bool
                           } deriving (Show)

makeSetters ''OtherOpts

instance Configurable OtherOpts where
    defOptions = OtherOpts { otherServer = ""
                           , otherPort = 8080
                           , otherUsername = ""
                           , otherPassword = Nothing
                           , otherVerbose = False
                           }
    descOptions = do
      _otherServer   =: set ['S'] ["server"]   "HOST" "Set server host"
      _otherPort     =: set ['P'] ["port"]     "PORT" "Set server port"
      _otherUsername =: set [   ] ["username"] "USER" "Set username"
      _otherPassword =: option [   ] ["password"] "PASS" "Set password" (\mstr _ -> fmap Just (maybe getLine return mstr))
      _otherVerbose  =: set ['v'] ["verbose"]  "BOOL" "Verbose mode"
      _otherVerbose  =: option ['q'] ["no-verbose"]  "asdfasdfasdf" "Verbose mode" (\() _ -> return False)

main = do
  ((), opts::Options, otherOpts::OtherOpts) <- parseOptions' defaultParsingConf =<< getArgs
  print opts
  print otherOpts

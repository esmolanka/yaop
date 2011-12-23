{-# LANGUAGE TemplateHaskell #-}

import System
import System.Environment
import System.Console.YAOP

import Data.List
import Data.Maybe

usageStr = "USAGE: ./ex2 [OPTIONS]"

-- | Options that are not mapped to data
withoutData = dummy =: option [] ["usage"] NoA "Print usage string" (\_ _ -> putStrLn usageStr >> exitWith ExitSuccess)

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
optDesc = do
  modM_optFileName =: option ['f'] ["filename"] (ReqA "FN") "Set some filename" (\arg _ -> print arg >> return (fromMaybe "" arg))
  modM_optCount    =: option ['c'] ["count"] (OptA "N") "Set some count" (\arg _ -> return $ fromMaybe 100 (read `fmap` arg))
  modM_optStuff    =: option ['s'] ["stuff"] NoA "Push \"foo\" to a list" (\arg x -> return $ (Right "foo" : x))

-- | Some additional options
data OtherOpts = OtherOpts { otherServer :: String
                           , otherPort :: Int
                           , otherUsername :: String
                           , otherPassword :: String
                           , otherVerbose :: Bool
                           } deriving (Show)

$(deriveModM ''OtherOpts)

defOtherOpts = OtherOpts { otherServer = ""
                         , otherPort = 8080
                         , otherUsername = ""
                         , otherPassword = ""
                         , otherVerbose = False
                         }

-- | Options description for "OtherOpts"
otherDesc = do
  modM_otherServer   =: option ['S'] ["server"]   (ReqA "HOST") "Set server host" (\(Just arg) _ -> return arg)
  modM_otherPort     =: option ['P'] ["port"]     (ReqA "PORT") "Set server port" (\(Just arg) _ -> return $ read arg)
  modM_otherUsername =: option [   ] ["username"] (ReqA "USER") "Set username" (\(Just arg) _ -> return arg)
  modM_otherPassword =: option [   ] ["username"] (OptA "PASS") "Set password" (\arg _ -> return $ fromMaybe "" arg)
  modM_otherVerbose  =: option ['v'] ["verbose"]  NoA           "Verbose mode" (\_ _ -> return True)

-- | Easily joined "Options" with "OtherOpts".
allDesc = withoutData >> firstM =: optDesc >> secondM =: otherDesc
defAll = (defOptions, defOtherOpts)

main = do
  (opts,args) <- parseOptions allDesc defAll (defaultParsingConf { pcUsageHeader = usageStr }) =<< getArgs
  print opts
  print args

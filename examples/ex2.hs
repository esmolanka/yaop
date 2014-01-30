{-# LANGUAGE TemplateHaskell #-}

import System.Exit
import System.Environment
import System.Console.YAOP

import Control.Monad

import Data.Maybe

usageStr = "USAGE: ./ex2 [OPTIONS]"

-- | Options that are not mapped to data
withoutData = dummy =: action [] ["usage"] "Print usage string" (\() -> putStrLn usageStr >> exitWith ExitSuccess)

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
  modM_optFileName =: param  ['f'] ["filename"] "FN" "Set some filename" (\arg _ -> print arg >> return arg)
  modM_optCount    =: param  ['c'] ["count"] "N" "Set some count" (\arg _ -> return $ fromMaybe 100 (read `fmap` arg))
  modM_optStuff    =: action ['s'] ["stuff"] "Push \"foo\" to a list" (\x -> return $ (Right "foo" : x))

-- | Some additional options
data OtherOpts = OtherOpts { otherServer :: String
                           , otherPort :: Int
                           , otherUsername :: String
                           , otherPassword :: Maybe String
                           , otherVerbose :: Bool
                           } deriving (Show)

$(deriveModM ''OtherOpts)

defOtherOpts = OtherOpts { otherServer = ""
                         , otherPort = 8080
                         , otherUsername = ""
                         , otherPassword = Nothing
                         , otherVerbose = False
                         }

-- | Options description for "OtherOpts"
otherDesc = do
  modM_otherServer   =: flag ['S'] ["server"]   "HOST" "Set server host"
  modM_otherPort     =: flag ['P'] ["port"]     "PORT" "Set server port"
  modM_otherUsername =: flag [   ] ["username"] "USER" "Set username"
  modM_otherPassword =: param [   ] ["password"] "PASS" "Set password" (\mstr _ -> fmap Just (maybe getLine return mstr))
  modM_otherVerbose  =: flag ['v'] ["verbose"]  "BOOL" "Verbose mode"
  modM_otherVerbose  =: tweak ['q'] ["no-verbose"]  "asdfasdfasdf" "Verbose mode" (\() -> const False)


-- | Easily joined "Options" with "OtherOpts".
allDesc = withoutData >>
          firstM =: optDesc >>
          secondM =: otherDesc

defAll = (defOptions, defOtherOpts)

main = do
  (opts,args) <- parseOptions allDesc defAll (defaultParsingConf { pcUsageHeader = usageStr }) =<< getArgs
  print opts
  print args

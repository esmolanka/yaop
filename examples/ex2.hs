{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

import System.Exit
import System.Environment
import System.Console.YAOP

usageStr :: String
usageStr = "USAGE: ./ex2 [OPTIONS]"

instance Configurable () where
    parseOpts = do
        dummy <=: action (putStrLn usageStr >> exitWith ExitSuccess)
                 <> long "usage" <> help "Print usage string"

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
    defConf = Options {optFileName = "default.txt", optCount = 0, optStuff = []}

    -- | Here we define a list of options that are mapped to Options
    parseOpts = do
      _optFileName <=: set <> short 'f' <> long "filename" <> help "Set some filename"
      _optCount    <=: set <> short 'c' <> long "count" <> help "Set some count"
      _optStuff    <=: setter (\() x -> return $ (Right "foo" : x)) <> short 's' <> long "stuff" <> help "Push \"foo\" to a list"

    signature _ = "FLAGS"

-- | Some additional options
data OtherOpts = OtherOpts { otherServer :: String
                           , otherPort :: Int
                           , otherUsername :: String
                           , otherPassword :: Maybe String
                           , otherVerbose :: Bool
                           } deriving (Show)

makeSetters ''OtherOpts

instance Configurable OtherOpts where
    defConf = OtherOpts { otherServer = ""
                           , otherPort = 8080
                           , otherUsername = ""
                           , otherPassword = Nothing
                           , otherVerbose = False
                           }
    parseOpts = do
      _otherServer   <=: set <> short 'S' <> long "server" <> metavar "HOST" <> help "Set server host"
      _otherPort     <=: set <> short 'P' <> long "port" <> metavar   "PORT" <> help "Set server port"
      _otherUsername <=: set <> long "username" <> metavar "USER" <> help "Set username"
      _otherPassword <=: setter (\mstr _ -> fmap Just (maybe getLine return mstr))
                         <> long "password" <> metavar "PASS" <> help "Set password"
      _otherVerbose  <=: set <> short 'v' <> long "verbose" <> help "Verbose mode"
      _otherVerbose  <=: setConst False <> short 'q' <> long "no-verbose" <> help "Verbose mode"

    signature _ = "OTHER-FLAGS"

main :: IO ()
main = do
  ((), opts::Options, otherOpts::OtherOpts) <- parseOptions' defaultParsingConf =<< getArgs
  print opts
  print otherOpts

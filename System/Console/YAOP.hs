{-# LANGUAGE FlexibleContexts, FlexibleInstances, DefaultSignatures #-}

module System.Console.YAOP
    ( module System.Console.YAOP.Selector
    , module System.Console.YAOP.TH
    , module System.Console.YAOP.Argument
    , module System.Console.YAOP.Types
    , (=:)
    , option
    , set
    , append
    , prepend
    , appendMap
    , Configurable (..)
    , ParsingConf (..)
    , defaultParsingConf
    , parseOptions
    , parseOptions'
    )
    where

import System.Environment
import System.IO.Unsafe
import System.Exit
import System.Console.GetOpt

import System.Console.YAOP.Selector
import System.Console.YAOP.Argument
import System.Console.YAOP.TH
import System.Console.YAOP.Types

import Control.Arrow

import Control.Monad.Writer

import Data.Default
import Data.Maybe
import Data.List

-- | Apply selector to options combinator
(=:) :: (MonadWriter [Opt t] (OptM t)) =>
        ((t -> IO t) -> a -> IO a)  -- ^ selector
     -> OptM t ()                   -- ^ options
     -> OptM a ()
(=:) f optm = tell . map (fmap f) $ runOptM optm

option :: (Argument arg) =>
         String
      -> [String]
      -> String
      -> String
      -> (arg -> a -> IO a)
      -> OptM a ()
option short long arg help setter =
    tell [ Opt $ Option short long (argDescr setter arg) help ]

set :: (Argument a) =>
       String -> [String] -> String -> String -> OptM a ()
set short long arg help =
    let setter a _ = return $ a
    in option short long arg help setter

append :: (Monoid a, Argument arg, Singleton arg a) =>
          String -> [String] -> String -> String -> OptM a ()
append short long arg help =
    let setter :: (Monoid a, Singleton arg a) => arg -> a -> IO a
        setter a acc = return $ acc <> singleton a
    in option short long arg help setter

prepend :: (Monoid a, Argument arg, Singleton arg a) =>
          String -> [String] -> String -> String -> OptM a ()
prepend short long arg help =
    let setter :: (Monoid a, Singleton arg a) => arg -> a -> IO a
        setter a acc = return $ singleton a <> acc
    in option short long arg help setter

appendMap :: (Monoid a, Argument arg) =>
             String
          -> [String] -> String
          -> String -> (arg -> IO a)
          -> OptM a ()
appendMap short long arg help f =
    let setter arg acc = do
          val <- f arg
          return $ val <> acc
    in option short long arg help setter

argument :: (Argument a) =>
            (a -> b -> IO b)
         -> OptM b ()
argument setter =
    tell [ Arg $ argParse setter ]

class Configurable a where
    defOptions :: a
    default defOptions :: (Default a) => a
    defOptions = def
    descOptions :: OptM a ()

instance Configurable [String] where
    defOptions = []
    descOptions = tell [ Arg $ ArgParse Many (\str l -> return $ l ++ [str] ) ]

instance (Configurable a, Configurable b) => Configurable (a, b) where
    defOptions = (defOptions, defOptions)
    descOptions = do
      firstM =: descOptions
      secondM =: descOptions

instance (Configurable a, Configurable b, Configurable c) => Configurable (a, b, c) where
    defOptions = (defOptions, defOptions, defOptions)
    descOptions = do
      (\f (x, y, z) -> f x >>= \q -> return (q, y, z)) =: descOptions
      (\f (x, y, z) -> f y >>= \q -> return (x, q, z)) =: descOptions
      (\f (x, y, z) -> f z >>= \q -> return (x, y, q)) =: descOptions

instance (Configurable a, Configurable b, Configurable c, Configurable d) => Configurable (a, b, c, d) where
    defOptions = (defOptions, defOptions, defOptions, defOptions)
    descOptions = do
      (\f (x, y, z, i) -> f x >>= \q -> return (q, y, z, i)) =: descOptions
      (\f (x, y, z, i) -> f y >>= \q -> return (x, q, z, i)) =: descOptions
      (\f (x, y, z, i) -> f z >>= \q -> return (x, y, q, i)) =: descOptions
      (\f (x, y, z, i) -> f i >>= \q -> return (x, y, z, q)) =: descOptions


parseArgs :: [String] -> [ArgParse (t -> IO t)] -> [(t -> IO t)]
parseArgs args parsers =
    let (reqBeginning, rest1)   = break (not . isRequired) parsers
        (reqEnding, optionalPs) = second reverse . break (not . isRequired) $ reverse rest1

    in undefined
    where
      isRequired (ArgParse OneReq _) = True
      consumeReq (x:xs) ((ArgParse OneReq fn):ps) =
          let (actions, rest) = consumeReq xs ps
          in (fn x : actions, rest)
      consumeReq [] (p:ps) = error "Required parameter is not specified"
      consumeReq xs [] = ([], xs)


data ParsingConf = ParsingConf
    { pcUsageHeader   :: String
      -- ^ Usage message header
    , pcHelpFlag      :: Maybe String
      -- ^ Name of help message flag, default: @\"help\"@
    , pcHelpExtraInfo :: String
      -- ^ Extra help information
    , pcPermuteArgs   :: Bool
      -- ^ @True@ means `System.Console.GetOpt`'s @Permute@, @False@ means @RequireOrder@
    }

-- | Default option parsing configuration
defaultParsingConf :: ParsingConf
defaultParsingConf = ParsingConf { pcUsageHeader   = "USAGE: " ++ unsafePerformIO getProgName ++ " [FLAGS]"
                                 , pcHelpFlag      = Just "help"
                                 , pcHelpExtraInfo = ""
                                 , pcPermuteArgs   = True
                                 }

parseOptions :: (Configurable t) =>
                [String]     -- ^ raw arguments
             -> IO (t, [String])
parseOptions = parseOptions' defaultParsingConf

-- | Run parser, return configured options environment and arguments
parseOptions' :: (Configurable t) =>
                 ParsingConf  -- ^ parsing configuration
              -> [String]     -- ^ raw arguments
              -> IO t
parseOptions' conf rawArgs = do
  let helpStr = init $ unlines [ pcUsageHeader conf
                               , ""
                               , pcHelpExtraInfo conf
                               ]
      usageStr = usageInfo helpStr optdescr
      showHelp opts = do
        putStrLn $ usageStr
        exitWith ExitSuccess
        return opts
      helpdescr = case pcHelpFlag conf of
                    Just flag -> [ Option [] [flag] (NoArg showHelp) "print the help message and exit." ]
                    Nothing -> []
      parsers = runOptM descOptions

      optdescr = helpdescr ++ concatMap (\x -> case x of {Opt d -> [d]; _ -> []} ) parsers

      argparsers = concatMap (\x -> case x of {(Arg p) -> [p]; _ -> []} ) parsers

  let (actions, args, msgs) = getOpt Permute optdescr rawArgs
      argActions = parseArgs args argparsers

  mapM_ (error . flip (++) usageStr) msgs

  opts <- foldl' (>>=) (return defOptions) (actions) -- ++ argActions)
  return opts


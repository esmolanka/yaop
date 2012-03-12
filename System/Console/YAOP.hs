{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell #-}

------------------------------------------------------------------------
-- |
-- Module      :  System.Console.YAOP
-- Copyright   :  2011 Eugene Smoalnka
-- License     :  BSD-style
-- Maintainer  :  smolanka.zhacka@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- YAOP is a library for options parsings that uses base
-- 'System.Console.GetOpt' as a backend.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import System
-- > import System.Environment
-- > import System.Console.YAOP
-- >
-- > import Data.List
-- > import Data.Maybe
-- >
-- > -- | Options that are not mapped to data
-- > withoutData = dummy =: option [] ["action"] NoA "Do some action" (\_ _ -> putStrLn "IO Action")
-- >
-- > -- | Options data structure. Should use record syntax, may have more than one constructor
-- > data Options = Options { optFileName :: FilePath
-- >                        , optCount :: Int
-- >                        , optStuff :: [Either Int String]
-- >                        } deriving (Show)
-- >
-- > -- | Default options
-- > defOptions = Options {optFileName = "default.txt", optCount = 0, optStuff = []}
-- >
-- > -- | This triggers YAOP's accessors generator, e.g.
-- > -- @modM_optFileName :: Monad m => (FilePath -> m FilePath) -> Options -> m Options@
-- > $(deriveModM ''Options)
-- >
-- > -- | Here we define a list of options that are mapped to Options
-- > optDesc = do
-- >   modM_optFileName =: option ['f'] ["filename"] (ReqA "FN")
-- >                       "Set some filename"
-- >                       (\arg x -> print arg >> return (fromMaybe "" arg))
-- >   modM_optCount    =: option ['c'] ["count"] (OptA "N")
-- >                       "Set some count"
-- >                       (\arg x -> return $ fromMaybe 100 (read `fmap` arg))
-- >   modM_optStuff    =: option ['s'] ["stuff"] NoA
-- >                       "Push \"foo\" to a list"
-- >                        (\arg x -> return (Right "foo" : x))
-- >
-- > bothDesc = withoutData >> optDesc
-- >
-- > main = do
-- >   (opts,args) <- parseOptions bothDesc defOptions defaultParsingConf =<< getArgs
-- >   print opts
-- >   print args

module System.Console.YAOP
       ( -- * TH selectors generator
         deriveModM
         -- * Construtors
       , ArgReq (..)
       , Opt
       , OptM
       , option
         -- * Combine
       , (=:)
         -- * Selectors
       , dummy
       , firstM
       , secondM
         -- * Runner
       , ParsingConf (..)
       , defaultParsingConf
       , parseOptions
       )
    where

import System.Exit
import System.Console.GetOpt

import Control.Monad.Writer

import Data.List

import Language.Haskell.TH

mkModM :: Name -> Q Dec
mkModM fname = do
  let modName = mkName ("modM_" ++ nameBase fname)
  fn <- newName "fn"
  rec <- newName "rec"
  val <- newName "val"
  body <- [|let set = $(return $ LamE [VarP val] (RecUpdE (VarE rec) [(fname,VarE val)])) in $(return $ VarE fn) ($(return $ VarE fname) $(return $ VarE rec)) >>= return . set|]
  return $ FunD modName [Clause [VarP fn,VarP rec] (NormalB body) []]

-- | Generate functions with @(a -> m a) -> rec -> rec@ type for all
-- fields of the specified record.
deriveModM :: Name -> Q [Dec]
deriveModM t = do
  TyConI (DataD _ _ _ constructors _) <- reify t
  let mkFieldsModM :: Con -> Q [Dec]
      mkFieldsModM (RecC name fields) = do
                 let fnames = map (\(name,_,_) -> name) fields
                 mapM mkModM fnames
      mkFieldsModM _ = error "Only records are supported"
  decs <- mapM mkFieldsModM constructors
  return (concat decs)

-- | Specifies if argument is required, optional or not necessary
data ArgReq = NoA | OptA String | ReqA String deriving (Show)

data Opt a = Opt String [String] ArgReq String (Maybe String -> a -> IO a)

instance Show (Opt a) where
  show (Opt s l r h _) = "Opt " ++ unwords [show s, show l, show r, show h] ++ " <fn>"

-- | Smart option constructor
option :: String     -- ^ short option, e.g.: @['a']@
       -> [String]   -- ^ long option, e.g.: @[\"add\"]@
       -> ArgReq     -- ^ specify if argument is required
       -> String     -- ^ help message
       -> (Maybe String -> a -> IO a) -- ^ a function that takes an argument and modifies selected field
       -> OptM a ()
option s l r h f = tell [ Opt s l r h f ]

newtype OptM a r = OptM (Writer [Opt a] r) deriving (Monad, MonadWriter [Opt a])
runOptM (OptM writer) = execWriter writer

-- | Dummy selector, selects nothing. Useful for some @--help@ options.
dummy :: Monad m => (() -> m a) -> b -> m b
dummy f t = f () >> return t

-- | Monadic action over the first element, useful as selector.
firstM :: Monad m => (t -> m t1) -> (t, t2) -> m (t1, t2)
firstM  f (x,y) = f x >>= \x' -> return (x', y)

-- | Monadic action over the second element, useful as selector.
secondM :: Monad m => (t -> m t2) -> (t1, t) -> m (t1, t2)
secondM f (x,y) = f y >>= \y' -> return (x, y')

-- | Apply selector to options combinator
(=:) :: (MonadWriter [Opt t] (OptM t)) =>
        ((t -> IO t) -> a -> IO a) -- ^ selector
     -> OptM t ()                  -- ^ options
     -> OptM a ()
(=:) f optm = do
  let os  = runOptM optm
      os' = map (fmapM f) os
  tell os'
  where
    fmapM f (Opt s l r h x) = Opt s l r h (\arg a -> f (x arg) a)

genOptDescr :: [Opt a] -> [OptDescr (a -> IO a)]
genOptDescr = let arg (NoA) f = NoArg (f Nothing)
                  arg (OptA h) f = OptArg f h
                  arg (ReqA h) f = ReqArg (f . Just) h
                  convert (Opt s l r h f) = Option s l (arg r f) h
              in map convert

data ParsingConf = ParsingConf { pcUsageHeader   :: String        -- ^ Usage message header
                               , pcHelpFlag      :: Maybe String  -- ^ Name of help message flag, default: @\"help\"@
                               , pcHelpExtraInfo :: String        -- ^ Extra help information
                               , pcPermuteArgs   :: Bool          -- ^ @True@ means `System.Console.GetOpt`'s @Permute@, @False@ means @RequireOrder@
                               }

-- | Default option parsing configuration
defaultParsingConf :: ParsingConf
defaultParsingConf = ParsingConf { pcUsageHeader   = "USAGE: ... [FLAGS]"
                                 , pcHelpFlag      = Just "help"
                                 , pcHelpExtraInfo = ""
                                 , pcPermuteArgs   = True
                                 }

-- | Run parser, return configured options environment and arguments
parseOptions :: OptM t ()    -- ^ options for datatype @t@
             -> t            -- ^ initial environment
             -> ParsingConf  -- ^ parsing configuration
             -> [String]     -- ^ raw arguments
             -> IO (t, [String])
parseOptions options defaultOptions conf rawArgs = do
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
      optdescr = helpdescr ++ genOptDescr (runOptM options)
      argorder = case pcPermuteArgs conf of
                  True -> Permute
                  False -> RequireOrder
  let (actions, args, msgs) = getOpt argorder optdescr rawArgs
  mapM_ (error . flip (++) usageStr) msgs
  opts <- foldl' (>>=) (return defaultOptions) actions
  return (opts, args)


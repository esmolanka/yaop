{-# LANGUAGE FlexibleContexts, FlexibleInstances, DefaultSignatures
  , ViewPatterns, RankNTypes, TypeOperators, DataKinds, KindSignatures, PolyKinds
  , MultiParamTypeClasses, TypeFamilies #-}

module System.Console.YAOP
    ( module System.Console.YAOP.Selector
    , module System.Console.YAOP.TH
    , module System.Console.YAOP.Argument
    , module System.Console.YAOP.Types
    , (<>) -- from Data.Monoid
    , (<=:)
    , short
    , long
    , metavar
    , help
    , setter
    , set
    , setConst
    , action
    , tweak
    , append
    , prepend
    , appendMap
    , (=:)
    , argument
--    , Annotated
--    , AnnotatedArg
--    , Ann
--    , Ann2
--    , ann
    , Configurable (..)
    , ParsingConf (..)
    , defaultParsingConf
    , withOptions
    , getOptions
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

import Control.Monad.Writer

import Data.Tagged
import Data.Typeable
import Data.Default
import Data.Maybe
import Data.List
import qualified Data.Map as M

import GHC.TypeLits

data OptBuilder arg a = OptBuilder
    { obShort   :: Maybe Char
    , obLong    :: Maybe String
    , obMetavar :: Maybe String
    , obDescr   :: Maybe String
    , obSetter  :: (arg -> a -> IO a)
    }

newtype OptBuilding arg a = OptBuilding (OptBuilder arg a -> OptBuilder arg a)

instance Monoid (OptBuilding arg a) where
    mempty = OptBuilding id
    mappend (OptBuilding f) (OptBuilding g) = OptBuilding (f . g)

short :: Char -> OptBuilding arg a
short f = OptBuilding (\b -> b {obShort = Just f})

long :: String -> OptBuilding arg a
long  f = OptBuilding (\b -> b {obLong  = Just f})

metavar :: String -> OptBuilding arg a
metavar v = OptBuilding (\b -> b {obMetavar = Just v})

help :: String -> OptBuilding arg a
help d    = OptBuilding (\b -> b {obDescr = Just d})

setter :: (arg -> a -> IO a) -> OptBuilding arg a
setter s = OptBuilding (\b -> b {obSetter = s})

set :: OptBuilding a a
set = setter (\arg _ -> return arg)

setConst :: a -> OptBuilding () a
setConst c = setter (\() _ -> return c)

action :: IO a -> OptBuilding () a
action act = setter (\() _ -> act)

tweak :: (arg -> a -> a) -> OptBuilding arg a
tweak f = setter (\a acc -> return (f a acc))

append :: (Monoid a, Argument arg, Singleton arg a) =>
           OptBuilding arg a
append = setter (\a acc -> return $ acc <> singleton a)

prepend :: (Monoid a, Argument arg, Singleton arg a) =>
           OptBuilding arg a
prepend = setter (\a acc -> return $ singleton a <> acc)

appendMap :: (Monoid a, Argument arg) =>
              (arg -> IO a) -> OptBuilding arg a
appendMap f = setter (\a acc -> f a >>= \x -> return $ acc <> x)

infix 0 <=:

(<=:) :: Argument arg =>
         ((t -> IO t) -> a -> IO a)
      -> OptBuilding arg t
      -> OptM a ()
(<=:) sel builder = sel =: pushOption builder
    where
      defOptBuilder = OptBuilder
                        Nothing
                        Nothing
                        Nothing
                        Nothing
                        (error "Parser does not have an action")
      pushOption (OptBuilding builder) =
          let (OptBuilder short long metavar help setter) = builder defOptBuilder
          in  tell [ Opt $ Option
                             (maybeToList short)
                             (maybeToList long)
                             (argDescr setter metavar)
                             (fromMaybe "(no description)" help)
                   ]

-- | Apply selector to options combinator
(=:) :: (MonadWriter [Opt t] (OptM t)) =>
        ((t -> IO t) -> a -> IO a)  -- ^ selector
     -> OptM t ()                   -- ^ options
     -> OptM a ()
(=:) f optm = tell . map (fmap f) $ runOptM optm


newtype FreeArg a = FreeArg a


type a :? s = Tagged (Sing s) a

{-

class Annotatable a b where
    type Ann a b s

instance Annotatable a a where
    type Ann a a s = a :? s -> a

instance Annotatable (FreeArg a) a where
    type Ann (FreeArg a) a s = (FreeArg a) :? s -> a


type Ann2 s = forall a b. Ann a b s

ann2 = unTagged
ann3 = argument . unTagged
-}

class Configurable a where
    defConf :: a

    default defConf :: (Default a) => a
    defConf = def

    signature :: a -> String

    default signature :: (Typeable a) => a -> String
    signature = (++ "]") . ("[" ++) . show . typeOf

    parseOpts :: OptM a ()

argument :: FreeArg t -> t
argument (FreeArg a) = a

instance (Configurable a, SingRep s String) => Configurable (a :? s) where
    defConf = Tagged defConf
    signature a = let singOf :: (SingI s) => (Tagged (Sing s) b) -> Sing s
                      singOf _ = sing
                  in fromSing $ singOf a
    parseOpts = (\f x -> f (unTagged x) >>= return . Tagged) =: parseOpts

instance (Argument a) => Configurable (FreeArg a) where
    defConf = let it = error $ "Required argument is missing: " ++ signature it in it
    signature _ = "ARG"
    parseOpts = tell [ Arg $ ArgParse OneReq (\str l -> return $ FreeArg $ either error id $ parseArg str) ]


instance Configurable [String] where
    defConf = []
    signature _ = "ARGS"
    parseOpts = tell [ Arg $ ArgParse Many (\str l -> return $ l ++ [str] ) ]

instance (Configurable a, Configurable b) => Configurable (a, b) where
    defConf = (defConf, defConf)
    signature (a, b) = intercalate " " [signature a, signature b]
    parseOpts = do
      firstM =: parseOpts
      secondM =: parseOpts

instance (Configurable a, Configurable b, Configurable c) => Configurable (a, b, c) where
    defConf = (defConf, defConf, defConf)
    signature (a, b, c) = intercalate " " [signature a, signature b, signature c]
    parseOpts = do
      (\f (x, y, z) -> f x >>= \q -> return (q, y, z)) =: parseOpts
      (\f (x, y, z) -> f y >>= \q -> return (x, q, z)) =: parseOpts
      (\f (x, y, z) -> f z >>= \q -> return (x, y, q)) =: parseOpts

instance (Configurable a, Configurable b, Configurable c, Configurable d) => Configurable (a, b, c, d) where
    defConf = (defConf, defConf, defConf, defConf)
    signature (a, b, c, d) = intercalate " " [signature a, signature b, signature c, signature d]
    parseOpts = do
      (\f (x, y, z, i) -> f x >>= \q -> return (q, y, z, i)) =: parseOpts
      (\f (x, y, z, i) -> f y >>= \q -> return (x, q, z, i)) =: parseOpts
      (\f (x, y, z, i) -> f z >>= \q -> return (x, y, q, i)) =: parseOpts
      (\f (x, y, z, i) -> f i >>= \q -> return (x, y, z, q)) =: parseOpts

instance ( Configurable a, Configurable b, Configurable c
         , Configurable d, Configurable e) => Configurable (a, b, c, d, e) where
    defConf = (defConf, defConf, defConf, defConf, defConf)
    signature (a, b, c, d, e) = intercalate " " [signature a, signature b, signature c, signature d, signature e]
    parseOpts = do
      (\f (x, y, z, i, j) -> f x >>= \q -> return (q, y, z, i, j)) =: parseOpts
      (\f (x, y, z, i, j) -> f y >>= \q -> return (x, q, z, i, j)) =: parseOpts
      (\f (x, y, z, i, j) -> f z >>= \q -> return (x, y, q, i, j)) =: parseOpts
      (\f (x, y, z, i, j) -> f i >>= \q -> return (x, y, z, q, j)) =: parseOpts
      (\f (x, y, z, i, j) -> f j >>= \q -> return (x, y, z, i, q)) =: parseOpts

{-
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
-}

data ParsingConf = ParsingConf
    { pcProgName      :: String
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
defaultParsingConf = ParsingConf { pcProgName      = unsafePerformIO getProgName
                                 , pcHelpFlag      = Just "help"
                                 , pcHelpExtraInfo = ""
                                 , pcPermuteArgs   = True
                                 }

withOptions :: Configurable t => (t -> IO b) -> IO b
withOptions act = getOptions >>= act

getOptions :: (Configurable t) => IO t
getOptions = parseOptions' defaultParsingConf =<< getArgs

parseOptions :: (Configurable t) =>
                [String]     -- ^ raw arguments
             -> IO t
parseOptions = parseOptions' defaultParsingConf

-- | Run parser, return configured options environment and arguments
parseOptions' :: (Configurable t) =>
                 ParsingConf  -- ^ parsing configuration
              -> [String]     -- ^ raw arguments
              -> IO t
parseOptions' conf rawArgs = do
  let initial = defConf

      usageStr sign = flip usageInfo optdescr . init . unlines $
                        [ "USAGE: " ++ pcProgName conf ++ " " ++ sign
                        , ""
                        , pcHelpExtraInfo conf
                        ]

      showHelp opts = do
        putStrLn $ usageStr (signature initial)
        exitWith ExitSuccess >>= \_ -> return opts

      helpdescr = case pcHelpFlag conf of
                    Just flag -> [ Option [] [flag] (NoArg showHelp) "print this help message and exit." ]
                    Nothing -> []

      parsers = runOptM parseOpts

      optdescr = helpdescr ++ concatMap (\x -> case x of {Opt d -> [d]; _ -> []} ) parsers

      --argparsers = concatMap (\x -> case x of {(Arg p) -> [p]; _ -> []} ) parsers

  let (actions, _args, msgs) = getOpt Permute optdescr rawArgs
      --argActions = parseArgs args argparsers

      groupedByShort = M.fromListWith (++) $ catMaybes $
                       map (\o@(Option (listToMaybe -> s) _ _ _) -> fmap (\x -> ('-':x:[], [o])) s) optdescr
      groupedByLong  = M.fromListWith (++) $ catMaybes $
                       map (\o@(Option _ (listToMaybe -> l) _ _) -> fmap (\x -> ("--"++x, [o])) l) optdescr

      duplicates = M.keys . M.filter (>1) . M.map length $ (M.unionWith (++) groupedByShort groupedByLong)

  unless (null duplicates) $
       error $ "Parsing error. Duplicate flags defined: " ++ intercalate ", " duplicates

  mapM_ (error . flip (++) (usageStr (signature initial))) msgs

  opts <- foldl' (>>=) (return initial) (actions) -- ++ argActions)
  return opts


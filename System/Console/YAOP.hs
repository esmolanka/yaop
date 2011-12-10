{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, ImpredicativeTypes, NoMonomorphismRestriction #-}

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
--   > {-# LANGUAGE TemplateHaskell #-}
--   >
--   > import System
--   > import System.Console.YAOP -- that parsing module
--   >
--   > import Data.List
--   > import Data.Maybe
--   >
--   > data Rec1 = Rec1 {rcField1 :: String, rcField2 :: Int} deriving (Show)
--   > $(deriveModM ''Rec1)
--   >
--   > defaultOptions = ((Rec1 "" 0), ())
--   >
--   > example = concat [
--   >  dummy =:: [ option [] ["help"] (NoA) "Help msg" (\_ _ -> print "help msg") ],
--   >  firstM =:: [
--   >    modM_rcField1 =: option ['a'] ["append"] (ReqA "X") "Append" (\s f -> return (f ++ (fromMaybe "" s))),
--   >    modM_rcField2 =: option ['r'] ["read"] (ReqA "Y") "Read" (\s f -> return (read $ fromMaybe "" s))
--   >    ],
--   >  secondM =:: [ option ['n'] ["no-arg"] (NoA) "Some action" (\_ _ -> print "action!") ]
--   >  ]
--   >
--   > main = do
--   >   (opts,args) <- parseOptions example defaultOptions "" =<< getArgs
--   >   print opts
--   >   print args
--   >

module System.Console.YAOP
       ( -- * TH selectors generator
         deriveModM
         -- * Construtors
       , ArgReq (..)
       , option
         -- * Combine
       , (=:)
       , (=::)
         -- * Selectors
       , dummy
       , firstM
       , secondM
         -- * Runner
       , parseOptions
       )
    where

import System.Console.GetOpt
import Data.List

import Language.Haskell.TH

mkModM :: Name -> Q Dec
mkModM fname = do
  let modName = mkName ("modM_" ++ nameBase fname)
  fn <- newName "fn"
  rec <- newName "rec"
  val <- newName "val"
  body <- [|let set = $(return $ LamE [VarP val] (RecUpdE (VarE rec) [(fname,VarE val)])) in $(return $ VarE fn) ($(return $ VarE fname) $(return $ VarE rec)) >>= return . set|]
  return $ FunD modName [Clause [VarP fn,VarP rec] (NormalB (body)) []]

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
  show (Opt s l r h _) = "Opt " ++ intercalate " " [show s, show l, show r, show h] ++ " <fn>"

-- | Smart option constructor
option :: String     -- ^ short option, e.g.: @['a']@
       -> [String]   -- ^ long option, e.g.: @[\"add\"]@
       -> ArgReq     -- ^ specify if argument is required
       -> String     -- ^ help message
       -> (Maybe String -> a -> IO a) -- ^ a function that takes an argument and modifies selected field
       -> Opt a
option s l r h f = Opt s l r h f

-- | Dummy selector, selects nothing. Useful for some @--help@ options.
dummy :: Monad m => (() -> m a) -> b -> m b
dummy f t = f () >> return t

-- | Monadic action over the first element, useful as selector.
firstM :: Monad m => (t -> m t1) -> (t, t2) -> m (t1, t2)
firstM  f (x,y) = f x >>= \x' -> return (x', y)

-- | Monadic action over the second element, useful as selector.
secondM :: Monad m => (t -> m t2) -> (t1, t) -> m (t1, t2)
secondM f (x,y) = f y >>= \y' -> return (x, y')

-- | Combines a selector and a single option.
(=:) :: ((t -> IO t) -> a -> IO a) -> Opt t -> Opt a
(=:) f op = fmapM f op
  where
    fmapM f (Opt s l r h x) = Opt s l r h (\arg a -> f (x arg) a)

-- | Combines a selector and a list of options, that could also be selected with @(=::)@ or @(=:)@.
(=::) :: ((t -> IO t) -> a -> IO a) -> [Opt t] -> [Opt a]
(=::) f ops = map (\op -> f =: op) ops

genOptDescr :: [Opt a] -> [OptDescr (a -> IO a)]
genOptDescr = let arg (NoA) f = NoArg (f Nothing)
                  arg (OptA h) f = OptArg f h
                  arg (ReqA h) f = ReqArg (\s -> f (Just s)) h
                  convert (Opt s l r h f) = Option s l (arg r f) h
              in map convert

-- | Run parser, return configured options environment and arguments
parseOptions :: [Opt t]      -- ^ options for datatype @t@
             -> t            -- ^ initial environment
             -> String       -- ^ usage header
             -> [String]     -- ^ raw arguments
             -> IO (t, [String])
parseOptions options defaultOptions usageHeader rawArgs = do
  let optdescr = genOptDescr options
  let (actions, args, msgs) = getOpt Permute optdescr rawArgs
  mapM_ (error . (flip (++)) (usageInfo usageHeader optdescr) ) msgs
  opts <- foldl' (>>=) (return defaultOptions) actions
  return (opts, args)


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Console.YAOP.Types where

import System.Console.GetOpt

import Control.Applicative

import Control.Monad.Writer

type Opt a = Parse (a -> IO a)

data ArgMode = OneReq | OneOpt | Many

data ArgParse a = ArgParse ArgMode (String -> a)

data Parse a = ParseOpt (OptDescr a)
             | ParseArg (ArgParse a)

{-
instance Functor ArgDescr where
    fmap f (NoArg a) = NoArg (f a)
    fmap f (ReqArg fa help) = ReqArg (f . fa) help
    fmap f (OptArg fa help) = OptArg (f . fa) help
-}

instance Show (ArgDescr a) where
    show (NoArg _) = "NoArg <f>"
    show (ReqArg _ help) = "ReqArg <f> " ++ show help
    show (OptArg _ help) = "OptArg <f> " ++ show help

instance Functor ArgParse where
    fmap f (ArgParse mode g) = ArgParse mode (f . g)

instance Functor Parse where
    fmap f (ParseOpt descr) = ParseOpt (fmap f descr)
    fmap f (ParseArg parse) = ParseArg (fmap f parse)

{-
instance Functor OptDescr where
    fmap f (Option s l arg h) = Option s l (fmap f arg) h
-}

newtype OptM a r = OptM (Writer [Opt a] r) deriving (Functor, Monad, Applicative, MonadWriter [Opt a])

runOptM :: OptM t a -> [Opt t]
runOptM (OptM writer) = execWriter writer

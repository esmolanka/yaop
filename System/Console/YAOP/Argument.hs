{-# LANGUAGE OverlappingInstances, FlexibleContexts
  , FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies
  , DefaultSignatures #-}

module System.Console.YAOP.Argument
    ( Argument (..)
    , Singleton (..)
    ) where

import System.Console.GetOpt

import System.Console.YAOP.Types

import Control.Applicative

import Data.Typeable
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

import Data.List.Split (splitOn)
import Data.String
import Data.Monoid

tryRead :: (Read a) => String -> Either String a
tryRead str =
    case reads str of
      [(res, [])] -> return res
      _ -> Left $ "Cannot parse " ++ show str

parseError :: [Char] -> [Char] -> a
parseError metavar msg = error $ "Error parsing " ++ metavar ++ ": " ++ msg

data Proxy ix = Proxy

proxy :: ix -> Proxy ix
proxy _ = Proxy

unProxy :: (Proxy ix) -> ix
unProxy = undefined

arr1Type :: (a -> b) -> Proxy a
arr1Type = const Proxy

class (Typeable a) => Argument a where
    parseArg :: String -> Either String a

    defMetavar :: Proxy a -> String
    default defMetavar :: (Typeable a) => Proxy a -> String
    defMetavar p = show . typeOf $ unProxy p

    argDescr :: (a -> b -> IO b) -> Maybe String -> ArgDescr (b -> IO b)
    argDescr f metavar =
        let metavar' = fromMaybe (defMetavar $ arr1Type f) metavar
        in  ReqArg (\str -> f (either (parseError metavar') id $ parseArg str)) metavar'

instance Argument a => Argument (Maybe a) where
    parseArg str = Just `fmap` parseArg str
    argDescr f metavar =
        let unMaybeProxy :: Proxy (Maybe a) -> Proxy a
            unMaybeProxy _ = Proxy
            metavar' = fromMaybe (defMetavar $ unMaybeProxy $ arr1Type f) metavar
        in  OptArg (\mstr -> f (fmap (either (parseError metavar') id . parseArg) mstr)) metavar'

instance Argument String where
    defMetavar _ = "String"
    parseArg = return

instance Argument Bool where
    parseArg s | s `elem` ["yes", "1", "true"] = return True
               | s `elem` ["no", "0", "false"] = return False
               | otherwise = Left $ "Cannot parse boolean " ++ show s
    argDescr f metavar =
        let metavar' = fromMaybe "Bool" metavar
        in OptArg (\mstr -> f (maybe True (either (parseError metavar') id . parseArg) mstr)) metavar'

instance Argument Int where
    parseArg = tryRead

instance Argument Float where
    parseArg = tryRead

instance Argument () where
    parseArg = const (Right ())
    argDescr f _ = NoArg (f ())

instance (Argument a) => Argument [a] where
    parseArg str = mapM parseArg $ splitOn "," str

instance (Argument a, Argument b) => Argument (a, b) where
    defMetavar p = let ~(a, b) = unProxy p
                   in defMetavar (proxy a) ++ "=" ++ defMetavar (proxy b)
    parseArg str = case splitOn "=" str of
                     [a, b] -> (,) <$> parseArg a <*> parseArg b
                     _ -> Left $ "Expected pair instead of " ++ show str

instance (Argument a, Argument b, Argument c) => Argument (a, b, c) where
    parseArg str = case splitOn "," str of
                     [a, b, c] -> (,,) <$> parseArg a <*> parseArg b <*> parseArg c
                     _ -> Left $ "Expected triple instead of " ++ show str

instance (Argument a, Argument b, Argument c, Argument d) => Argument (a, b, c, d) where
    parseArg str = case splitOn "," str of
                     [a, b, c, d] -> (,,,) <$> parseArg a <*> parseArg b <*> parseArg c <*> parseArg d
                     _ -> Left $ "Expected 4-field tuple instead of " ++ show str

class Singleton s c | c -> s where
    singleton :: s -> c

instance (Ord k) => Singleton (k, v) (M.Map k v) where
    singleton (k, v) = M.singleton k v

instance (Ord a) => Singleton a (S.Set a) where
    singleton = S.singleton

instance Singleton a [a] where
    singleton = (:[])

instance Singleton a (Product a) where
    singleton = Product

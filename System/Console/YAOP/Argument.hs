{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module System.Console.YAOP.Argument where

import System.Console.GetOpt

import System.Console.YAOP.Types

import Control.Applicative

import Data.List.Split (splitOn)
import Data.String

tryRead :: (Read a) => String -> Either String a
tryRead str =
    case reads str of
      [(res, [])] -> return res
      _ -> Left $ "Cannot parse " ++ show str

parseError metavar msg = error $ "Error parsing " ++ metavar ++ ": " ++ msg

class Argument a where
    parseArg :: String -> Either String a

    argDescr :: (a -> b -> IO b) -> String -> ArgDescr (b -> IO b)
    argDescr f metavar = ReqArg (\str -> f (either (parseError metavar) id $ parseArg str)) metavar

    argParse :: (a -> b -> IO b) -> ArgParse (b -> IO b)
    argParse f = ArgParse OneReq (\str -> f (either (parseError "argument") id $ parseArg str))

instance Argument a => Argument (Maybe a) where
    parseArg str = Just `fmap` parseArg str
    argDescr f metavar = OptArg (\mstr -> f (fmap (either (parseError metavar) id . parseArg) mstr)) metavar

instance Argument String where
    parseArg = return

instance Argument Bool where
    parseArg s | s `elem` ["yes", "1", "true"] = return True
               | s `elem` ["no", "0", "false"] = return False
               | otherwise = Left $ "Cannot parse boolean " ++ show s
    argDescr f metavar = OptArg (\mstr -> f (maybe True (either (parseError metavar) id . parseArg) mstr)) metavar

instance Argument Int where
    parseArg = tryRead

instance Argument Float where
    parseArg = tryRead

instance Argument () where
    parseArg = const (Right ())
    argDescr f _ = NoArg (f ())

{-
instance (Argument a) => Argument [a] where
    parseArg str = mapM parseArg $ splitOn "," str
-}

instance (Argument a, Argument b) => Argument (a, b) where
    parseArg str = case splitOn "," str of
                     [a, b] -> (,) <$> parseArg a <*> parseArg b
                     _ -> Left $ "Wrong number of elements in " ++ show str

instance (Argument a, Argument b, Argument c) => Argument (a, b, c) where
    parseArg str = case splitOn "," str of
                     [a, b, c] -> (,,) <$> parseArg a <*> parseArg b <*> parseArg c
                     _ -> Left $ "Wrong number of elements in " ++ show str

instance (Argument a, Argument b, Argument c, Argument d) => Argument (a, b, c, d) where
    parseArg str = case splitOn "," str of
                     [a, b, c, d] -> (,,,) <$> parseArg a <*> parseArg b <*> parseArg c <*> parseArg d
                     _ -> Left $ "Wrong number of elements in " ++ show str

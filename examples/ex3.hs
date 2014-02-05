{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RecordWildCards, TypeOperators, DataKinds #-}

import System.Environment
import System.Directory

import System.Console.YAOP
import System.Console.YAOP.TH

import Control.Applicative

import Data.Monoid

import qualified Data.Map as M

data Options = Options { optFoo :: Product Int
                       , optBar :: Maybe Int
                       , optDict :: M.Map String Int
                       , optSources  :: [FilePath]
                       , optThings :: First String
                       } deriving (Show)

makeSetters ''Options

instance Configurable Options where
    defConf = Options { optFoo = mempty
                      , optBar = Nothing
                      , optDict = M.empty
                      , optSources = []
                      , optThings = First Nothing
                      }
    signature _ = "[FLAGS]"
    parseOpts = do
      _optFoo     <=: append <> short 'F' <> long "foo" <> help "Set foo"
      _optDict    <=: prepend <> short 'a' <> long "add" <> help "Add environment variable"
      _optSources <=: append <> short 'S' <> help "Include file"
      _optBar     <=: tweak (\x acc -> Just $ maybe x (x +) acc) <> long "bar"
      _optThings  <=: long "add-existing-file"
                      <> help "Add include path"
                      <> appendMap (\(path :: String) -> doesFileExist path >>=
                                    \x -> return $ if x then First (Just path) else First Nothing)


test :: [String] -> IO ()
test args = withArgs args main

main :: IO ()
main = withOptions $ \
     ( opts :: Options
     , argument -> arg :: (String, Int)
     ) -> do
  print $ opts
  print $ arg

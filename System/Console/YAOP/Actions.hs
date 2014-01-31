{-# LANGUAGE ExistentialQuantification #-}
module System.Console.YAOP.Actions where
{-
    ( Application (..)
    , Action (..)
    , runApplication
    ) where

import System.Console.YAOP
import System.Console.YAOP.Types

data Application g
    = Application { appName :: String
                  , appActions :: [Action g]
                  , appGlobalDesc :: OptM g ()
                  , appGlobalDefs :: g
                  , appLongDesc :: String
                  }

data Action g
    = forall l.
      Action { actName :: String
             , actOptDesc ::  (OptM l ())
             , actDefOpts :: l
             , actAction :: (g -> l -> [String] -> IO ())
             , actShortDesc :: String
             , actLongDesc :: String
             }

indentBy str width = unlines . map (replicate width ' ' ++) . lines $ str

runAction :: String -> g -> Action g -> [String] -> IO ()
runAction appname global (Action name desc defs act _ help) rawArgs = do
  let helpMessage = init $ unlines [ help `indentBy` 2
                            , "Flags:"
                            ]
      conf = defaultParsingConf { pcUsageHeader = "USAGE: " ++ unwords [appname, name] ++ " [FLAGS]"
                                , pcHelpExtraInfo = helpMessage
                                }
  (opts, args) <- parseOptions desc defs conf rawArgs
  act global opts args

-- | Runs application: parses args, looks up an action and runs it
runApplication :: Application g -> [String] -> IO ()
runApplication (Application name actions desc defs help) rawArgs = do
  let maxNameLength = maximum $ map (length . actName) actions
      showActionShortDesc act = let spaces = replicate (maxNameLength + 4 - length (actName act)) ' '
                                in actName act ++ spaces ++ actShortDesc act

      helpMessage = init $ unlines [ help `indentBy` 2
                                   , "Actions:"
                                   , (unlines . map showActionShortDesc $ actions) `indentBy` 2
                                   , "Global flags:"
                            ]

      conf = defaultParsingConf { pcUsageHeader = "USAGE: " ++ name ++ " [GLOBAL-FLAGS] ACTION [FLAGS]"
                                , pcHelpExtraInfo = helpMessage
                                , pcPermuteArgs = False
                                }
      actionTable = map (\a -> (actName a, a)) actions

  (global, args) <- parseOptions desc defs conf rawArgs
  case args of
    [] ->
        error $ "Please specify an action. See "++ name ++" --help"
    (aname:aextra) ->
        case aname `lookup` actionTable of
          Nothing -> error $ "Unknown action '" ++ aname ++ "'. See "++name++" --help"
          Just action -> runAction name global action aextra

-}

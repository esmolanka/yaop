
module System.Console.YAOP.Types where


import System.Exit
import System.Console.GetOpt

import System.Console.YAOP.Selector
import System.Console.YAOP.TH

type Opt a = Parse (a -> IO a)

data ArgMode = OneReq | OneOpt | Many

data ArgParse a = ArgParse ArgMode (String -> a)

data Parse a = Opt (OptDescr a)
             | Arg (ArgParse a)

instance Functor ArgDescr where
    fmap f (NoArg a) = NoArg (f a)
    fmap f (ReqArg fa help) = ReqArg (f . fa) help
    fmap f (OptArg fa help) = OptArg (f . fa) help

instance Functor ArgParse where
    fmap f (ArgParse mode g) = ArgParse mode (f . g)

instance Functor Parse where
    fmap f (Opt descr) = Opt (fmap f descr)
    fmap f (Arg parse) = Arg (fmap f parse)

instance Functor OptDescr where
    fmap f (Option s l arg h) = Option s l (fmap f arg) h
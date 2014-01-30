{-# LANGUAGE TemplateHaskell #-}

module System.Console.YAOP.TH where

import Language.Haskell.TH

makeSetter :: Type -> Name -> Type -> Q [Dec]
makeSetter rtyp fname ftyp = do
  let modName = mkName ("_" ++ nameBase fname)
  fn <- newName "fn"
  rec <- newName "rec"
  val <- newName "val"
  body <- [| let set = $(return $ LamE [VarP val] (RecUpdE (VarE rec) [(fname,VarE val)]))
             in $(return $ VarE fn) ($(return $ VarE fname) $(return $ VarE rec)) >>= return . set
           |]

  let typA = mkName "a"
  let typB = mkName "b"

  typ <- [t| ($(return ftyp) -> IO $(return ftyp)) -> $(return rtyp) -> IO $(return rtyp) |]
  return [ SigD modName typ
         , FunD modName [Clause [VarP fn,VarP rec] (NormalB body) []]
         ]

-- | Generate functions with @(a -> m a) -> rec -> m rec@ type for all
-- fields of the specified record.
makeSetters :: Name -> Q [Dec]
makeSetters t = do
  TyConI dt <- reify t

  decs <- case dt of
            DataD _ tyConName _ [RecC _ fields] _ ->
                mapM (\(name, _, ftyp) -> makeSetter (ConT tyConName) name ftyp) fields
            _ ->
                error "Only record data type with one constructor is supported"
  return (concat decs)

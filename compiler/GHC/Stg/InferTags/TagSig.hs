{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'InferTaggedBinders = XLet 'Vanilla

-- We export this type from this module instead of the Types one, because it leaks into various parts of the compiler.
module GHC.Stg.InferTags.TagSig

where

import GHC.Prelude

import GHC.Types.Basic  ( Arity )
import GHC.Types.Var
import GHC.Utils.Outputable
import GHC.Utils.Binary
import GHC.Utils.Panic.Plain

data TagInfo
  = TagDunno
  | TagTuple [TagInfo]  -- Unboxed tuple
  | TagProper           -- Heap pointer to properly-tagged value
  | TagTagged           -- Bottom of the domain.
  deriving (Eq)

instance Outputable TagInfo where
  ppr TagTagged      = text "TagTagged"
  ppr TagDunno       = text "TagDunno"
  ppr TagProper      = text "TagProper"
  ppr (TagTuple tis) = text "TagTuple" <> brackets (pprWithCommas ppr tis)

instance Binary TagInfo where
  put_ bh TagDunno  = putByte bh 1
  put_ bh (TagTuple flds) = putByte bh 2 >> put_ bh flds
  put_ bh TagProper = putByte bh 3
  put_ bh TagTagged = putByte bh 4

  get bh = do tag <- getByte bh
              case tag of 1 -> return TagDunno
                          2 -> TagTuple <$> get bh
                          3 -> return TagProper
                          4 -> return TagTagged
                          _ -> panic ("get TagInfo " ++ show tag)

data TagSig  -- The signature for each binding
  = TagSig !Arity !TagInfo -- TODO: I think we can skip the arity, it should always be available via idArity
                         -- for all cases where we compute it.
  deriving (Eq)

instance Outputable TagSig where
  ppr (TagSig ar ti) = char '<' <> ppr ar <> comma <> ppr ti <> char '>'
instance OutputableBndr (Id,TagSig) where
  pprInfixOcc  = ppr
  pprPrefixOcc = ppr

instance Binary TagSig where
  put_ bh (TagSig arity sig) = put_ bh arity >> put_ bh sig
  get bh = pure TagSig <*> get bh <*> get bh

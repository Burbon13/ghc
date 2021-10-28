{-# LANGUAGE TypeFamilies, DataKinds, GADTs, FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

{-# LANGUAGE UndecidableInstances #-}
 -- To permit: type instance XLet 'InferTaggedBinders = XLet 'Vanilla

-- We export this type from this module instead of the Types one, because it leaks into various parts of the compiler.
module GHC.Stg.InferTags.TagSig

where

import GHC.Prelude

import GHC.Types.Var
import GHC.Utils.Outputable
import GHC.Utils.Binary
import GHC.Utils.Panic.Plain

data TagInfo
  = TagDunno
  | TagTuple [TagInfo]  -- Represent a function/thunk which when evaluated
                        -- will return a Unboxed tuple whos components have
                        -- the given TagInfos.
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

newtype TagSig  -- The signature for each binding, this is a newtype as we might
                -- want to track functions separately in the future.
  = TagSig TagInfo
  deriving (Eq)

instance Outputable TagSig where
  ppr (TagSig ti) = char '<' <> ppr ti <> char '>'
instance OutputableBndr (Id,TagSig) where
  pprInfixOcc  = ppr
  pprPrefixOcc = ppr

instance Binary TagSig where
  put_ bh (TagSig sig) = put_ bh sig
  get bh = pure TagSig <*> get bh

isTaggedSig :: TagSig -> Bool
isTaggedSig (TagSig TagProper) = True
isTaggedSig (TagSig TagTagged) = True
isTaggedSig _ = False

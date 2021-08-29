

-----------------------------------------------------------------------------
--
-- Code generator utilities; mostly monadic
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.TagCheck
  ( emitTagAssertion, emitArgTagCheck
  ) where

import GHC.Prelude

import GHC.StgToCmm.Env
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Utils
import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.Graph as CmmGraph

import GHC.Core.Type
import GHC.Types.Id
import GHC.Utils.Misc
import GHC.Utils.Outputable

import GHC.Core.DataCon
import Control.Monad

-- Perhaps the code below would fit better elseehere like the Utils module
-- but to avoid loops I decided it simplest to put them here.

-- | Call barf if we failed to predict a tag correctly.
-- This is immensly useful when debugging issues in tag inference
-- as it will result in a program abort when we encounter an invalid
-- call/heap object, rather than leaving it be and segfaulting arbitrary
-- long after.
emitTagAssertion :: String -> CmmExpr -> FCode ()
emitTagAssertion onWhat fun = do
  { platform <- getPlatform
  ; lret <- newBlockId
  ; lfault <- newBlockId
  -- ; pprTraceM "emitTagAssertion" (text onWhat)
  ; emit $ mkCbranch (cmmIsTagged platform fun)
                     lret lfault Nothing
  ; emitLabel lfault
  ; emitBarf ("Tag inference failed on:" ++ onWhat)
  ; emitLabel lret
  }

emitArgTagCheck :: Id -> [Id] -> FCode ()
emitArgTagCheck id args = do
  let marks = idCbvMarks_maybe id
  case marks of
    Nothing -> return ()
    Just marks -> do
      let cbv_args = filter (isLiftedRuntimeRep . idType) $ filterByList (map isMarkedStrict marks) args
      arg_infos <- mapM getCgIdInfo cbv_args
      let arg_cmms = map idInfoToAmode arg_infos
          mk_msg arg = showPprUnsafe (text "Untagged arg:" <> ppr id <+> ppr arg)
      zipWithM_ emitTagAssertion (map mk_msg args) (arg_cmms)

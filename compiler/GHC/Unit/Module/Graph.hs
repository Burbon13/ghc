{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Unit.Module.Graph
   ( ModuleGraph
   , ModuleGraphNode(..)
   , emptyMG
   , mkModuleGraph
   , extendMG
   , extendMGInst
   , extendMG'
   , filterToposortToModules
   , mapMG
   , mgModSummaries
   , mgModSummaries'
   , mgElemModule
   , mgLookupModule
   , mgBootModules
   , needsTemplateHaskellOrQQ
   , isTemplateHaskellOrQQNonBoot
   , showModMsg
   , moduleGraphNodeModule
   , moduleGraphNodeUnitId

   , NodeKey(..)
   , ModNodeKeyWithUid(..)
   )
where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Data.Maybe
import GHC.Data.Graph.Directed ( SCC(..) )

import GHC.Driver.Backend
import GHC.Driver.Ppr
import GHC.Driver.Session

import GHC.Types.SourceFile ( hscSourceString )

import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Env
import GHC.Unit.Types
import GHC.Utils.Outputable

import System.FilePath
import GHC.Linker.Static

-- | A '@ModuleGraphNode@' is a node in the '@ModuleGraph@'.
-- Edges between nodes mark dependencies arising from module imports
-- and dependencies arising from backpack instantiations.
data ModuleGraphNode
  -- | Instantiation nodes track the instantiation of other units
  -- (backpack dependencies) with the holes (signatures) of the current package.
  = InstantiationNode UnitId InstantiatedUnit
  -- | There is a module summary node for each module, signature, and boot module being built.
  | ModuleNode [NodeKey] ModSummary
  | LinkNode UnitId [NodeKey]

moduleGraphNodeModule :: ModuleGraphNode -> Maybe ModSummary
moduleGraphNodeModule (InstantiationNode {}) = Nothing
moduleGraphNodeModule (LinkNode {})          = Nothing
moduleGraphNodeModule (ModuleNode _ ems)     = Just ems

moduleGraphNodeUnitId :: ModuleGraphNode -> UnitId
moduleGraphNodeUnitId mgn =
  case mgn of
    InstantiationNode uid _iud -> uid
    ModuleNode _ ems        -> (toUnitId (moduleUnit (ms_mod ems)))
    LinkNode uid _             -> uid

instance Outputable ModuleGraphNode where
  ppr = \case
    InstantiationNode _ iuid -> ppr iuid
    ModuleNode nks ems -> ppr (ms_mod ems ) <+> ppr nks
    LinkNode uid _     -> text "LN:" <+> ppr uid

data NodeKey = NodeKey_Unit {-# UNPACK #-} !InstantiatedUnit
             | NodeKey_Module {-# UNPACK #-} !ModNodeKeyWithUid
             | NodeKey_Link !UnitId
  deriving (Eq, Ord)

instance Outputable NodeKey where
  ppr nk = pprNodeKey nk

pprNodeKey :: NodeKey -> SDoc
pprNodeKey (NodeKey_Unit iu) = ppr iu
pprNodeKey (NodeKey_Module mk) = ppr mk
pprNodeKey (NodeKey_Link uid)  = ppr uid

data ModNodeKeyWithUid = ModNodeKeyWithUid { mnkModuleName :: ModuleNameWithIsBoot
                                           , mnkUnitId     :: UnitId } deriving (Eq, Ord)

instance Outputable ModNodeKeyWithUid where
  ppr (ModNodeKeyWithUid mnwib uid) = ppr uid <> colon <> ppr mnwib

-- | A '@ModuleGraph@' contains all the nodes from the home package (only). See
-- '@ModuleGraphNode@' for information about the nodes.
--
-- Modules need to be compiled. hs-boots need to be typechecked before
-- the associated "real" module so modules with {-# SOURCE #-} imports can be
-- built. Instantiations also need to be typechecked to ensure that the module
-- fits the signature. Substantiation typechecking is roughly comparable to the
-- check that the module and its hs-boot agree.
--
-- The graph is not necessarily stored in topologically-sorted order.  Use
-- 'GHC.topSortModuleGraph' and 'GHC.Data.Graph.Directed.flattenSCC' to achieve this.
data ModuleGraph = ModuleGraph
  { mg_mss :: [ModuleGraphNode]
  , mg_non_boot :: ModuleEnv ModSummary
    -- a map of all non-boot ModSummaries keyed by Modules
  , mg_boot :: ModuleSet
    -- a set of boot Modules
  , mg_needs_th_or_qq :: !Bool
    -- does any of the modules in mg_mss require TemplateHaskell or
    -- QuasiQuotes?
  }

-- | Determines whether a set of modules requires Template Haskell or
-- Quasi Quotes
--
-- Note that if the session's 'DynFlags' enabled Template Haskell when
-- 'depanal' was called, then each module in the returned module graph will
-- have Template Haskell enabled whether it is actually needed or not.
needsTemplateHaskellOrQQ :: ModuleGraph -> Bool
needsTemplateHaskellOrQQ mg = mg_needs_th_or_qq mg

-- | Map a function 'f' over all the 'ModSummaries'.
-- To preserve invariants 'f' can't change the isBoot status.
mapMG :: (ModSummary -> ModSummary) -> ModuleGraph -> ModuleGraph
mapMG f mg@ModuleGraph{..} = mg
  { mg_mss = flip fmap mg_mss $ \case
      InstantiationNode uid iuid -> InstantiationNode uid iuid
      LinkNode uid nks -> LinkNode uid nks
      ModuleNode deps ms  -> ModuleNode deps (f ms)
  , mg_non_boot = mapModuleEnv f mg_non_boot
  }

mgBootModules :: ModuleGraph -> ModuleSet
mgBootModules ModuleGraph{..} = mg_boot

mgModSummaries :: ModuleGraph -> [ModSummary]
mgModSummaries mg = [ m | ModuleNode _ m <- mgModSummaries' mg ]

mgModSummaries' :: ModuleGraph -> [ModuleGraphNode]
mgModSummaries' = mg_mss

mgElemModule :: ModuleGraph -> Module -> Bool
mgElemModule ModuleGraph{..} m = elemModuleEnv m mg_non_boot

-- | Look up a ModSummary in the ModuleGraph
mgLookupModule :: ModuleGraph -> Module -> Maybe ModSummary
mgLookupModule ModuleGraph{..} m = lookupModuleEnv mg_non_boot m

emptyMG :: ModuleGraph
emptyMG = ModuleGraph [] emptyModuleEnv emptyModuleSet False

isTemplateHaskellOrQQNonBoot :: ModSummary -> Bool
isTemplateHaskellOrQQNonBoot ms =
  (xopt LangExt.TemplateHaskell (ms_hspp_opts ms)
    || xopt LangExt.QuasiQuotes (ms_hspp_opts ms)) &&
  (isBootSummary ms == NotBoot)

-- | Add an ExtendedModSummary to ModuleGraph. Assumes that the new ModSummary is
-- not an element of the ModuleGraph.
extendMG :: ModuleGraph -> [NodeKey] -> ModSummary -> ModuleGraph
extendMG ModuleGraph{..} deps ms = ModuleGraph
  { mg_mss = ModuleNode deps ms : mg_mss
  , mg_non_boot = case isBootSummary ms of
      IsBoot -> mg_non_boot
      NotBoot -> extendModuleEnv mg_non_boot (ms_mod ms) ms
  , mg_boot = case isBootSummary ms of
      NotBoot -> mg_boot
      IsBoot -> extendModuleSet mg_boot (ms_mod ms)
  , mg_needs_th_or_qq = mg_needs_th_or_qq || isTemplateHaskellOrQQNonBoot ms
  }

extendMGInst :: ModuleGraph -> UnitId -> InstantiatedUnit -> ModuleGraph
extendMGInst mg uid depUnitId = mg
  { mg_mss = InstantiationNode uid depUnitId : mg_mss mg
  }

extendMGLink :: ModuleGraph -> UnitId -> [NodeKey] -> ModuleGraph
extendMGLink mg uid nks = mg { mg_mss = LinkNode uid nks : mg_mss mg }

extendMG' :: ModuleGraph -> ModuleGraphNode -> ModuleGraph
extendMG' mg = \case
  InstantiationNode uid depUnitId -> extendMGInst mg uid depUnitId
  ModuleNode deps ems -> extendMG mg deps ems
  LinkNode uid deps   -> extendMGLink mg uid deps

mkModuleGraph :: [ModuleGraphNode] -> ModuleGraph
mkModuleGraph = foldr (flip extendMG') emptyMG

-- | This function filters out all the instantiation nodes from each SCC of a
-- topological sort. Use this with care, as the resulting "strongly connected components"
-- may not really be strongly connected in a direct way, as instantiations have been
-- removed. It would probably be best to eliminate uses of this function where possible.
filterToposortToModules
  :: [SCC ModuleGraphNode] -> [SCC ModSummary]
filterToposortToModules = mapMaybe $ mapMaybeSCC $ \case
  InstantiationNode _ _ -> Nothing
  LinkNode{} -> Nothing
  ModuleNode deps node -> Just node
  where
    -- This higher order function is somewhat bogus,
    -- as the definition of "strongly connected component"
    -- is not necessarily respected.
    mapMaybeSCC :: (a -> Maybe b) -> SCC a -> Maybe (SCC b)
    mapMaybeSCC f = \case
      AcyclicSCC a -> AcyclicSCC <$> f a
      CyclicSCC as -> case mapMaybe f as of
        [] -> Nothing
        [a] -> Just $ AcyclicSCC a
        as -> Just $ CyclicSCC as

showModMsg :: DynFlags -> Bool -> ModuleGraphNode -> SDoc
showModMsg dflags _ (LinkNode uid deps) =
      let staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> False

          platform  = targetPlatform dflags
          exe_file  = exeFileName platform staticLink (outputFile_ dflags)
      in text exe_file
showModMsg _ _ (InstantiationNode _uid indef_unit) =
  ppr $ instUnitInstanceOf indef_unit
showModMsg dflags recomp (ModuleNode _ mod_summary) =
  if gopt Opt_HideSourcePaths dflags
      then text mod_str
      else hsep $
         [ text (mod_str ++ replicate (max 0 (16 - length mod_str)) ' ')
         , char '('
         , text (op $ msHsFilePath mod_summary) <> char ','
         , message, char ')' ]

  where
    op       = normalise
    mod      = moduleName (ms_mod mod_summary)
    mod_str  = showPpr dflags mod ++ hscSourceString (ms_hsc_src mod_summary)
    dyn_file = op $ msDynObjFilePath mod_summary
    obj_file = op $ msObjFilePath mod_summary
    message = case backend dflags of
                Interpreter | recomp -> text "interpreted"
                NoBackend            -> text "nothing"
                _                    ->
                  if gopt Opt_BuildDynamicToo  dflags
                    then text obj_file <> comma <+> text dyn_file
                    else text obj_file


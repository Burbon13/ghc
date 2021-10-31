{-# LANGUAGE DeriveTraversable #-}
module GHC.Unit.Env
    ( UnitEnv (..)
    , initUnitEnv
    , unsafeGetHomeUnit
    , updateHug
    , updateHpt
    , mapHomeUnitEnvs
    , mapHomeUnitEnvsM
    -- * Unit Env helper functions
    , ue_units
    , ue_setUnits
    , ue_unit_dbs
    , ue_setUnitDbs
    , ue_hpt
    , ue_dflags
    , ue_homeUnit
    , ue_unsafeHomeUnit
    , ue_setFlags
    , ue_updateFlags
    , ue_setUnitFlags
    , ue_updateUnitFlags
    , ue_setHomeUnit
    , ue_updateHomeUnit
    , ue_updateUnitHomeUnit
    , ue_setUnitHPT
    , ue_updateUnitHPT
    , ue_updateHPT
    , ue_updateHUG
    , ue_currentHomeUnitEnv
    , ue_setActiveUnit
    , ue_currentUnit
    , ue_findHomeUnitEnv_maybe
    , ue_findHomeUnitEnv
    , ue_memberHomeUnitEnv
    , ue_unitHPT_maybe
    , ue_unitHPT
    , ue_unitHomeUnit_maybe
    , ue_unitHomeUnit
    , ue_unitFlags_maybe
    , ue_unitFlags
    , ue_newHomeUnitGraph
    , ue_singletonHomeUnitGraph
    , ue_insertHomeUnitEnv
    , ue_updateHomeUnitEnv
    , ue_renameUnitId
    , ue_HPTs
    , ue_home_unit_graphs
    , ue_allDflags
    , ue_allHomeUnits
    , ue_allUnitIds
    -- * HomeUnitEnv
    , HomeUnitGraph
    , HomeUnitEnv (..)
    , homeUnitEnv_unsafeHomeUnit
    , mkHomeUnitEnv
    , lookupHugByModule
    , lookupHugByModuleButNot
    , hugElts
    , initHomeUnitGraph
    , lookupHug
    , addHomeModInfoToHug
    -- * UnitEnvGraph
    , UnitEnvGraph (..)
    , unitEnv_insert
    , unitEnv_delete
    , unitEnv_adjust
    , unitEnv_new
    , unitEnv_singleton
    , unitEnv_map
    , unitEnv_member
    , unitEnv_lookup_maybe
    , unitEnv_lookup
    , unitEnv_keys
    , unitEnv_hpts
    , unitEnv_foldWithKey
    -- * Assertions
    , assertUnitEnvInvariant
    -- * Preload units info
    , preloadUnitsInfo
    , preloadUnitsInfo'
    -- * Home Module functions
    , isUnitEnvInstalledModule
    )
where

import GHC.Prelude

import GHC.Unit.External
import GHC.Unit.State
import GHC.Unit.Home
import GHC.Unit.Types
import GHC.Unit.Home.ModInfo

import GHC.Platform
import GHC.Settings
import GHC.Data.Maybe
import GHC.Utils.Panic.Plain
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC.Stack.Types (HasCallStack)
import GHC.Driver.Session
import GHC.Utils.Outputable
import GHC.Utils.Panic (pprPanic)
import GHC.Unit.Module.ModIface
import GHC.Unit.Module
import Data.Foldable (asum)
import Data.Coerce

data UnitEnv = UnitEnv
    { ue_eps :: {-# UNPACK #-} !ExternalUnitCache
        -- ^ Information about the currently loaded external packages.
        -- This is mutable because packages will be demand-loaded during
        -- a compilation run as required.

    , ue_current_unit    :: UnitId
    , ue_home_unit_graph :: !HomeUnitGraph

    , ue_platform  :: !Platform
        -- ^ Platform

    , ue_namever   :: !GhcNameVersion
        -- ^ GHC name/version (used for dynamic library suffix)
    }

initUnitEnv :: UnitId -> HomeUnitGraph -> GhcNameVersion -> Platform -> IO UnitEnv
initUnitEnv cur_unit hug namever platform = do
  eps <- initExternalUnitCache
  return $ UnitEnv
    { ue_eps             = eps
    , ue_home_unit_graph = hug
    , ue_current_unit    = cur_unit
    , ue_platform        = platform
    , ue_namever         = namever
    }

-- | Get home-unit
--
-- Unsafe because the home-unit may not be set
unsafeGetHomeUnit :: UnitEnv -> HomeUnit
unsafeGetHomeUnit ue = ue_unsafeHomeUnit ue

updateHpt :: (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
updateHpt = ue_updateHPT

updateHug :: (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
updateHug = ue_updateHUG

mapHomeUnitEnvs :: (HomeUnitEnv -> HomeUnitEnv) -> UnitEnv -> UnitEnv
mapHomeUnitEnvs f ue = ue { ue_home_unit_graph = fmap f (ue_home_unit_graph ue) }

mapHomeUnitEnvsM :: Monad m => (HomeUnitEnv -> m HomeUnitEnv) -> UnitEnv -> m UnitEnv
mapHomeUnitEnvsM f ue = do
  home_unit_graph <- mapM f (ue_home_unit_graph ue)
  pure ue { ue_home_unit_graph = home_unit_graph }

-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of preload (command-line) packages to determine which packages to
-- use.

-- | Lookup 'UnitInfo' for every preload unit from the UnitState, for every unit
-- used to instantiate the home unit, and for every unit explicitly passed in
-- the given list of UnitId.
preloadUnitsInfo' :: UnitEnv -> [UnitId] -> MaybeErr UnitErr [UnitInfo]
preloadUnitsInfo' unit_env ids0 = all_infos
  where
    unit_state = ue_units unit_env
    ids      = ids0 ++ inst_ids
    inst_ids = case ue_homeUnit unit_env of
      Nothing -> []
      Just home_unit
       -- An indefinite package will have insts to HOLE,
       -- which is not a real package. Don't look it up.
       -- Fixes #14525
       | isHomeUnitIndefinite home_unit -> []
       | otherwise -> map (toUnitId . moduleUnit . snd) (homeUnitInstantiations home_unit)
    pkg_map = unitInfoMap unit_state
    preload = preloadUnits unit_state

    all_pkgs  = closeUnitDeps' pkg_map preload (ids `zip` repeat Nothing)
    all_infos = map (unsafeLookupUnitId unit_state) <$> all_pkgs


-- | Lookup 'UnitInfo' for every preload unit from the UnitState and for every
-- unit used to instantiate the home unit.
preloadUnitsInfo :: UnitEnv -> MaybeErr UnitErr [UnitInfo]
preloadUnitsInfo unit_env = preloadUnitsInfo' unit_env []

-- -----------------------------------------------------------------------------

data HomeUnitEnv = HomeUnitEnv
  { homeUnitEnv_units     :: !UnitState
      -- ^ External units

  , homeUnitEnv_unit_dbs :: !(Maybe [UnitDatabase UnitId])
      -- ^ Stack of unit databases for the target platform.
      --
      -- This field is populated with the result of `initUnits`.
      --
      -- 'Nothing' means the databases have never been read from disk.
      --
      -- Usually we don't reload the databases from disk if they are
      -- cached, even if the database flags changed!

  , homeUnitEnv_dflags :: DynFlags
    -- ^ The dynamic flag settings
  , homeUnitEnv_hpt :: HomePackageTable
    -- ^ The home package table describes already-compiled
    -- home-package modules, /excluding/ the module we
    -- are compiling right now.
    -- (In one-shot mode the current module is the only
    -- home-package module, so homeUnitEnv_hpt is empty.  All other
    -- modules count as \"external-package\" modules.
    -- However, even in GHCi mode, hi-boot interfaces are
    -- demand-loaded into the external-package table.)
    --
    -- 'homeUnitEnv_hpt' is not mutable because we only demand-load
    -- external packages; the home package is eagerly
    -- loaded, module by module, by the compilation manager.
    --
    -- The HPT may contain modules compiled earlier by @--make@
    -- but not actually below the current module in the dependency
    -- graph.
    --
    -- (This changes a previous invariant: changed Jan 05.)

  , homeUnitEnv_home_unit :: !(Maybe HomeUnit)
    -- ^ Home-unit
  }

instance Outputable HomeUnitEnv where
  ppr hug = pprHPT (homeUnitEnv_hpt hug)

homeUnitEnv_unsafeHomeUnit :: HomeUnitEnv -> HomeUnit
homeUnitEnv_unsafeHomeUnit hue = case homeUnitEnv_home_unit hue of
  Nothing -> panic "homeUnitEnv_unsafeHomeUnit: No home unit"
  Just h  -> h

mkHomeUnitEnv :: DynFlags -> HomePackageTable -> Maybe HomeUnit -> HomeUnitEnv
mkHomeUnitEnv dflags hpt home_unit = HomeUnitEnv
  { homeUnitEnv_units = emptyUnitState
  , homeUnitEnv_unit_dbs = Nothing
  , homeUnitEnv_dflags = dflags
  , homeUnitEnv_hpt = hpt
  , homeUnitEnv_home_unit = home_unit
  }

-- | Test if the module comes from the home unit
isUnitEnvInstalledModule :: UnitEnv -> InstalledModule -> Bool
isUnitEnvInstalledModule ue m = maybe False (`isHomeInstalledModule` m) hu
  where
    hu = ue_unitHomeUnit_maybe (moduleUnit m) ue


type HomeUnitGraph = UnitEnvGraph HomeUnitEnv

lookupHugByModule :: HasCallStack => Module -> HomeUnitGraph -> Maybe HomeModInfo
lookupHugByModule mod hug
  | otherwise = do
      env <- (unitEnv_lookup_maybe (toUnitId $ moduleUnit mod) hug)
      lookupHptByModule (homeUnitEnv_hpt env) mod

lookupHugByModuleButNot :: UnitId -> Module -> HomeUnitGraph -> Maybe HomeModInfo
lookupHugByModuleButNot cur_unit mod _ | toUnitId (moduleUnit mod) == cur_unit = Nothing
lookupHugByModuleButNot _cur_unit mod hug = lookupHugByModule mod hug

hugElts :: HomeUnitGraph -> [(UnitId, HomeUnitEnv)]
hugElts hug = unitEnv_elts hug

initHomeUnitGraph :: [(UnitId, HomeUnitEnv)] -> HomeUnitGraph
initHomeUnitGraph = UnitEnvGraph . Map.fromList

addHomeModInfoToHug :: HomeModInfo -> HomeUnitGraph -> HomeUnitGraph
addHomeModInfoToHug hmi hug = unitEnv_alter go hmi_unit hug
  where
    hmi_mod :: Module
    hmi_mod = mi_module (hm_iface hmi)

    hmi_unit = toUnitId (moduleUnit hmi_mod)
    _hmi_mn   = moduleName hmi_mod

    go :: Maybe HomeUnitEnv -> Maybe HomeUnitEnv
    go Nothing = pprPanic "addHomeInfoToHug" (ppr hmi_mod)
    go (Just hue) = Just (updateHueHpt (addHomeModInfoToHpt hmi) hue)

updateHueHpt :: (HomePackageTable -> HomePackageTable) -> HomeUnitEnv -> HomeUnitEnv
updateHueHpt f hue = hue { homeUnitEnv_hpt = f (homeUnitEnv_hpt hue)}


lookupHug :: HomeUnitGraph -> ModuleName -> Maybe HomeModInfo
lookupHug hug mod = asum (map (\(_, hue) -> lookupHpt (homeUnitEnv_hpt hue) mod) (hugElts hug))




instance Outputable (UnitEnvGraph elt) where
  ppr (UnitEnvGraph g ) = ppr (Map.keys g)


type UnitEnvGraphKey = UnitId

newtype UnitEnvGraph v = UnitEnvGraph
  { unitEnv_graph :: (Map UnitEnvGraphKey v)
  } deriving (Functor, Foldable, Traversable)

unitEnv_insert :: UnitEnvGraphKey -> v -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_insert unitId env unitEnv = unitEnv
  { unitEnv_graph = Map.insert unitId env (unitEnv_graph unitEnv)
  }

unitEnv_delete :: UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_delete uid unitEnv =
    unitEnv
      { unitEnv_graph = Map.delete uid (unitEnv_graph unitEnv)
      }

unitEnv_adjust :: (v -> v) -> UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_adjust f uid unitEnv = unitEnv
  { unitEnv_graph = Map.adjust f uid (unitEnv_graph unitEnv)
  }

unitEnv_alter :: (Maybe v -> Maybe v) -> UnitEnvGraphKey -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_alter f uid unitEnv = unitEnv
  { unitEnv_graph = Map.alter f uid (unitEnv_graph unitEnv)
  }

unitEnv_new :: Map UnitEnvGraphKey v -> UnitEnvGraph v
unitEnv_new m =
  UnitEnvGraph
    { unitEnv_graph = m
    }

unitEnv_singleton :: UnitEnvGraphKey -> v -> UnitEnvGraph v
unitEnv_singleton active m = UnitEnvGraph
  { unitEnv_graph = Map.singleton active m
  }

unitEnv_map :: (v -> v) -> UnitEnvGraph v -> UnitEnvGraph v
unitEnv_map f m = m { unitEnv_graph = Map.map f (unitEnv_graph m)}

unitEnv_member :: UnitEnvGraphKey -> UnitEnvGraph v -> Bool
unitEnv_member u env = Map.member u (unitEnv_graph env)

unitEnv_lookup_maybe :: UnitEnvGraphKey -> UnitEnvGraph v -> Maybe v
unitEnv_lookup_maybe u env = Map.lookup u (unitEnv_graph env)

unitEnv_lookup :: UnitEnvGraphKey -> UnitEnvGraph v -> v
unitEnv_lookup u env = fromJust $ unitEnv_lookup_maybe u env

unitEnv_keys :: UnitEnvGraph v -> [UnitEnvGraphKey]
unitEnv_keys env = Map.keys (unitEnv_graph env)

unitEnv_elts :: UnitEnvGraph v -> [(UnitEnvGraphKey, v)]
unitEnv_elts env = Map.toList (unitEnv_graph env)

unitEnv_hpts :: UnitEnvGraph HomeUnitEnv -> [HomePackageTable]
unitEnv_hpts env = map homeUnitEnv_hpt (Map.elems (unitEnv_graph env))

unitEnv_foldWithKey :: (b -> UnitEnvGraphKey -> a -> b) -> b -> UnitEnvGraph a -> b
unitEnv_foldWithKey f z (UnitEnvGraph g)= Map.foldlWithKey' f z g

-- -------------------------------------------------------
-- Query and modify UnitState in HomeUnitEnv
-- -------------------------------------------------------

ue_units :: HasCallStack => UnitEnv -> UnitState
ue_units = homeUnitEnv_units . ue_currentHomeUnitEnv

ue_setUnits :: UnitState -> UnitEnv -> UnitEnv
ue_setUnits units ue = ue_updateHomeUnitEnv f (ue_currentUnit ue) ue
  where
    f hue = hue { homeUnitEnv_units = units  }

ue_unit_dbs :: UnitEnv ->  Maybe [UnitDatabase UnitId]
ue_unit_dbs = homeUnitEnv_unit_dbs . ue_currentHomeUnitEnv

ue_setUnitDbs :: Maybe [UnitDatabase UnitId] -> UnitEnv -> UnitEnv
ue_setUnitDbs unit_dbs ue = ue_updateHomeUnitEnv f (ue_currentUnit ue) ue
  where
    f hue = hue { homeUnitEnv_unit_dbs = unit_dbs  }

-- -------------------------------------------------------
-- Query and modify Home Package Table in HomeUnitEnv
-- -------------------------------------------------------

ue_hpt :: HasCallStack => UnitEnv -> HomePackageTable
ue_hpt = homeUnitEnv_hpt . ue_currentHomeUnitEnv

ue_updateHPT :: HasCallStack => (HomePackageTable -> HomePackageTable) -> UnitEnv -> UnitEnv
ue_updateHPT f e = ue_updateUnitHPT f (ue_currentUnit e) e

ue_updateHUG :: HasCallStack => (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
ue_updateHUG f e = ue_updateUnitHUG f e

ue_setUnitHPT :: HasCallStack => UnitId -> HomePackageTable -> UnitEnv -> UnitEnv
ue_setUnitHPT uid hpt e = ue_updateUnitHPT (const hpt) uid e

ue_updateUnitHPT :: HasCallStack => (HomePackageTable -> HomePackageTable) -> UnitId -> UnitEnv -> UnitEnv
ue_updateUnitHPT f uid ue_env = ue_updateHomeUnitEnv update uid ue_env
  where
    update unitEnv = unitEnv { homeUnitEnv_hpt = f $ homeUnitEnv_hpt unitEnv }

ue_updateUnitHUG :: HasCallStack => (HomeUnitGraph -> HomeUnitGraph) -> UnitEnv -> UnitEnv
ue_updateUnitHUG f ue_env = ue_env { ue_home_unit_graph = f (ue_home_unit_graph ue_env)}

ue_unitHPT_maybe :: HasCallStack => UnitId -> UnitEnv -> Maybe HomePackageTable
ue_unitHPT_maybe uid ue_env =
  fmap homeUnitEnv_hpt (ue_findHomeUnitEnv_maybe uid ue_env)

ue_unitHPT :: HasCallStack => UnitId -> UnitEnv -> HomePackageTable
ue_unitHPT uid ue_env = homeUnitEnv_hpt $ ue_findHomeUnitEnv uid ue_env

-- -------------------------------------------------------
-- Query and modify DynFlags in HomeUnitEnv
-- -------------------------------------------------------

ue_dflags :: HasCallStack => UnitEnv -> DynFlags
ue_dflags = homeUnitEnv_dflags . ue_currentHomeUnitEnv

ue_setFlags :: HasCallStack => DynFlags -> UnitEnv -> UnitEnv
ue_setFlags dflags ue_env = ue_setUnitFlags (ue_currentUnit ue_env) dflags ue_env

ue_updateFlags :: HasCallStack => (DynFlags -> DynFlags) -> UnitEnv -> UnitEnv
ue_updateFlags f e = ue_updateUnitFlags f (ue_currentUnit e) e

ue_setUnitFlags :: HasCallStack => UnitId -> DynFlags -> UnitEnv -> UnitEnv
ue_setUnitFlags uid dflags e =
  ue_updateUnitFlags (const dflags) uid e

ue_unitFlags_maybe ::HasCallStack => UnitId -> UnitEnv -> Maybe DynFlags
ue_unitFlags_maybe uid e =
  fmap homeUnitEnv_dflags (ue_findHomeUnitEnv_maybe uid e)

ue_unitFlags :: HasCallStack => UnitId -> UnitEnv -> DynFlags
ue_unitFlags uid ue_env = homeUnitEnv_dflags $ ue_findHomeUnitEnv uid ue_env

ue_updateUnitFlags :: HasCallStack => (DynFlags -> DynFlags) -> UnitId -> UnitEnv -> UnitEnv
ue_updateUnitFlags f uid e = ue_updateHomeUnitEnv update uid e
  where
    update hue = hue { homeUnitEnv_dflags = f $ homeUnitEnv_dflags hue }

-- -------------------------------------------------------
-- Query and modify home units in HomeUnitEnv
-- -------------------------------------------------------

ue_homeUnit :: HasCallStack => UnitEnv -> Maybe HomeUnit
ue_homeUnit = homeUnitEnv_home_unit . ue_currentHomeUnitEnv

ue_unsafeHomeUnit :: HasCallStack => UnitEnv -> HomeUnit
ue_unsafeHomeUnit ue = case ue_homeUnit ue of
  Nothing -> panic "unsafeGetHomeUnit: No home unit"
  Just h  -> h

ue_setHomeUnit :: HasCallStack => HomeUnit -> UnitEnv -> UnitEnv
ue_setHomeUnit homeUnit ue_env = ue_updateUnitHomeUnit (const homeUnit) (ue_currentUnit ue_env) ue_env

ue_updateHomeUnit :: HasCallStack => (HomeUnit -> HomeUnit) -> UnitEnv -> UnitEnv
ue_updateHomeUnit f e = ue_updateUnitHomeUnit f (ue_currentUnit e) e

ue_updateUnitHomeUnit :: HasCallStack => (HomeUnit -> HomeUnit) -> UnitId -> UnitEnv -> UnitEnv
ue_updateUnitHomeUnit f uid e = ue_updateHomeUnitEnv update uid e
  where
    update hue = hue { homeUnitEnv_home_unit = Just (f $ homeUnitEnv_unsafeHomeUnit hue) }

ue_unitHomeUnit_maybe :: HasCallStack => UnitId -> UnitEnv -> Maybe HomeUnit
ue_unitHomeUnit_maybe uid ue_env =
  fmap homeUnitEnv_unsafeHomeUnit (ue_findHomeUnitEnv_maybe uid ue_env)

ue_unitHomeUnit :: HasCallStack => UnitId -> UnitEnv -> HomeUnit
ue_unitHomeUnit uid ue_env = homeUnitEnv_unsafeHomeUnit $ ue_findHomeUnitEnv uid ue_env


-- -------------------------------------------------------
-- Query and modify the currently active unit
-- -------------------------------------------------------

ue_currentHomeUnitEnv :: HasCallStack => UnitEnv -> HomeUnitEnv
ue_currentHomeUnitEnv e =
  case ue_findHomeUnitEnv_maybe (ue_currentUnit e) e of
    Just unitEnv -> unitEnv
    Nothing -> pprPanic "packageNotFound" $
      (ppr $ ue_currentUnit e) $$ ppr (ue_home_unit_graph e)

ue_setActiveUnit :: HasCallStack => UnitId -> UnitEnv -> UnitEnv
ue_setActiveUnit u ue_env = assertUnitEnvInvariant $ ue_env
  { ue_current_unit = u
  }

ue_currentUnit :: UnitEnv -> UnitId
ue_currentUnit = ue_current_unit

-- -------------------------------------------------------
-- Operations on arbitrary elements of the home unit graph
-- -------------------------------------------------------

ue_findHomeUnitEnv_maybe :: HasCallStack => UnitId -> UnitEnv -> Maybe HomeUnitEnv
ue_findHomeUnitEnv_maybe uid e =
  unitEnv_lookup_maybe uid (ue_home_unit_graph e)

ue_findHomeUnitEnv :: HasCallStack => UnitId -> UnitEnv -> HomeUnitEnv
ue_findHomeUnitEnv uid e = case unitEnv_lookup_maybe uid (ue_home_unit_graph e) of
  Nothing -> pprPanic "Unit unknown to the internal unit environment"
              $  text "unit (" <> ppr uid <> text ")"
              $$ pprUnitEnvGraph e
  Just hue -> hue

ue_memberHomeUnitEnv :: HasCallStack => UnitId -> UnitEnv -> Bool
ue_memberHomeUnitEnv uid e =
  unitEnv_member uid (ue_home_unit_graph e)

ue_newHomeUnitGraph :: HasCallStack => Map UnitId HomeUnitEnv -> UnitEnv ->  UnitEnv
ue_newHomeUnitGraph home_unit_env e = e
  { ue_home_unit_graph = unitEnv_new home_unit_env
  }

ue_singletonHomeUnitGraph :: HasCallStack => UnitId -> HomeUnitEnv -> UnitEnv -> UnitEnv
ue_singletonHomeUnitGraph unitId unitEnv ue_env = ue_env
  { ue_home_unit_graph = unitEnv_new (Map.singleton unitId unitEnv)
  }

ue_insertHomeUnitEnv :: HasCallStack => HomeUnitEnv -> UnitEnv -> UnitEnv
ue_insertHomeUnitEnv internalUnitEnv e = e
  { ue_home_unit_graph = unitEnv_insert (homeUnitId_ $ homeUnitEnv_dflags internalUnitEnv) internalUnitEnv $ ue_home_unit_graph e
  }

ue_updateHomeUnitEnv :: HasCallStack => (HomeUnitEnv -> HomeUnitEnv) -> UnitId -> UnitEnv -> UnitEnv
ue_updateHomeUnitEnv f uid e = e
  { ue_home_unit_graph = unitEnv_adjust f uid $ ue_home_unit_graph e
  }

-- ue_setInternalUnitEnvList :: HasCallStack => [(UnitId, HomeUnitEnv)] -> UnitEnv -> UnitEnv
-- ue_setInternalUnitEnvList home_unit_env ue_env =
--   ue_setInternalUnitEnv (Map.fromList home_unit_env) ue_env

-- ue_setInternalUnitEnv :: HasCallStack => Map UnitId HomeUnitEnv -> UnitEnv ->  UnitEnv
-- ue_setInternalUnitEnv home_unit_env e = assertUnitEnvInvariant e
--   { ue_home_unit_graph = unitEnv_new (ue_currentUnit e) home_unit_env
--   }

-- ue_setInternalUnitEnvGraph :: HasCallStack => UnitEnv -> UnitEnv ->  UnitEnv
-- ue_setInternalUnitEnvGraph unitEnv e = assertUnitEnvInvariant e
--   { ue_home_unit_graph = unitEnv
--   }


-- | Rename a unit id in the internal unit env.
--
-- @'ue_renameUnitId' oldUnit newUnit UnitEnv@, it is assumed that the 'oldUnit' exists in the map,
-- otherwise we panic.
-- The 'DynFlags' associated with the home unit will have its field 'homeUnitId' set to 'newUnit'.
ue_renameUnitId :: HasCallStack => UnitId -> UnitId -> UnitEnv -> UnitEnv
ue_renameUnitId oldUnit newUnit unitEnv = case ue_findHomeUnitEnv_maybe oldUnit unitEnv of
  Nothing ->
    pprPanic "Tried to rename unit, but it didn't exist"
              $ text "Rename old unit \"" <> ppr oldUnit <> text "\" to \""<> ppr newUnit <> text "\""
              $$ nest 2 (pprUnitEnvGraph unitEnv)
  Just oldEnv ->
    let
      activeUnit :: UnitId
      !activeUnit = if ue_currentUnit unitEnv == oldUnit
                then newUnit
                else ue_currentUnit unitEnv

      newInternalUnitEnv = oldEnv
        { homeUnitEnv_dflags = (homeUnitEnv_dflags oldEnv)
            { homeUnitId_ = newUnit
            }
        }
    in
    unitEnv
      { ue_current_unit = activeUnit
      , ue_home_unit_graph =
          unitEnv_insert newUnit newInternalUnitEnv
          $ unitEnv_delete oldUnit
          $ ue_home_unit_graph unitEnv
          }

-- -----------------------------------------------------------------------------
-- Getters
-- -----------------------------------------------------------------------------

ue_HPTs :: HasCallStack => UnitEnv -> [HomePackageTable]
ue_HPTs = map homeUnitEnv_hpt . ue_home_unit_graphs

ue_home_unit_graphs :: HasCallStack => UnitEnv -> [HomeUnitEnv]
ue_home_unit_graphs = Map.elems . unitEnv_graph . ue_home_unit_graph

ue_allDflags :: HasCallStack => UnitEnv -> [DynFlags]
ue_allDflags = map homeUnitEnv_dflags . ue_home_unit_graphs

ue_allHomeUnits :: HasCallStack => UnitEnv -> [HomeUnit]
ue_allHomeUnits = map homeUnitEnv_unsafeHomeUnit . ue_home_unit_graphs

ue_allUnitIds :: HasCallStack => UnitEnv -> Set UnitId
ue_allUnitIds = Map.keysSet . unitEnv_graph . ue_home_unit_graph

-- -----------------------------------------------------------------------------
-- Functionality for home units
-- -----------------------------------------------------------------------------

-- ue_currentHomeUnitDependencies :: HasCallStack => UnitEnv -> Set UnitId
-- ue_currentHomeUnitDependencies e = ue_homeUnitDependencies e (ue_currentUnit e)

-- ue_homeUnitDependencies :: HasCallStack => UnitEnv -> UnitId -> Set UnitId
-- ue_homeUnitDependencies ue_env unitId =
--   Map.keysSet . unitEnv_graph
--     $ unitEnv_restrictHomeUnitDependencies (ue_home_unit_graph ue_env) unitId

-- ue_homeUnitDependencies_unitEnv :: HasCallStack => UnitEnv -> UnitId -> [HomeUnitEnv]
-- ue_homeUnitDependencies_unitEnv ue_env unitId =
--   Map.elems . unitEnv_graph
--     $ unitEnv_restrictHomeUnitDependencies (ue_home_unit_graph  ue_env) unitId

-- unitEnv_homeUnitDependencies :: HasCallStack => UnitEnv -> UnitId -> [UnitId]
-- unitEnv_homeUnitDependencies unitEnv unitId = filter (`unitEnv_member` unitEnv) deps
--   where
--     dflags = homeUnitEnv_dflags $ unitEnv_lookup unitId unitEnv
--     deps = map toUnitId $ explicitUnits (unitState dflags)

-- unitEnv_restrictHomeUnitDependencies :: HasCallStack => UnitEnv -> UnitId -> UnitEnv
-- unitEnv_restrictHomeUnitDependencies unitEnv unitId = unitEnv_setGraph
--   (Map.restrictKeys
--       (unitEnv_graph unitEnv)
--       deps
--   )
--   unitEnv
--   where
--     dflags = homeUnitEnv_dflags $ unitEnv_lookup unitId unitEnv    --
--     -- Always remove the current home unit from the list of home unit dependencies.
--     -- We do this, since if "-this-unit-id" is set to "base" (or any other wired-in unit)
--     -- but we are not compiling the wired-in unit, the unit id key in 'UnitEnvGraph' will be
--     -- "base-<wired-in version>" and it will also have a dependency on "base-<wired-in version>".
--     -- Those units are actually not the same unit, but it will look like it.
--     -- So, "base-<wired-in version>" is both home unit id and a dependency for the current unit id.
--     -- Therefore, it will be in the Set of Unit Ids and violate our post-condition
--     -- that the function result does not include the given Unit Id's key.
--     deps = Set.delete unitId $  Set.fromList $ map toUnitId $ explicitUnits (unitState dflags)

-- ---------------------------------------------
-- Asserts to enforce invariants for the UnitEnv
-- ---------------------------------------------

assertUnitEnvInvariant :: HasCallStack => UnitEnv -> UnitEnv
assertUnitEnvInvariant u =
  if ue_current_unit u `unitEnv_member` ue_home_unit_graph u
    then u
    else pprPanic "invariant" (ppr (ue_current_unit u) $$ ppr (ue_home_unit_graph u))
{-
assertUnitEnvKnownUnit :: HasCallStack => UnitId -> HomeUnitGraph -> HomeUnitGraph
assertUnitEnvKnownUnit pkg e =
  assertPpr (unitEnv_member pkg e) (errMsg) e
  where
    errMsg = text "assertUnitKnown" <+> ppr (unitEnv_currentUnit e) $$ nest 2 (ppr (Map.keys $ unitEnv_graph e))

assertHomeUnitGraphInvariant :: HasCallStack => HomeUnitGraph -> HomeUnitGraph
assertHomeUnitGraphInvariant e = assertUnitEnvKnownUnit (unitEnv_currentUnit e) $ assertUnitEnvConsistent e

assertUnitEnvConsistent :: HasCallStack => HomeUnitGraph -> HomeUnitGraph
assertUnitEnvConsistent env =
  assertPpr (all sameUnitId entries) inconsistentDflagsMsg env
  where
    entries = Map.assocs $ unitEnv_graph env

    sameUnitId (uid, env) = uid == (homeUnitId_ $ homeUnitEnv_dflags env)

    inconsistentDflagsMsg =
      text "Unit Environment is inconsistent. An Entry has a different unit than its dflags homeUnitId:"
      $$ pprHomeUnitGraph env

assertUnitEnvInvariant :: HasCallStack => UnitEnv -> UnitEnv
assertUnitEnvInvariant e = e { ue_home_unit_graph = assertHomeUnitGraphInvariant $ ue_home_unit_graph e }
-}

-- -----------------------------------------------------------------------------
-- Pretty output functions
-- -----------------------------------------------------------------------------

pprUnitEnvGraph :: HasCallStack => UnitEnv -> SDoc
pprUnitEnvGraph env = text "pprInternalUnitMap"
  $$ nest 2 (pprHomeUnitGraph $ ue_home_unit_graph env)

pprHomeUnitGraph :: HasCallStack => HomeUnitGraph -> SDoc
pprHomeUnitGraph unitEnv = vcat (map (\(k, v) -> pprHomeUnitEnv k v) $ Map.assocs $ unitEnv_graph unitEnv)

pprHomeUnitEnv :: HasCallStack => UnitId -> HomeUnitEnv -> SDoc
pprHomeUnitEnv uid env =
  ppr uid <+> text "(flags:" <+> ppr (homeUnitId_ $ homeUnitEnv_dflags env) <> text "," <+> ppr (fmap homeUnitId $ homeUnitEnv_home_unit env) <> text ")" <+> text "->"
  $$ nest 4 (pprHPT $ homeUnitEnv_hpt env)

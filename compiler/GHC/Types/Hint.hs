{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module GHC.Types.Hint where

import GHC.Prelude

import GHC.Utils.Outputable
import GHC.LanguageExtensions
import Data.Typeable
import GHC.Unit.Module (ModuleName, Module)

-- | A type for hints emitted by GHC.
-- A /hint/ suggests a possible way to deal with a particular warning or error.
data GhcHint where
  -- | An \"unknown\" hint. This type constructor allows arbitrary
  -- hints to be embedded. The typical use case would be GHC plugins
  -- willing to emit hints alonside their custom diagnostics.
  UnknownHint :: (Outputable a, Typeable a) => a -> GhcHint
  -- | Suggests adding a particular language extension.
  SuggestExtension :: !Extension -> GhcHint
  -- | Suggests that a particular statement should be written within a \"do\"
  -- block.
  SuggestDo :: GhcHint
  -- | Suggests that a missing \"do\" block.
  SuggestMissingDo :: GhcHint
  -- | Suggests that a \"let\" expression is needed in a \"do\" block.
  SuggestLetInDo :: GhcHint
  -- | Suggests to add an \".hsig\" signature file to the Cabal manifest.
  SuggestAddSignatureCabalFile :: !ModuleName -> GhcHint
  -- | Suggests to explictly list the instantiations for the signatures in
  -- the GHC invocation command.
  SuggestSignatureInstantiations :: !ModuleName -> [InstantiationSuggestion] -> GhcHint
  -- | Suggests to use spaces instead of tabs.
  SuggestUseSpaces :: GhcHint
  -- | Suggests wrapping an expression in parentheses
  SuggestParentheses :: GhcHint
  SuggestAddWhitespaceAround :: GhcHint
  -- | Suggests using 'Type' from 'Data.Kind' instead of \"*\".
  SuggestUseDataKindType :: GhcHint


instance Outputable GhcHint where
  ppr = \case
    UnknownHint m
      -> ppr m
    SuggestExtension ext
      -> text "Perhaps you intended to use" <+> ppr ext
    SuggestDo
      -> text "Perhaps this statement should be within a 'do' block?"
    SuggestMissingDo
      -> text "Possibly caused by a missing 'do'?"
    SuggestLetInDo
      -> text "Perhaps you need a 'let' in a 'do' block?"
           $$ text "e.g. 'let x = 5' instead of 'x = 5'"
    SuggestAddSignatureCabalFile pi_mod_name
      -> text "Try adding" <+> quotes (ppr pi_mod_name)
           <+> text "to the"
           <+> quotes (text "signatures")
           <+> text "field in your Cabal file."
    SuggestSignatureInstantiations pi_mod_name suggestions
      -> let suggested_instantiated_with =
               hcat (punctuate comma $
                   [ ppr k <> text "=" <> ppr v
                   | InstantiationSuggestion k v <- suggestions
                   ])
         in text "Try passing -instantiated-with=\"" <>
              suggested_instantiated_with <> text "\"" $$
                text "replacing <" <> ppr pi_mod_name <> text "> as necessary."
    SuggestUseSpaces
      -> text "Please use spaces instead."
    SuggestParentheses
      -> text "You could write it with parentheses"
    SuggestAddWhitespaceAround
      -> text "Add a whitespace around it."
    SuggestUseDataKindType
      -> text "Use" <+> quotes (text "Type")
           <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."

-- | An 'InstantiationSuggestion' for a '.hsig' file. This is generated
-- by GHC in case of a 'DriverUnexpectedSignature' and suggests a way
-- to instantiate a particular signature, where the first argument is
-- the signature name and the second is the module where the signature
-- was defined.
-- Example:
--
-- src/MyStr.hsig:2:11: error:
--     Unexpected signature: ‘MyStr’
--     (Try passing -instantiated-with="MyStr=<MyStr>"
--      replacing <MyStr> as necessary.)
data InstantiationSuggestion = InstantiationSuggestion !ModuleName !Module

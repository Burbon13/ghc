module GHC.Driver.Errors (
    printOrThrowWarnings
  , printBagOfErrors
  , handleFlagWarnings
  , partitionMessageBag
  ) where

import GHC.Driver.Session
import GHC.Data.Bag
import GHC.Utils.Exception
import GHC.Utils.Error ( formatBulleted, sortMsgBag )
import GHC.Types.SourceError ( mkSrcErr )
import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Types.Error
import GHC.Utils.Outputable ( text, withPprStyle, mkErrStyle )
import GHC.Utils.Logger
import qualified GHC.Driver.CmdLine as CmdLine

-- | Partitions the messages and returns a tuple which first element are the warnings, and the
-- second the errors.
partitionMessageBag :: Diagnostic e => Bag (MsgEnvelope e) -> (Bag (MsgEnvelope e), Bag (MsgEnvelope e))
partitionMessageBag = partitionBag isWarningMessage

printBagOfErrors :: Diagnostic a => Logger -> DynFlags -> Bag (MsgEnvelope a) -> IO ()
printBagOfErrors logger dflags bag_of_errors
  = sequence_ [ let style = mkErrStyle unqual
                    ctx   = initSDocContext dflags style
                in putLogMsg logger dflags (MCDiagnostic sev . diagnosticReason $ dia) s $
                   withPprStyle style (formatBulleted ctx (diagnosticMessage dia))
              | MsgEnvelope { errMsgSpan      = s,
                              errMsgDiagnostic = dia,
                              errMsgSeverity = sev,
                              errMsgContext   = unqual } <- sortMsgBag (Just dflags)
                                                                       bag_of_errors ]

handleFlagWarnings :: Logger -> DynFlags -> [CmdLine.Warn] -> IO ()
handleFlagWarnings logger dflags warns = do
  let warns' = filter (shouldPrintWarning dflags . CmdLine.warnReason)  warns

      -- It would be nicer if warns :: [Located SDoc], but that
      -- has circular import problems.
      bag = listToBag [ mkPlainMsgEnvelope WarningWithoutFlag loc (text warn)
                      | CmdLine.Warn _ (L loc warn) <- warns' ]

  printOrThrowWarnings logger dflags bag

-- Given a warn reason, check to see if it's associated -W opt is enabled
shouldPrintWarning :: DynFlags -> CmdLine.WarnReason -> Bool
shouldPrintWarning dflags CmdLine.ReasonDeprecatedFlag
  = wopt Opt_WarnDeprecatedFlags dflags
shouldPrintWarning dflags CmdLine.ReasonUnrecognisedFlag
  = wopt Opt_WarnUnrecognisedWarningFlags dflags
shouldPrintWarning _ _
  = True

-- | Given a bag of warnings, turn them into an exception if
-- -Werror is enabled, or print them out otherwise.
printOrThrowWarnings :: Logger -> DynFlags -> Bag WarnMsg -> IO ()
printOrThrowWarnings logger dflags warns = do
  let (make_error, warns') =
        mapAccumBagL
          (\make_err warn ->
            case warn_msg_severity dflags warn of
              SevWarning ->
                (make_err, warn)
              SevError ->
                (True, set_severity SevError warn))
          False warns
  if make_error
    then throwIO (mkSrcErr warns')
    else printBagOfErrors logger dflags warns

  where

    -- | Sets the 'Severity' of the input 'WarnMsg' according to the 'DynFlags'.
    warn_msg_severity :: DynFlags -> WarnMsg -> Severity
    warn_msg_severity dflags msg =
      case diagnosticReason (errMsgDiagnostic msg) of
        ErrorWithoutFlag   -> SevError
        WarningWithoutFlag ->
          if gopt Opt_WarnIsError dflags
            then SevError
            else SevWarning
        WarningWithFlag wflag ->
          if wopt_fatal wflag dflags
            then SevError
            else SevWarning

    -- | Adjust the 'Severity' of the input 'WarnMsg'.
    set_severity :: Severity -> WarnMsg -> MsgEnvelope DiagnosticMessage
    set_severity newSeverity msg = msg { errMsgSeverity = newSeverity }


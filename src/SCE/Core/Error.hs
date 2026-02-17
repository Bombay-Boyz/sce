{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

{-|
Module      : SCE.Core.Error
Description : Unified error system for the Statistical Charting Engine
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
Maintainer  : dev@sce.example.com

This module defines the canonical error ADT used throughout SCE.
It replaces the old flat @ValidationError@ with a structured
@DetailedError@ that carries an error code, context, severity,
and actionable suggestions.

Design constraints:
  * Zero internal SCE imports — only @base@, @text@.
  * No partial functions. No @error@, @undefined@, @head@.
  * Every exported function is total.
  * All output is printable ASCII (no Unicode box-drawing).
-}
module SCE.Core.Error
  ( -- * Error codes
    ErrorCode(..)
    -- * Error hierarchy
  , StatEngineError(..)
  , DetailedError(..)
  , ErrorContext(..)
  , Severity(..)
    -- * Result type alias
  , SCEResult
    -- * Construction
  , mkError
  , mkErrorAt
  , addContext
  , addSuggestion
    -- * Pretty printing (ASCII only)
  , formatError
  , formatErrorShort
    -- * Utilities
  , errorCodePrefix
  , severityLabel
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Error codes
-- ---------------------------------------------------------------------------

-- | Numeric error codes grouped by subsystem.
--
-- @E1xxx@ — Ingestion
-- @E2xxx@ — Validation
-- @E3xxx@ — Statistical computation
-- @E4xxx@ — Hypothesis testing / inference
-- @E5xxx@ — Rendering (tables, charts)
data ErrorCode
  -- Ingestion errors
  = E1001  -- ^ CSV parse failure
  | E1002  -- ^ Invalid encoding
  | E1003  -- ^ Delimiter not detected
  | E1004  -- ^ Empty file
  | E1005  -- ^ Schema confirmation failed
  -- Validation errors
  | E2001  -- ^ Type mismatch in column
  | E2002  -- ^ Insufficient data (below minimum n)
  | E2003  -- ^ Invalid measurement scale for operation
  | E2004  -- ^ Missing value in non-nullable column
  | E2005  -- ^ Percentage sum invariant violated
  | E2006  -- ^ Unit mismatch across columns
  | E2007  -- ^ Monotonicity violated (time series)
  | E2008  -- ^ Cardinality too high for Nominal scale
  -- Statistical errors
  | E3001  -- ^ Numerical instability detected
  | E3002  -- ^ Singular matrix
  | E3003  -- ^ Convergence failure
  | E3004  -- ^ Infinite or NaN value
  -- Test / inference errors
  | E4001  -- ^ Assumption violated (e.g. normality required)
  | E4002  -- ^ Insufficient sample size for test
  | E4003  -- ^ Zero variance in group
  | E4004  -- ^ p-value out of bounds (internal bug)
  -- Rendering errors
  | E5001  -- ^ Data range too tight for chart
  | E5002  -- ^ Too many categories for chart type
  | E5003  -- ^ Chart type inadmissible for scale
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Human-readable prefix for an error code, e.g. @"[E2002]"@.
-- Total: covers every constructor via 'Enum' / 'Bounded' exhaustion.
errorCodePrefix :: ErrorCode -> Text
errorCodePrefix code = "[" <> T.pack (show code) <> "]"

-- ---------------------------------------------------------------------------
-- Severity
-- ---------------------------------------------------------------------------

-- | How serious is the error?
data Severity
  = Info      -- ^ Informational; the computation can continue.
  | Warning   -- ^ Something suspicious; result may be unreliable.
  | Error     -- ^ Operation failed; result is unavailable.
  | Critical  -- ^ Fundamental invariant violated; system state is suspect.
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Short ASCII label for a severity level.
severityLabel :: Severity -> Text
severityLabel Info     = "INFO"
severityLabel Warning  = "WARN"
severityLabel Error    = "ERROR"
severityLabel Critical = "CRITICAL"

-- ---------------------------------------------------------------------------
-- ErrorContext
-- ---------------------------------------------------------------------------

-- | Where in the data-processing pipeline did the error occur?
data ErrorContext
  = FileContext    FilePath Int Int
    -- ^ CSV source file: path, line number (1-based), column number (1-based).
  | DataContext    Text Int Text
    -- ^ In-memory frame: column name, row index (0-based), offending value.
  | ComputationContext Text [(Text, Text)]
    -- ^ Statistical operation: operation name, list of (param, value) pairs.
  | NoContext
    -- ^ No additional context available.
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- StatEngineError — top-level discriminator (for catch / pattern-match)
-- ---------------------------------------------------------------------------

-- | High-level category, useful for coarse-grained recovery logic.
data StatEngineError
  = IngestionError
  | ValidationError
  | StatisticalError
  | InferenceError
  | RenderingError
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Map an 'ErrorCode' to its high-level 'StatEngineError' category.
statEngineErrorFor :: ErrorCode -> StatEngineError
statEngineErrorFor code = case code of
  E1001 -> IngestionError
  E1002 -> IngestionError
  E1003 -> IngestionError
  E1004 -> IngestionError
  E1005 -> IngestionError
  E2001 -> ValidationError
  E2002 -> ValidationError
  E2003 -> ValidationError
  E2004 -> ValidationError
  E2005 -> ValidationError
  E2006 -> ValidationError
  E2007 -> ValidationError
  E2008 -> ValidationError
  E3001 -> StatisticalError
  E3002 -> StatisticalError
  E3003 -> StatisticalError
  E3004 -> StatisticalError
  E4001 -> InferenceError
  E4002 -> InferenceError
  E4003 -> InferenceError
  E4004 -> InferenceError
  E5001 -> RenderingError
  E5002 -> RenderingError
  E5003 -> RenderingError

-- ---------------------------------------------------------------------------
-- DetailedError
-- ---------------------------------------------------------------------------

-- | The canonical error type for SCE.
--
-- Replaces the old flat @ValidationError@ ADT.  Every function that can
-- fail returns @Either DetailedError a@ (aliased as @SCEResult a@).
data DetailedError = DetailedError
  { errorCode        :: ErrorCode
    -- ^ Numeric code that identifies the failure mode.
  , errorMessage     :: Text
    -- ^ Human-readable explanation of what went wrong.
  , errorContext     :: ErrorContext
    -- ^ Where in the pipeline / data the error occurred.
  , errorSuggestions :: [Text]
    -- ^ Ordered list of actionable suggestions for the user.
  , errorSeverity    :: Severity
    -- ^ How serious this error is.
  , errorCategory    :: StatEngineError
    -- ^ High-level category (derived from 'errorCode').
  } deriving stock (Show, Eq)

-- | Convenience alias — every fallible SCE function returns this.
type SCEResult a = Either DetailedError a

-- ---------------------------------------------------------------------------
-- Construction helpers
-- ---------------------------------------------------------------------------

-- | Build a 'DetailedError' with 'NoContext'.
-- Use 'addContext' afterwards when the call site knows more.
--
-- >>> mkError E2002 "Insufficient data" ["Collect at least 30 samples."] Error
mkError :: ErrorCode -> Text -> [Text] -> Severity -> DetailedError
mkError code msg suggestions sev = DetailedError
  { errorCode        = code
  , errorMessage     = msg
  , errorContext     = NoContext
  , errorSuggestions = suggestions
  , errorSeverity    = sev
  , errorCategory    = statEngineErrorFor code
  }

-- | Build a 'DetailedError' with an explicit context in one call.
mkErrorAt :: ErrorCode -> Text -> ErrorContext -> [Text] -> Severity -> DetailedError
mkErrorAt code msg ctx suggestions sev = DetailedError
  { errorCode        = code
  , errorMessage     = msg
  , errorContext     = ctx
  , errorSuggestions = suggestions
  , errorSeverity    = sev
  , errorCategory    = statEngineErrorFor code
  }

-- | Replace the context of an existing error.
-- Useful at call-site boundaries where the context is known.
addContext :: ErrorContext -> DetailedError -> DetailedError
addContext ctx e = e { errorContext = ctx }

-- | Append a suggestion to an existing error.
addSuggestion :: Text -> DetailedError -> DetailedError
addSuggestion s e = e { errorSuggestions = errorSuggestions e ++ [s] }

-- ---------------------------------------------------------------------------
-- Pretty printing — ASCII only, 80-column safe
-- ---------------------------------------------------------------------------

-- | Multi-line ASCII error report.
--
-- Example output (wrapped at 78 chars):
--
-- > +------------------------------------------------------------------+
-- > | [E2002] ERROR  Insufficient data                                  |
-- > +------------------------------------------------------------------+
-- > | Context: operation=t-test, required=30, actual=12                 |
-- > | Suggestion: Collect more data or use Mann-Whitney U (non-param.)  |
-- > +------------------------------------------------------------------+
formatError :: DetailedError -> Text
formatError e =
  let header  = formatHeader e
      ctxLine = formatContext (errorContext e)
      suggs   = formatSuggestions (errorSuggestions e)
      border  = T.replicate 70 "-"
  in T.intercalate "\n"
      ( [border, header, border]
     ++ (if T.null ctxLine then [] else [ctxLine])
     ++ suggs
     ++ [border]
      )

-- | One-line summary suitable for log files.
--
-- Example: @"[E2002] ERROR Insufficient data (operation=t-test)"@
formatErrorShort :: DetailedError -> Text
formatErrorShort e =
  errorCodePrefix (errorCode e)
  <> " " <> severityLabel (errorSeverity e)
  <> " " <> errorMessage e
  <> shortContext (errorContext e)

-- ---------------------------------------------------------------------------
-- Internal formatting helpers (all total, ASCII-only)
-- ---------------------------------------------------------------------------

formatHeader :: DetailedError -> Text
formatHeader e =
  "  " <> errorCodePrefix (errorCode e)
  <> " " <> severityLabel (errorSeverity e)
  <> "  " <> errorMessage e

formatContext :: ErrorContext -> Text
formatContext NoContext = ""
formatContext (FileContext path lineN colN) =
  "  Context: file=" <> T.pack path
  <> ", line=" <> T.pack (show lineN)
  <> ", col=" <> T.pack (show colN)
formatContext (DataContext colName rowIdx val) =
  "  Context: column=" <> colName
  <> ", row=" <> T.pack (show rowIdx)
  <> ", value=" <> val
formatContext (ComputationContext opName params) =
  "  Context: operation=" <> opName
  <> if null params then ""
     else ", " <> T.intercalate ", " (map formatParam params)
  where
    formatParam (k, v) = k <> "=" <> v

formatSuggestions :: [Text] -> [Text]
formatSuggestions []   = []
formatSuggestions sugs = map (\s -> "  Suggestion: " <> s) sugs

shortContext :: ErrorContext -> Text
shortContext NoContext                      = ""
shortContext (FileContext path lineN _)    =
  " (file=" <> T.pack path <> ", line=" <> T.pack (show lineN) <> ")"
shortContext (DataContext col row _)       =
  " (column=" <> col <> ", row=" <> T.pack (show row) <> ")"
shortContext (ComputationContext op [])    = " (operation=" <> op <> ")"
shortContext (ComputationContext op (p:_)) =
  " (operation=" <> op <> ", " <> fst p <> "=" <> snd p <> ")"

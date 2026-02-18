{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Validation.Schema
Description : Column-level scale and cardinality validation (Phase 2)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Validates that the committed 'MeasurementScale' for each column is
consistent with the actual data it contains.

Design constraints:
  * All functions are total — no partial functions, no 'error', no 'undefined'.
  * 'validateColumn' never returns 'Left'; individual check failures are
    embedded inside 'ColumnValidationResult' so callers always get a report.
  * 'validateScale' and 'validateCardinality' return 'Left' on hard violations
    so they can short-circuit in pipeline contexts.
-}
module SCE.Validation.Schema
  ( -- * Column-level validation
    validateColumn
  , validateScale
  , validateCardinality
    -- * Types
  , ColumnValidationResult(..)
  ) where

import SCE.Core.Types
  ( DataValue(..)
  , MeasurementScale(..)
  , DetailedError
  , ErrorCode(..)
  , Severity(..)
  , SCEResult
  , mkError
  , addContext
  , ErrorContext(..)
  )

import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.List   (nub)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Summary of all validation checks run against a single column.
-- 'validateColumn' always produces one of these (never 'Left').
data ColumnValidationResult = ColumnValidationResult
  { cvrColumn   :: Text              -- ^ Column name
  , cvrScale    :: MeasurementScale  -- ^ Committed scale being validated
  , cvrPassed   :: [Text]            -- ^ Names of checks that passed
  , cvrWarnings :: [DetailedError]   -- ^ Non-fatal issues (Warning severity)
  , cvrErrors   :: [DetailedError]   -- ^ Hard violations (Error severity)
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- validateScale
-- ---------------------------------------------------------------------------

-- | Verify that the committed 'MeasurementScale' is consistent with the
-- observed data.
--
-- Hard rules:
--   * 'Ratio'   — no negative numeric values allowed (returns 'Left E2003').
--   * 'Nominal' — cardinality must be <= 50 (returns 'Left E2008').
--   * 'Ordinal' — column must not be purely numeric without rank information
--                 (warning only; no hard 'Left').
--
-- Returns 'Right ()' when the data is consistent with the declared scale.
validateScale
  :: Text              -- ^ Column name (for error context)
  -> MeasurementScale  -- ^ Committed scale
  -> Vector DataValue  -- ^ Observed values (including 'MissingValue')
  -> SCEResult ()
validateScale colName scale values =
  case scale of
    Ratio    -> checkNoNegatives colName values
    Nominal  -> checkCardinalityLimit colName 50 values
    Interval -> Right ()  -- negative values are fine for Interval
    Ordinal  -> Right ()  -- text with ordering; no hard constraint here

-- ---------------------------------------------------------------------------
-- validateCardinality
-- ---------------------------------------------------------------------------

-- | Warn when a 'Nominal' column exceeds the cardinality threshold.
-- High cardinality in a Nominal column is almost always a data-quality issue
-- (e.g. a free-text field mistakenly declared Nominal).
--
-- Returns 'Left E2008' when @uniqueCount > threshold@.
-- The threshold should normally be 50; callers supply it for flexibility.
validateCardinality
  :: Text             -- ^ Column name
  -> Int              -- ^ Cardinality threshold (e.g. 50)
  -> Vector DataValue -- ^ Observed values
  -> SCEResult ()
validateCardinality colName threshold values =
  let nonMissing  = V.filter (not . isMissing) values
      uniques     = nub (V.toList nonMissing)
      uniqueCount = length uniques
  in if uniqueCount > threshold
       then Left
         $ addContext (DataContext colName 0 (T.pack (show uniqueCount) <> " unique values"))
         $ mkError E2008
             (  "Column '" <> colName <> "' has " <> T.pack (show uniqueCount)
             <> " unique values, exceeding the Nominal cardinality limit of "
             <> T.pack (show threshold) <> "."
             )
             [ "Consider whether this column should be Ordinal or a free-text field."
             , "If Nominal is correct, raise the threshold in the schema confirmation."
             ]
             Warning
       else Right ()

-- ---------------------------------------------------------------------------
-- validateColumn
-- ---------------------------------------------------------------------------

-- | Run all column-level validation checks and collect results.
-- This function is total — it never returns 'Left'.
-- All check failures are accumulated into 'cvrErrors' or 'cvrWarnings'.
validateColumn
  :: Text              -- ^ Column name
  -> MeasurementScale  -- ^ Committed scale
  -> Vector DataValue  -- ^ Observed values
  -> ColumnValidationResult
validateColumn colName scale values =
  let passed   = []
      warnings = []
      errors   = []

      -- Check 1: scale consistency
      (p1, w1, e1) = runCheck "scale-consistency" passed warnings errors
                       (validateScale colName scale values)

      -- Check 2: cardinality for Nominal columns
      (p2, w2, e2) = case scale of
        Nominal -> runCheckWarn "cardinality" p1 w1 e1
                     (validateCardinality colName 50 values)
        _       -> ("cardinality-n/a" : p1, w1, e1)

      -- Check 3: non-empty (at least one non-missing value)
      (p3, w3, e3) = runCheckWarn "non-empty" p2 w2 e2
                       (checkNonEmpty colName values)

      -- Check 4: all-missing guard (Error-level)
      (p4, w4, e4) = runCheck "all-values-present" p3 w3 e3
                       (checkNotAllMissing colName values)

  in ColumnValidationResult
       { cvrColumn   = colName
       , cvrScale    = scale
       , cvrPassed   = p4
       , cvrWarnings = w4
       , cvrErrors   = e4
       }

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Run a check; on 'Left' add to errors, on 'Right' add to passed.
runCheck
  :: Text
  -> [Text]           -- passed accumulator
  -> [DetailedError]  -- warnings accumulator
  -> [DetailedError]  -- errors accumulator
  -> SCEResult ()
  -> ([Text], [DetailedError], [DetailedError])
runCheck name passed warnings errors result =
  case result of
    Right () -> (name : passed, warnings, errors)
    Left  e  -> (passed, warnings, e : errors)

-- | Like 'runCheck' but 'Left' goes to warnings (not errors).
runCheckWarn
  :: Text
  -> [Text]
  -> [DetailedError]
  -> [DetailedError]
  -> SCEResult ()
  -> ([Text], [DetailedError], [DetailedError])
runCheckWarn name passed warnings errors result =
  case result of
    Right () -> (name : passed, warnings, errors)
    Left  e  -> (passed, e : warnings, errors)

-- | Enforce that a Ratio column has no negative numeric values.
checkNoNegatives :: Text -> Vector DataValue -> SCEResult ()
checkNoNegatives colName values =
  case V.find isNegativeNumeric values of
    Nothing -> Right ()
    Just (NumericValue v) ->
      Left
        $ addContext (DataContext colName 0 (T.pack (show v)))
        $ mkError E2003
            (  "Column '" <> colName
            <> "' is declared Ratio scale but contains a negative value ("
            <> T.pack (show v) <> ")."
            )
            [ "Use Interval scale for columns that can contain negative values."
            , "Remove or correct negative values if they are data errors."
            ]
            Error
    Just _ -> Right ()  -- unreachable; isNegativeNumeric only matches NumericValue

-- | Hard cardinality check: Left when uniqueCount > limit.
checkCardinalityLimit :: Text -> Int -> Vector DataValue -> SCEResult ()
checkCardinalityLimit colName limit values =
  validateCardinality colName limit values

-- | Warn when every value in the column is missing.
checkNotAllMissing :: Text -> Vector DataValue -> SCEResult ()
checkNotAllMissing colName values =
  if V.all isMissing values
    then Left
      $ addContext (DataContext colName 0 "all values missing")
      $ mkError E2004
          ("Column '" <> colName <> "' contains only missing values.")
          ["Check the data source; this column may be empty or mis-named."]
          Error
    else Right ()

-- | Warn when the column has no non-missing values (same condition, Warning severity).
checkNonEmpty :: Text -> Vector DataValue -> SCEResult ()
checkNonEmpty colName values =
  let nonMissing = V.filter (not . isMissing) values
  in if V.null nonMissing
       then Left
         $ mkError E2002
             ("Column '" <> colName <> "' has no non-missing values.")
             ["Verify that the column name is correct and the CSV contains data."]
             Warning
       else Right ()

-- | True for 'NumericValue' with a negative number.
isNegativeNumeric :: DataValue -> Bool
isNegativeNumeric (NumericValue v) = v < 0
isNegativeNumeric _                = False

-- | True for 'MissingValue' or empty 'TextValue'.
isMissing :: DataValue -> Bool
isMissing MissingValue    = True
isMissing (TextValue t)   = T.null t
isMissing (NumericValue _) = False

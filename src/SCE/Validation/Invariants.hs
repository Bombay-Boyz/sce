{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Validation.Invariants
Description : Cross-column invariants and business rules (Phase 2 extended)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Enforces invariants that span multiple columns or relate columns to known
business constraints: percentage sums, time-series monotonicity, range
bounds, unit compatibility, sign heuristics, and part-whole consistency.

Phase 2 additions:
  * 'checkUnitConsistency'  — detect incompatible currency / unit suffixes
  * 'checkSignConstraints'  — heuristic sign check on "revenue", "count" etc.
  * 'checkMonotonicity'     — strict monotone check for time-series columns
  * 'checkSumConstraint'    — verify that part-columns sum to a whole-column
  * 'InvariantViolation'    — structured record for all violations
-}
module SCE.Validation.Invariants
  ( -- * Existing checks (kept from Phase 1)
    checkPercentageInvariants
  , checkTimeSeriesInvariants
  , checkRangeInvariants
    -- * New in Phase 2
  , checkUnitConsistency
  , checkSignConstraints
  , checkMonotonicity
  , checkSumConstraint
    -- * Types
  , InvariantViolation(..)
  ) where

import SCE.Core.Types
  ( MeasurementScale(..)
  , DetailedError
  , ErrorCode(..)
  , Severity(..)
  , SCEResult
  , mkError
  , addContext
  , ErrorContext(..)
  )
import SCE.Ingestion.Coercion
  ( ValidatedFrame(..)
  , TypedColumn(..)
  , getNumericVec
  , columnName
  )

import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.List   (nub)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A structured record describing a single invariant violation.
data InvariantViolation = InvariantViolation
  { ivInvariant   :: Text          -- ^ Short invariant identifier
  , ivColumns     :: [Text]        -- ^ Columns involved in the violation
  , ivDescription :: Text          -- ^ Human-readable description
  , ivError       :: DetailedError -- ^ Underlying error (code + context)
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Existing checks
-- ---------------------------------------------------------------------------

-- | Verify that all values lie in [0, 100] and sum to 100 +/- 0.01.
checkPercentageInvariants :: Vector Double -> SCEResult ()
checkPercentageInvariants percentages = do
  V.mapM_ checkRange percentages
  let total = V.sum percentages
  if abs (total - 100.0) <= 0.01
    then Right ()
    else Left (mkError E2005
          ("Percentage sum is " <> T.pack (show total)
           <> ", expected 100.0 (+/- 0.01)")
          ["Ensure all percentages sum to 100."] Error)
  where
    checkRange p =
      if p >= 0 && p <= 100
        then Right ()
        else Left (mkError E2005
              ("Percentage out of range [0,100]: " <> T.pack (show p))
              ["Ensure all percentage values are between 0 and 100."] Error)

-- | Verify that a time-series vector has at least 2 values.
checkTimeSeriesInvariants :: Vector Double -> SCEResult ()
checkTimeSeriesInvariants values
  | V.null values       = Left (mkError E2002
      "Time series is empty: at least 2 values required"
      ["Provide at least 2 time points."] Error)
  | V.length values < 2 = Left (mkError E2002
      ("Time series has " <> T.pack (show (V.length values))
       <> " point(s), need at least 2")
      ["Provide at least 2 time points."] Error)
  | otherwise            = Right ()

-- | Verify that every value lies within [minVal, maxVal].
checkRangeInvariants :: Double -> Double -> Vector Double -> SCEResult ()
checkRangeInvariants minVal maxVal values =
  V.mapM_ checkInRange values
  where
    checkInRange v =
      if v >= minVal && v <= maxVal
        then Right ()
        else Left (mkError E2001
              ("Value " <> T.pack (show v)
               <> " is outside range ["
               <> T.pack (show minVal) <> ", "
               <> T.pack (show maxVal) <> "]")
              ["Ensure all values lie within the expected range."] Error)

-- ---------------------------------------------------------------------------
-- New Phase 2: checkUnitConsistency
-- ---------------------------------------------------------------------------

-- | Detect columns with incompatible unit suffixes.
--
-- Unit suffix is inferred from the column name when no explicit unit is
-- provided (e.g. @revenue_usd@ -> USD, @cost_eur@ -> EUR).
-- Returns one 'InvariantViolation' when > 1 distinct unit is found.
checkUnitConsistency :: [(Text, Maybe Text)] -> [InvariantViolation]
checkUnitConsistency namedCols =
  let resolved = map (\(n, mu) -> (n, maybe (inferUnit n) Just mu)) namedCols
      withUnit = [ (n, u) | (n, Just u) <- resolved ]
      units    = nub (map snd withUnit)
  in if length units <= 1
       then []
       else
         let colNames = map fst withUnit
             err = mkError E2006
                     ("Columns have incompatible units: "
                      <> T.intercalate ", " (map (\u -> "'" <> u <> "'") units)
                      <> ". Arithmetic across these columns may be incorrect.")
                     [ "Standardise all monetary/physical columns to the same unit."
                     , "Explicitly separate analyses that use different units."
                     ]
                     Warning
         in [ InvariantViolation
                { ivInvariant   = "unit-consistency"
                , ivColumns     = colNames
                , ivDescription =
                    "Columns with incompatible units: "
                    <> T.intercalate ", "
                         (map (\(n, u) -> n <> " (" <> u <> ")") withUnit)
                , ivError = err
                }
            ]
  where
    knownUnits :: [(Text, Text)]
    knownUnits =
      [ ("_usd","USD"),("_eur","EUR"),("_gbp","GBP"),("_jpy","JPY")
      , ("_inr","INR"),("_cad","CAD"),("_aud","AUD"),("_chf","CHF")
      , ("_km","km"),("_mi","mi"),("_kg","kg"),("_lb","lb")
      , ("_m","m"),("_ft","ft"),("_l","L"),("_ml","mL")
      ]

    inferUnit :: Text -> Maybe Text
    inferUnit name =
      let lower = T.toLower name
      in foldr (\(suf, unit) acc ->
                  if suf `T.isSuffixOf` lower then Just unit else acc)
               Nothing knownUnits

-- ---------------------------------------------------------------------------
-- New Phase 2: checkSignConstraints
-- ---------------------------------------------------------------------------

-- | Heuristic: columns whose names imply non-negativity should not have
-- negative values. Returns 'Warning'-level violations (not hard errors).
checkSignConstraints
  :: Vector (Text, MeasurementScale)
  -> ValidatedFrame
  -> [InvariantViolation]
checkSignConstraints colSpecs vf =
  concatMap checkCol (V.toList colSpecs)
  where
    checkCol (colName, _) =
      if not (looksNonNegative colName)
        then []
        else
          case lookupNumericVec colName vf of
            Nothing  -> []
            Just vec ->
              let negCount = V.length $ V.filter (< 0) vec
              in if negCount == 0
                   then []
                   else
                     let err = addContext
                                 (DataContext colName 0
                                   (T.pack (show negCount) <> " negative values"))
                               $ mkError E2001
                                   ("Column '" <> colName <> "' contains "
                                    <> T.pack (show negCount)
                                    <> " negative value(s) but its name suggests "
                                    <> "it should be non-negative.")
                                   [ "Verify whether negatives represent "
                                     <> "credits/adjustments (acceptable) or errors."
                                   ]
                                   Warning
                     in [ InvariantViolation
                            { ivInvariant   = "sign-constraint"
                            , ivColumns     = [colName]
                            , ivDescription =
                                "Column '" <> colName <> "' has "
                                <> T.pack (show negCount) <> " negative value(s)."
                            , ivError = err
                            }
                        ]

    nonNegKeywords :: [Text]
    nonNegKeywords =
      [ "revenue","price","count","sales","amount","quantity"
      , "cost","spend","fee","rate","income","profit","volume"
      , "total","sum","balance","stock","inventory","units"
      ]

    looksNonNegative :: Text -> Bool
    looksNonNegative name =
      let lower = T.toLower name
      in any (`T.isInfixOf` lower) nonNegKeywords

    lookupNumericVec :: Text -> ValidatedFrame -> Maybe (Vector Double)
    lookupNumericVec name frame =
      case filter (\c -> columnName c == name) (V.toList (vfColumns frame)) of
        (c:_) -> getNumericVec c
        []    -> Nothing

-- ---------------------------------------------------------------------------
-- New Phase 2: checkMonotonicity
-- ---------------------------------------------------------------------------

-- | Verify that a time-series column is strictly monotone increasing.
-- Returns 'Left E2007' at the first violating index.
-- An empty or single-element vector passes by definition.
checkMonotonicity :: Text -> Vector Double -> SCEResult ()
checkMonotonicity colName values
  | V.length values < 2 = Right ()
  | otherwise           = go 0
  where
    go i
      | i >= V.length values - 1 = Right ()
      | values V.! (i + 1) <= values V.! i =
          Left
            $ addContext (DataContext colName i
                (T.pack (show (values V.! i))
                 <> " -> " <> T.pack (show (values V.! (i + 1)))))
            $ mkError E2007
                ("Column '" <> colName
                 <> "' violates strict monotone order at row "
                 <> T.pack (show i)
                 <> " (" <> T.pack (show (values V.! i))
                 <> " >= " <> T.pack (show (values V.! (i + 1))) <> ").")
                [ "Sort the data by the time column before analysis."
                , "Check for duplicate timestamps or reversed row order."
                ]
                Error
      | otherwise = go (i + 1)

-- ---------------------------------------------------------------------------
-- New Phase 2: checkSumConstraint
-- ---------------------------------------------------------------------------

-- | Verify that part-columns sum to a whole-column within tolerance.
-- Returns 'Left E2005' with the first violating row index.
-- Returns 'Left E2004' when any named column is absent or non-numeric.
checkSumConstraint
  :: [Text]          -- ^ Part column names
  -> Text            -- ^ Whole column name
  -> Double          -- ^ Absolute tolerance (e.g. 0.01)
  -> ValidatedFrame
  -> SCEResult ()
checkSumConstraint partNames wholeName tol vf = do
  wholeVec <- requireNumeric wholeName vf
  partVecs <- mapM (`requireNumeric` vf) partNames
  let nRows = V.length wholeVec
  go 0 nRows wholeVec partVecs
  where
    go i n whole parts
      | i >= n    = Right ()
      | otherwise =
          let wholeVal = whole V.! i
              partSum  = sum [ if i < V.length pv then pv V.! i else 0
                             | pv <- parts ]
              diff     = abs (wholeVal - partSum)
          in if diff > tol
               then Left
                 $ addContext (DataContext wholeName i
                     ("whole=" <> T.pack (show wholeVal)
                      <> " parts_sum=" <> T.pack (show partSum)))
                 $ mkError E2005
                     ("Sum constraint violated at row " <> T.pack (show i)
                      <> ": parts sum to " <> T.pack (show partSum)
                      <> " but whole column '" <> wholeName
                      <> "' is " <> T.pack (show wholeVal)
                      <> " (diff=" <> T.pack (show diff)
                      <> " > tol=" <> T.pack (show tol) <> ").")
                     [ "Check for rounding errors in the source data."
                     , "Verify that all part columns are included in the sum."
                     ]
                     Error
               else go (i + 1) n whole parts

    requireNumeric :: Text -> ValidatedFrame -> SCEResult (Vector Double)
    requireNumeric name frame =
      case filter (\c -> columnName c == name) (V.toList (vfColumns frame)) of
        (c:_) ->
          case getNumericVec c of
            Just v  -> Right v
            Nothing ->
              Left $ mkError E2004
                ("Column '" <> name
                 <> "' is not numeric and cannot be used in a sum constraint.")
                ["Ensure all part and whole columns are Ratio or Interval scale."]
                Error
        [] ->
          Left $ mkError E2004
            ("Column '" <> name <> "' not found in the frame.")
            ["Check the column name spelling in the schema confirmation."]
            Error

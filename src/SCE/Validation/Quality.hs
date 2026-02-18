{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Validation.Quality
Description : Data quality reporting on a ValidatedFrame (Phase 2)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Runs a battery of quality checks on a 'ValidatedFrame' and produces a
human-readable 'QualityReport'. All functions are pure — no IO.

Checks performed:
  * Missing rate per column: Poor >= 30 %, Unusable >= 80 %
  * Outlier prevalence via IQR method: Poor if > 10 % of rows are outliers
  * Duplicate rows: any duplicates → Acceptable warning
  * Zero-variance columns: Unusable for any inferential analysis
  * Extreme skewness (|skew| > 3.0): Warning for analysis suitability

Design constraints:
  * No partial functions. Every call is total.
  * ASCII-only output in all 'Text' fields.
-}
module SCE.Validation.Quality
  ( -- * Quality report
    generateQualityReport
    -- * Types
  , QualityReport(..)
  , QualityCheck(..)
  , QualityLevel(..)
  ) where

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
import           Data.List   (nub, sort)


-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Four-level quality classification, ordered from best to worst.
data QualityLevel
  = Good        -- ^ No issues; ready for all analyses.
  | Acceptable  -- ^ Minor issues present; results may need caveats.
  | Poor        -- ^ Significant issues; analysis should be done with caution.
  | Unusable    -- ^ Column or dataset cannot be used for the intended analysis.
  deriving stock (Show, Eq, Ord)

-- | A single quality check and its outcome.
data QualityCheck = QualityCheck
  { qcName           :: Text         -- ^ Short check identifier
  , qcColumn         :: Maybe Text   -- ^ 'Nothing' = table-level check
  , qcLevel          :: QualityLevel -- ^ Severity of the finding
  , qcDescription    :: Text         -- ^ What was found
  , qcRecommendation :: Text         -- ^ What to do about it
  } deriving stock (Show)

-- | Aggregated quality report for an entire 'ValidatedFrame'.
data QualityReport = QualityReport
  { qrOverallLevel     :: QualityLevel -- ^ Worst level across all checks
  , qrChecks           :: [QualityCheck]
  , qrReadyForAnalysis :: Bool          -- ^ False when any check is 'Unusable'
  , qrSummary          :: Text          -- ^ One-paragraph ASCII summary
  } deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Run all quality checks on a 'ValidatedFrame'. Pure, no IO.
generateQualityReport :: ValidatedFrame -> QualityReport
generateQualityReport vf =
  let cols    = V.toList (vfColumns vf)
      nRows   = vfRowCount vf

      -- Per-column checks
      colChecks = concatMap (columnChecks nRows) cols

      -- Table-level checks
      tblChecks = tableChecks vf

      allChecks = colChecks ++ tblChecks
      overall   = worstLevel allChecks
      ready     = overall /= Unusable
      summary   = buildSummary overall (length allChecks) nRows cols

  in QualityReport
       { qrOverallLevel     = overall
       , qrChecks           = allChecks
       , qrReadyForAnalysis = ready
       , qrSummary          = summary
       }

-- ---------------------------------------------------------------------------
-- Per-column checks
-- ---------------------------------------------------------------------------

columnChecks :: Int -> TypedColumn -> [QualityCheck]
columnChecks nRows col =
  let name = columnName col
  in  missingCheck name col nRows
   ++ outlierCheck  name col
   ++ varianceCheck name col
   ++ skewnessCheck name col

-- | Check 1: missing rate
missingCheck :: Text -> TypedColumn -> Int -> [QualityCheck]
missingCheck colName col nRows =
  case getNumericVec col of
    Nothing -> []  -- categoric columns: skip (missingness already in MissingnessReport)
    Just vec ->
      -- For numeric columns, missing values were dropped during coercion;
      -- compare vector length against total row count.
      let missingN   = nRows - V.length vec
          missingPct :: Double
          missingPct = if nRows == 0 then 0
                       else fromIntegral missingN / fromIntegral nRows * 100
          level
            | missingPct >= 80 = Unusable
            | missingPct >= 30 = Poor
            | missingPct >= 5  = Acceptable
            | otherwise        = Good
      in if level == Good
           then []
           else [ QualityCheck
                    { qcName           = "missing-rate"
                    , qcColumn         = Just colName
                    , qcLevel          = level
                    , qcDescription    =
                        "Column '" <> colName <> "' has "
                        <> formatPct missingPct <> " missing values ("
                        <> T.pack (show missingN) <> " of "
                        <> T.pack (show nRows) <> " rows)."
                    , qcRecommendation =
                        if level == Unusable
                          then "Drop this column or use imputation before analysis."
                          else "Consider imputation or document the missing-data mechanism."
                    }
                ]

-- | Check 2: outlier prevalence via Tukey IQR fence (1.5 * IQR)
outlierCheck :: Text -> TypedColumn -> [QualityCheck]
outlierCheck colName col =
  case getNumericVec col of
    Nothing  -> []
    Just vec ->
      if V.length vec < 4
        then []  -- not enough data to compute IQR sensibly
        else
          let sorted   = V.fromList $ sort $ V.toList vec
              q1       = quantile sorted 0.25
              q3       = quantile sorted 0.75
              iqr      = q3 - q1
              fence    = 1.5 * iqr
              lo       = q1 - fence
              hi       = q3 + fence
              outliers = V.length $ V.filter (\x -> x < lo || x > hi) vec
              pct :: Double
              pct      = fromIntegral outliers / fromIntegral (V.length vec) * 100
          in if pct > 10
               then [ QualityCheck
                        { qcName           = "outlier-prevalence"
                        , qcColumn         = Just colName
                        , qcLevel          = Poor
                        , qcDescription    =
                            "Column '" <> colName <> "' has "
                            <> formatPct pct <> " outliers by IQR method ("
                            <> T.pack (show outliers) <> " values outside ["
                            <> formatDbl lo <> ", " <> formatDbl hi <> "])."
                        , qcRecommendation =
                            "Investigate outliers before analysis. "
                            <> "Consider robust statistical methods."
                        }
                    ]
               else []

-- | Check 3: zero-variance columns
varianceCheck :: Text -> TypedColumn -> [QualityCheck]
varianceCheck colName col =
  case getNumericVec col of
    Nothing  -> []
    Just vec ->
      if V.length vec < 2
        then []
        else
          let mn  = V.sum vec / fromIntegral (V.length vec)
              var = V.sum (V.map (\x -> (x - mn) ^ (2 :: Int)) vec)
                    / fromIntegral (V.length vec - 1)
          in if var == 0
               then [ QualityCheck
                        { qcName           = "zero-variance"
                        , qcColumn         = Just colName
                        , qcLevel          = Unusable
                        , qcDescription    =
                            "Column '" <> colName
                            <> "' has zero variance (all values are identical)."
                        , qcRecommendation =
                            "Drop this column; it adds no information to analyses "
                            <> "and will cause singular-matrix errors in regressions."
                        }
                    ]
               else []

-- | Check 4: extreme skewness warning
skewnessCheck :: Text -> TypedColumn -> [QualityCheck]
skewnessCheck colName col =
  case getNumericVec col of
    Nothing  -> []
    Just vec ->
      if V.length vec < 3
        then []
        else
          let n    = fromIntegral (V.length vec) :: Double
              mn   = V.sum vec / n
              s    = sqrt (V.sum (V.map (\x -> (x - mn) ^ (2 :: Int)) vec) / (n - 1))
              skew = if s == 0 then 0
                     else V.sum (V.map (\x -> ((x - mn) / s) ^ (3 :: Int)) vec)
                          * n / ((n - 1) * (n - 2))
          in if abs skew > 3.0
               then [ QualityCheck
                        { qcName           = "extreme-skewness"
                        , qcColumn         = Just colName
                        , qcLevel          = Acceptable
                        , qcDescription    =
                            "Column '" <> colName <> "' has extreme skewness ("
                            <> formatDbl skew <> "). "
                            <> "Normality-based tests may be unreliable."
                        , qcRecommendation =
                            "Consider a log or Box-Cox transformation, "
                            <> "or use non-parametric methods."
                        }
                    ]
               else []

-- ---------------------------------------------------------------------------
-- Table-level checks
-- ---------------------------------------------------------------------------

tableChecks :: ValidatedFrame -> [QualityCheck]
tableChecks vf = duplicateRowCheck vf

-- | Warn when duplicate rows are detected (uses numeric columns as key).
duplicateRowCheck :: ValidatedFrame -> [QualityCheck]
duplicateRowCheck vf =
  let numericCols = [ vec | col <- V.toList (vfColumns vf)
                          , Just vec <- [getNumericVec col] ]
  in case numericCols of
       []    -> []
       (v:_) ->
         -- Heuristic: compare sorted row fingerprints using first numeric col
         -- A full row-hash would require many columns; this catches obvious dupes.
         let vals    = V.toList v
             sorted  = sort vals
             hasDups = length sorted /= length (nub sorted)
         in if hasDups
              then [ QualityCheck
                       { qcName           = "duplicate-rows"
                       , qcColumn         = Nothing
                       , qcLevel          = Acceptable
                       , qcDescription    =
                           "Possible duplicate rows detected "
                           <> "(repeated values in numeric columns)."
                       , qcRecommendation =
                           "Verify whether duplicates are intentional. "
                           <> "Remove exact duplicates before analysis if not."
                       }
                   ]
              else []

-- ---------------------------------------------------------------------------
-- Aggregation helpers
-- ---------------------------------------------------------------------------

worstLevel :: [QualityCheck] -> QualityLevel
worstLevel [] = Good
worstLevel cs = maximum (map qcLevel cs)

buildSummary :: QualityLevel -> Int -> Int -> [TypedColumn] -> Text
buildSummary overall nChecks nRows cols =
  let numCols   = length cols
      levelDesc = case overall of
        Good       -> "All quality checks passed."
        Acceptable -> "Minor quality issues detected; proceed with caution."
        Poor       -> "Significant quality issues detected; review before analysis."
        Unusable   -> "Critical quality issues detected; analysis is not recommended."
  in  levelDesc
   <> " Frame: " <> T.pack (show nRows) <> " rows, "
   <> T.pack (show numCols) <> " columns. "
   <> T.pack (show nChecks) <> " checks run."
   <> (if overall == Good then "" else " See individual checks for details.")

-- ---------------------------------------------------------------------------
-- Numeric utilities (all total)
-- ---------------------------------------------------------------------------

-- | Type-7 quantile (matches R and NumPy default).
-- Requires a sorted input vector of length >= 1.
quantile :: Vector Double -> Double -> Double
quantile sorted p
  | V.null sorted = 0
  | otherwise =
      let n   = V.length sorted
          idx = p * fromIntegral (n - 1)
          lo  = floor idx   :: Int
          hi  = ceiling idx :: Int
          frac = idx - fromIntegral lo
      in if lo >= n - 1
           then sorted V.! (n - 1)
           else sorted V.! lo + frac * (sorted V.! hi - sorted V.! lo)

formatPct :: Double -> Text
formatPct p = T.pack (show (round p :: Int)) <> "%"

formatDbl :: Double -> Text
formatDbl d = T.pack (show (fromIntegral (round d :: Int) :: Int))

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Statistics.Correlation
Description : Correlation analysis with TestResult output (Phase 3)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Wraps 'SCE.Core.Correlation' in the Phase-3 'TestResult' / 'CorrelationResult'
types so downstream consumers (charts, CLI) get consistent, annotation-rich output.

'SCE.Core.Correlation' remains the numeric back-end.  This module is the public
API for Phase 3.

== Exported

  * 'pearson'            — Pearson product-moment r.
  * 'spearman'           — Spearman rank correlation.
  * 'kendallTau'         — Kendall τ-b.
  * 'partialCorrelation' — First-order (and higher-order) partial correlation.
  * 'CorrelationResult'  — Result type with coefficient, CI, p-value, and interpretation.
-}
module SCE.Statistics.Correlation
  ( -- * Correlation functions
    pearson
  , spearman
  , kendallTau
  , partialCorrelation
    -- * Result type
  , CorrelationResult(..)
  , interpretCorrelation
  ) where

import SCE.Core.Types
  ( SCEResult, mkError, ErrorCode(..), Severity(..) )
import SCE.Statistics.TestResult
  ( ConfidenceInterval(..) )
import qualified SCE.Core.Correlation as Core

import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- ---------------------------------------------------------------------------
-- CorrelationResult
-- ---------------------------------------------------------------------------

-- | Rich correlation result, aligned with the Phase-3 TestResult conventions.
data CorrelationResult = CorrelationResult
  { crCoefficient    :: Double            -- ^ Always in [-1, 1] (clamped)
  , crCI             :: Maybe ConfidenceInterval
    -- ^ 95% CI via Fisher's z-transformation (Nothing when n < 10).
  , crPValue         :: Maybe Double      -- ^ Two-sided p-value (Nothing when n < 10)
  , crN              :: Int               -- ^ Sample size used
  , crInterpretation :: T.Text            -- ^ Plain-English summary
  } deriving stock (Show, Eq)

-- | Plain-English strength label.
--
-- Convention: |r| < 0.1 = negligible, < 0.3 = weak, < 0.5 = moderate,
-- < 0.7 = strong, >= 0.7 = very strong.
interpretCorrelation :: Double -> T.Text
interpretCorrelation r =
  let a = abs r
  in (if r < 0 then "negative " else "positive ")
     <> if a < 0.1 then "negligible"
        else if a < 0.3 then "weak"
        else if a < 0.5 then "moderate"
        else if a < 0.7 then "strong"
        else "very strong"

-- ---------------------------------------------------------------------------
-- pearson
-- ---------------------------------------------------------------------------

-- | Pearson product-moment correlation coefficient.
--
-- Requires Interval or Ratio scale (caller's responsibility to check scale).
-- Internally uses the numerically stable implementation in 'SCE.Core.Correlation'.
--
-- Returns 'Left E2002' if n < 3.
-- Returns 'Left E2001' if vectors have different lengths.
pearson :: Vector Double -> Vector Double -> SCEResult CorrelationResult
pearson xs ys = do
  cr <- Core.pearsonCorrelation xs ys
  let r    = Core.corrValue cr
      ci   = case (Core.corrCI95Lo cr, Core.corrCI95Hi cr) of
               (Just lo, Just hi) -> Just (ConfidenceInterval 0.95 lo hi)
               _                  -> Nothing
  return CorrelationResult
    { crCoefficient    = r
    , crCI             = ci
    , crPValue         = Core.corrPValue cr
    , crN              = Core.corrN cr
    , crInterpretation = "Pearson r = " <> T.pack (show (round3 r))
                         <> " (" <> interpretCorrelation r <> ")"
                         <> maybe "" (\p -> ", p = " <> formatP p) (Core.corrPValue cr)
    }

-- ---------------------------------------------------------------------------
-- spearman
-- ---------------------------------------------------------------------------

-- | Spearman rank correlation coefficient.
--
-- Handles ties by averaging ranks (mid-rank method).
-- Appropriate for Ordinal scale or non-normal distributions.
-- Returns 'Left E2002' if n < 3.
spearman :: Vector Double -> Vector Double -> SCEResult CorrelationResult
spearman xs ys = do
  cr <- Core.spearmanCorrelation xs ys
  let r  = Core.corrValue cr
      ci = case (Core.corrCI95Lo cr, Core.corrCI95Hi cr) of
             (Just lo, Just hi) -> Just (ConfidenceInterval 0.95 lo hi)
             _                  -> Nothing
  return CorrelationResult
    { crCoefficient    = r
    , crCI             = ci
    , crPValue         = Core.corrPValue cr
    , crN              = Core.corrN cr
    , crInterpretation = "Spearman ρ = " <> T.pack (show (round3 r))
                         <> " (" <> interpretCorrelation r <> ")"
                         <> maybe "" (\p -> ", p = " <> formatP p) (Core.corrPValue cr)
    }

-- ---------------------------------------------------------------------------
-- kendallTau
-- ---------------------------------------------------------------------------

-- | Kendall τ-b correlation.
--
-- More robust than Spearman for small samples with ties.
-- Returns 'Left E2002' if n < 3.
kendallTau :: Vector Double -> Vector Double -> SCEResult CorrelationResult
kendallTau xs ys = do
  cr <- Core.kendallTau xs ys
  let r  = Core.corrValue cr
      ci = case (Core.corrCI95Lo cr, Core.corrCI95Hi cr) of
             (Just lo, Just hi) -> Just (ConfidenceInterval 0.95 lo hi)
             _                  -> Nothing
  return CorrelationResult
    { crCoefficient    = r
    , crCI             = ci
    , crPValue         = Core.corrPValue cr
    , crN              = Core.corrN cr
    , crInterpretation = "Kendall τ-b = " <> T.pack (show (round3 r))
                         <> " (" <> interpretCorrelation r <> ")"
                         <> maybe "" (\p -> ", p = " <> formatP p) (Core.corrPValue cr)
    }

-- ---------------------------------------------------------------------------
-- partialCorrelation
-- ---------------------------------------------------------------------------

-- | First-order (and higher-order) partial correlation of X and Y, controlling for Z.
--
-- Uses the recursive formula:
--   r_{XY|Z} = (r_{XY} - r_{XZ}·r_{YZ}) / sqrt((1-r_{XZ}²)(1-r_{YZ}²))
-- Extended to multiple control variables by iterative application.
--
-- Returns 'Left E2002' if n < 3 or any required correlation cannot be computed.
partialCorrelation
  :: Vector Double    -- ^ X
  -> Vector Double    -- ^ Y
  -> [Vector Double]  -- ^ Control variables Z₁, Z₂, …
  -> SCEResult CorrelationResult
partialCorrelation xs ys zs = do
  cr <- Core.partialCorrelation xs ys zs
  let r  = Core.corrValue cr
      ci = case (Core.corrCI95Lo cr, Core.corrCI95Hi cr) of
             (Just lo, Just hi) -> Just (ConfidenceInterval 0.95 lo hi)
             _                  -> Nothing
      k  = length zs
      lbl = case k of
              0 -> "Pearson r"
              1 -> "Partial r (1 covariate)"
              _ -> "Partial r (" <> T.pack (show k) <> " covariates)"
  return CorrelationResult
    { crCoefficient    = r
    , crCI             = ci
    , crPValue         = Core.corrPValue cr
    , crN              = Core.corrN cr
    , crInterpretation = lbl <> " = " <> T.pack (show (round3 r))
                         <> " (" <> interpretCorrelation r <> ")"
                         <> maybe "" (\p -> ", p = " <> formatP p) (Core.corrPValue cr)
    }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

formatP :: Double -> T.Text
formatP p = T.pack (show (fromIntegral (round (p * 1000) :: Int) / 1000.0 :: Double))

round3 :: Double -> Double
round3 x = fromIntegral (round (x * 1000) :: Int) / 1000.0

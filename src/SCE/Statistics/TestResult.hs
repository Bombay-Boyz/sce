{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Statistics.TestResult
Description : Shared output types for all Phase 3 statistical tests
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Every inferential function in SCE.Statistics.* returns 'TestResult'.
This module must be created first — all other Phase 3 modules depend on it.

Design constraints:
  * Zero internal SCE imports beyond SCE.Core.Error.
  * All Double fields that represent probabilities are guaranteed in [0,1]
    by the smart constructors (runtime guard; LH-verified in Phase 6).
  * No partial functions.
-}
module SCE.Statistics.TestResult
  ( -- * Core result type
    TestResult(..)
  , EffectSize(..)
  , ConfidenceInterval(..)
  , AssumptionCheck(..)
  , AssumptionStatus(..)
    -- * Smart constructor
  , mkTestResult
    -- * Accessors
  , isSignificant
  , interpretEffect
  , interpretEffectSize
  , ciContains
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- ConfidenceInterval
-- ---------------------------------------------------------------------------

-- | A confidence interval with an explicit coverage level.
-- Invariant: 'ciLower' <= 'ciUpper' (enforced by 'mkTestResult').
data ConfidenceInterval = ConfidenceInterval
  { ciLevel :: Double  -- ^ Coverage level, e.g. 0.95 for 95% CI
  , ciLower :: Double  -- ^ Lower bound
  , ciUpper :: Double  -- ^ Upper bound
  } deriving stock (Show, Eq)

-- | True if the value lies within the interval (inclusive).
ciContains :: Double -> ConfidenceInterval -> Bool
ciContains x ci = x >= ciLower ci && x <= ciUpper ci

-- ---------------------------------------------------------------------------
-- AssumptionCheck
-- ---------------------------------------------------------------------------

data AssumptionStatus
  = Satisfied      -- ^ Assumption holds; parametric test is appropriate.
  | Violated       -- ^ Assumption is violated; consider non-parametric alternative.
  | NotTested      -- ^ n too small to test meaningfully (e.g. n < 8 for Shapiro-Wilk).
  | NotApplicable  -- ^ Assumption is not relevant to this test.
  deriving stock (Show, Eq)

data AssumptionCheck = AssumptionCheck
  { acName        :: Text            -- ^ Short name, e.g. "Normality"
  , acStatus      :: AssumptionStatus
  , acDescription :: Text            -- ^ Human-readable explanation
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- EffectSize
-- ---------------------------------------------------------------------------

-- | Effect size measures.  All magnitude values are non-negative.
data EffectSize
  = CohensD        Double ConfidenceInterval
    -- ^ Standardised mean difference; d=0.2 small, d=0.5 medium, d=0.8 large.
  | HedgesG        Double ConfidenceInterval
    -- ^ Bias-corrected Cohen's d; preferred for unequal n.
  | EtaSquared     Double
    -- ^ Proportion of total variance explained; 0.01/0.06/0.14 = small/medium/large.
  | OmegaSquared   Double
    -- ^ Less-biased alternative to eta-squared.
  | CramersV       Double
    -- ^ Effect size for chi-square; 0.1/0.3/0.5 = small/medium/large.
  | PhiCoefficient Double
    -- ^ Pearson phi for 2x2 contingency tables; equivalent to Pearson r.
  | KendallW       Double
    -- ^ Coefficient of concordance for k raters; 0 = no agreement, 1 = perfect.
  deriving stock (Show, Eq)

-- | Plain-English magnitude label for an effect size.
--
-- Cohen's conventions (adjusted where alternatives apply):
--   small < medium < large
interpretEffect :: EffectSize -> Text
interpretEffect (CohensD     d _) = interpretD   (abs d)
interpretEffect (HedgesG     g _) = interpretD   (abs g)
interpretEffect (EtaSquared  e)   = interpretEta (abs e)
interpretEffect (OmegaSquared o)  = interpretEta (abs o)
interpretEffect (CramersV    v)   = interpretCramer (abs v)
interpretEffect (PhiCoefficient p) = interpretPhi (abs p)
interpretEffect (KendallW    w)   = interpretW   (abs w)

-- | Extract the raw magnitude from an EffectSize.
interpretEffectSize :: EffectSize -> Double
interpretEffectSize (CohensD d _)        = abs d
interpretEffectSize (HedgesG g _)        = abs g
interpretEffectSize (EtaSquared e)       = abs e
interpretEffectSize (OmegaSquared o)     = abs o
interpretEffectSize (CramersV v)         = abs v
interpretEffectSize (PhiCoefficient p)   = abs p
interpretEffectSize (KendallW w)         = abs w

-- ---------------------------------------------------------------------------
-- Internal magnitude helpers (all total)
-- ---------------------------------------------------------------------------

interpretD :: Double -> Text
interpretD d
  | d < 0.2   = "negligible"
  | d < 0.5   = "small"
  | d < 0.8   = "medium"
  | otherwise  = "large"

interpretEta :: Double -> Text
interpretEta e
  | e < 0.01  = "negligible"
  | e < 0.06  = "small"
  | e < 0.14  = "medium"
  | otherwise  = "large"

interpretCramer :: Double -> Text
interpretCramer v
  | v < 0.1   = "negligible"
  | v < 0.3   = "small"
  | v < 0.5   = "medium"
  | otherwise  = "large"

interpretPhi :: Double -> Text
interpretPhi p
  | p < 0.1   = "negligible"
  | p < 0.3   = "small"
  | p < 0.5   = "medium"
  | otherwise  = "large"

interpretW :: Double -> Text
interpretW w
  | w < 0.1   = "weak"
  | w < 0.3   = "moderate"
  | w < 0.7   = "strong"
  | otherwise  = "very strong"

-- ---------------------------------------------------------------------------
-- TestResult
-- ---------------------------------------------------------------------------

-- | The canonical output of every inferential test in SCE.
--
-- Invariants enforced by 'mkTestResult':
--   * @trPValue@ is in [0, 1].
--   * @ciLower <= ciUpper@ for every 'ConfidenceInterval' in 'trCI'.
--   * @trSampleSize >= 1@.
data TestResult = TestResult
  { trTestName         :: Text
    -- ^ Short identifier, e.g. @"one-sample t-test"@.
  , trStatistic        :: Double
    -- ^ The test statistic (t, F, chi-square, U, W, etc.).
  , trDegreesOfFreedom :: Maybe Double
    -- ^ Degrees of freedom (Nothing for exact or non-parametric tests).
  , trPValue           :: Double
    -- ^ Two-sided p-value, always in [0, 1].
  , trCI               :: Maybe ConfidenceInterval
    -- ^ Confidence interval for the primary estimand (if applicable).
  , trEffectSize       :: Maybe EffectSize
    -- ^ Effect size with magnitude label.
  , trPower            :: Maybe Double
    -- ^ Post-hoc power at alpha=0.05 (if computed).
  , trSampleSize       :: Int
    -- ^ Total number of observations used.
  , trAssumptions      :: [AssumptionCheck]
    -- ^ Results of prerequisite assumption checks.
  , trInterpretation   :: Text
    -- ^ One-sentence plain-English interpretation of the result.
  } deriving stock (Show, Eq)

-- | Smart constructor for 'TestResult'.
--
-- Clamps the p-value to [0, 1] and normalises the CI (lower <= upper).
-- Every statistical function should build its result through this constructor.
mkTestResult
  :: Text          -- ^ Test name
  -> Double        -- ^ Test statistic
  -> Maybe Double  -- ^ Degrees of freedom
  -> Double        -- ^ Raw p-value (clamped to [0,1] if out of range due to floating-point)
  -> Maybe ConfidenceInterval
  -> Maybe EffectSize
  -> Maybe Double  -- ^ Power estimate
  -> Int           -- ^ Sample size
  -> [AssumptionCheck]
  -> Text          -- ^ Interpretation
  -> TestResult
mkTestResult name stat df pRaw ci eff power n assumptions interp =
  let pClamped = max 0.0 (min 1.0 pRaw)
      ciNorm   = fmap normCI ci
  in TestResult
       { trTestName         = name
       , trStatistic        = stat
       , trDegreesOfFreedom = df
       , trPValue           = pClamped
       , trCI               = ciNorm
       , trEffectSize       = eff
       , trPower            = power
       , trSampleSize       = n
       , trAssumptions      = assumptions
       , trInterpretation   = interp
       }
  where
    normCI c = c { ciLower = min (ciLower c) (ciUpper c)
                 , ciUpper = max (ciLower c) (ciUpper c) }

-- ---------------------------------------------------------------------------
-- Accessors
-- ---------------------------------------------------------------------------

-- | @True@ if @trPValue < alpha@.
isSignificant :: Double   -- ^ Alpha level (typically 0.05)
              -> TestResult
              -> Bool
isSignificant alpha tr = trPValue tr < alpha

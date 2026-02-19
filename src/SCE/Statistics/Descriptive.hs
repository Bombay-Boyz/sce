{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Statistics.Descriptive
Description : Full descriptive statistics with confidence intervals (Phase 3)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Wraps and extends 'SCE.Core.Statistics' with:
  * Coefficient of Variation (CV) — Ratio scale only.
  * 95% CI for the mean using the t-distribution (Cornish-Fisher expansion).
  * Standard quantile grid [(p%, value)].
  * An extended 'DescriptiveStats' record with all of the above.

The module re-exports the individual statistics from 'SCE.Core.Statistics' so
callers can depend on this module alone.

== Liquid Haskell annotations (stubs — activated in Phase 6)

{-@ computeMean :: xs:NonEmptyVec Double
               -> SCEResult {v:Double | v >= vMin xs && v <= vMax xs} @-}
{-@ computeVariance :: xs:MinSizeVec Double 2 -> SCEResult NonNegative @-}
{-@ computeStdDev   :: xs:MinSizeVec Double 2 -> SCEResult NonNegative @-}
{-@ computeIQR      :: xs:NonEmptyVec Double  -> SCEResult NonNegative @-}
-}
module SCE.Statistics.Descriptive
  ( -- * Extended descriptive statistics record
    DescriptiveStats(..)
  , computeDescriptiveStats

    -- * Confidence interval for the mean
  , computeCI

    -- * Coefficient of Variation
  , computeCV

    -- * Standard quantile grid
  , computeQuantiles

    -- * Re-exported individual statistics
  , computeMean
  , computeMedian
  , computeMode
  , computeStdDev
  , computeVariance
  , computeSkewness
  , computeKurtosis
  , computeMAD
  , computeIQR
  , computePercentile
  ) where

import SCE.Core.Types
  ( SCEResult, mkError, ErrorCode(..), Severity(..), MeasurementScale(..) )
import SCE.Core.Statistics
  ( computeMean
  , computeMedian
  , computeMode
  , computeStdDev
  , computeVariance
  , computeSkewness
  , computeKurtosis
  , computeMAD
  , computeIQR
  , computePercentile
  , kahanMean
  , welfordVariance
  )
import SCE.Core.MeasurementScale ( canComputeMean )
import SCE.Statistics.TestResult ( ConfidenceInterval(..) )

import qualified Data.Text   as T
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- ---------------------------------------------------------------------------
-- Extended DescriptiveStats
-- ---------------------------------------------------------------------------

-- | Comprehensive descriptive statistics for a numeric column.
--
-- Fields that require >= 2 observations (stddev, variance, CI) are 'Maybe'.
-- 'dsCV' is additionally 'Nothing' for non-Ratio scales or mean ≈ 0.
data DescriptiveStats = DescriptiveStats
  { dsCount     :: Int
  , dsMean      :: Maybe Double
    -- ^ 'Nothing' for Nominal/Ordinal scale.
  , dsMeanCI    :: Maybe ConfidenceInterval
    -- ^ 95% CI for the mean (t-distribution, n-1 df).
  , dsMedian    :: Maybe Double
    -- ^ 'Nothing' for Nominal scale.
  , dsMode      :: Maybe Double
  , dsStdDev    :: Maybe Double
    -- ^ Requires n >= 2.
  , dsVariance  :: Maybe Double
    -- ^ Requires n >= 2.
  , dsSkewness  :: Maybe Double
    -- ^ Fisher's bias-corrected g1; requires n >= 3.
  , dsKurtosis  :: Maybe Double
    -- ^ Excess kurtosis G2 (normal = 0); requires n >= 4.
  , dsMin       :: Double
  , dsMax       :: Double
  , dsRange     :: Double
  , dsIQR       :: Double
  , dsMAD       :: Double
    -- ^ Median Absolute Deviation (unscaled).
  , dsCV        :: Maybe Double
    -- ^ Coefficient of Variation (stddev / |mean|).  Ratio scale + n>=2 only.
  , dsQuantiles :: [(Double, Double)]
    -- ^ [(percentile%, value)] for grid 1,5,10,25,50,75,90,95,99.
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- computeCV
-- ---------------------------------------------------------------------------

-- | Coefficient of Variation: @stddev / |mean|@.
--
-- * Returns 'Left E2003' for non-Ratio scales.
-- * Returns 'Left E2002' when n < 2.
-- * Returns 'Left E3001' when |mean| < 1e-12 (CV is undefined).
computeCV :: MeasurementScale -> Vector Double -> SCEResult Double
computeCV scale values
  | scale /= Ratio =
      Left $ mkError E2003
        "Coefficient of Variation is only defined for Ratio scale data"
        ["Use Ratio scale data (non-negative with a true zero)."] Error
  | V.length values < 2 =
      Left $ mkError E2002
        ("CV requires >= 2 observations, got " <> T.pack (show (V.length values)))
        ["Collect more data."] Error
  | otherwise =
      let mn = kahanMean values
          sd = sqrt (welfordVariance values)
      in if mn == 0.0
           then Left $ mkError E3001
                  "CV undefined: mean is zero"
                  ["CV = SD / mean is undefined when mean = 0."] Error
           else Right (sd / abs mn)


-- ---------------------------------------------------------------------------
-- computeCI
-- ---------------------------------------------------------------------------

-- | Confidence interval for the mean using the t-distribution (n-1 df).
--
-- The t-quantile is computed via the Cornish-Fisher expansion
-- (Abramowitz & Stegun 26.7.8).  Accurate to < 5e-4 for df >= 1.
--
-- * Returns 'Left E2002' if n < 2 or @level@ is outside (0, 1).
computeCI :: Double        -- ^ Confidence level, e.g. 0.95
          -> Vector Double
          -> SCEResult ConfidenceInterval
computeCI level values
  | level <= 0.0 || level >= 1.0 =
      Left $ mkError E2002
        ("CI level must be in (0, 1), got " <> T.pack (show level))
        ["Use a value such as 0.95."] Error
  | V.length values < 2 =
      Left $ mkError E2002
        ("CI requires >= 2 observations, got " <> T.pack (show (V.length values)))
        ["Collect more data."] Error
  | otherwise =
      let n  = V.length values
          mn = kahanMean values
          se = sqrt (welfordVariance values / fromIntegral n)
          df = fromIntegral (n - 1) :: Double
          tq = tQuantile ((1.0 + level) / 2.0) df
          lo = mn - tq * se
          hi = mn + tq * se
      in Right ConfidenceInterval
           { ciLevel = level
           , ciLower = min lo hi
           , ciUpper = max lo hi
           }

-- ---------------------------------------------------------------------------
-- computeQuantiles
-- ---------------------------------------------------------------------------

-- | Standard percentile grid: 1, 5, 10, 25, 50, 75, 90, 95, 99.
-- Returns 'Left E2002' if the vector is empty.
computeQuantiles :: Vector Double -> SCEResult [(Double, Double)]
computeQuantiles values
  | V.null values =
      Left $ mkError E2002
        "Cannot compute quantiles: dataset is empty"
        ["Provide at least one data point."] Error
  | otherwise =
      let grid = [1.0, 5.0, 10.0, 25.0, 50.0, 75.0, 90.0, 95.0, 99.0]
      in traverse (\p -> fmap ((,) p) (computePercentile p values)) grid

-- ---------------------------------------------------------------------------
-- computeDescriptiveStats
-- ---------------------------------------------------------------------------

-- | Full descriptive statistics for a numeric vector.
--
-- Statistics that cannot be computed (scale restriction or n too small) are
-- 'Nothing' rather than errors.  Call individual functions for hard failures.
computeDescriptiveStats
  :: MeasurementScale
  -> Vector Double
  -> SCEResult DescriptiveStats
computeDescriptiveStats scale values
  | V.null values =
      Left $ mkError E2002
        "Cannot compute descriptive stats: dataset is empty"
        ["Provide at least one data point."] Error
  | otherwise = do
      let n    = V.length values
          -- Safe min/max: V.null guarded above; V.uncons is total here
          (h, tl) = case V.uncons values of
                      Just p  -> p
                      Nothing -> (0.0, V.empty)   -- unreachable: null guarded
          minV = V.foldl' min h tl
          maxV = V.foldl' max h tl
          mnV  = if canComputeMean scale then Just (kahanMean values) else Nothing
          varV = if n >= 2 && canComputeMean scale
                   then Just (welfordVariance values)
                   else Nothing
          sdV  = fmap sqrt varV
          q1V  = either (const minV) id (computePercentile 25.0 values)
          q3V  = either (const maxV) id (computePercentile 75.0 values)

      medV   <- case computeMedian scale values of
                  Right v -> Right (Just v)
                  Left  _ -> Right Nothing

      ciV    <- case (mnV, n >= 2 && canComputeMean scale) of
                  (Just _, True) -> case computeCI 0.95 values of
                                      Right ci -> Right (Just ci)
                                      Left  _  -> Right Nothing
                  _              -> Right Nothing

      cvV    <- case (scale, n >= 2) of
                  (Ratio, True) -> case computeCV scale values of
                                     Right cv -> Right (Just cv)
                                     Left  _  -> Right Nothing
                  _             -> Right Nothing

      quantV <- computeQuantiles values

      return DescriptiveStats
        { dsCount     = n
        , dsMean      = mnV
        , dsMeanCI    = ciV
        , dsMedian    = medV
        , dsMode      = either (const Nothing) Just (computeMode values)
        , dsStdDev    = sdV
        , dsVariance  = varV
        , dsSkewness  = either (const Nothing) Just (computeSkewness values)
        , dsKurtosis  = either (const Nothing) Just (computeKurtosis values)
        , dsMin       = minV
        , dsMax       = maxV
        , dsRange     = maxV - minV
        , dsIQR       = q3V - q1V
        , dsMAD       = either (const 0.0) id (computeMAD values)
        , dsCV        = cvV
        , dsQuantiles = quantV
        }

-- ---------------------------------------------------------------------------
-- Student-t quantile (Cornish-Fisher expansion)
-- ---------------------------------------------------------------------------

-- | @tQuantile p df@ returns q such that P(T_df <= q) = p.
--
-- Algorithm: Cornish-Fisher expansion of the normal quantile.
-- Abramowitz & Stegun 26.7.8.  Accurate to < 5e-4 for df >= 1, p in (0.5, 0.999).
tQuantile :: Double   -- ^ p ∈ (0, 1)
          -> Double   -- ^ df > 0
          -> Double
tQuantile p df
  | p <= 0.0  = -1.0e15
  | p >= 1.0  =  1.0e15
  | df <= 0.0 =  0.0
  | p < 0.5   = -(tQuantile (1.0 - p) df)
  | otherwise =
      let z  = normalQuantile p
          nu = df
          g1 = (z^(3::Int) + z) / (4.0 * nu)
          g2 = (5.0 * z^(5::Int) + 16.0 * z^(3::Int) + 3.0 * z)
               / (96.0 * nu^(2::Int))
          g3 = (3.0 * z^(7::Int) + 19.0 * z^(5::Int) + 17.0 * z^(3::Int) - 15.0 * z)
               / (384.0 * nu^(3::Int))
          g4 = (79.0 * z^(9::Int) + 776.0 * z^(7::Int) + 1482.0 * z^(5::Int)
                - 1920.0 * z^(3::Int) - 945.0 * z)
               / (92160.0 * nu^(4::Int))
      in z + g1 + g2 + g3 + g4

-- | Standard normal quantile (Acklam's rational approximation).
-- Max error < 1.15e-9 for p ∈ (0, 1).
normalQuantile :: Double -> Double
normalQuantile p
  | p <= 0.0  = -1.0e15
  | p >= 1.0  =  1.0e15
  | p < 0.5   = -(rationalApprox (sqrt (-2.0 * log p)))
  | otherwise =   rationalApprox (sqrt (-2.0 * log (1.0 - p)))
  where
    rationalApprox t =
      let num = t * ( -3.969683028665376e+01
              + t * (  2.209460984245205e+02
              + t * ( -2.759285104469687e+02
              + t * (  1.383577518672690e+02
              + t * ( -3.066479806614716e+01
              + t *    2.506628277459239e+00)))))
          den = 1.0
              + t * ( -5.447609879822406e+01
              + t * (  1.615858368580409e+02
              + t * ( -1.556989798598866e+02
              + t * (  6.680131188771972e+01
              + t *  (-1.328068155288572e+01)))))
      in num / den

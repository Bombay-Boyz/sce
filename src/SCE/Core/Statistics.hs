{-# LANGUAGE StrictData        #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

{-|
Module      : SCE.Core.Statistics
Description : Numerically-stable descriptive statistics (Phase 2.2)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

== Numerical algorithms

  * Mean     : Kahan compensated summation (reduces error to O(ε)).
  * Variance : Welford one-pass online algorithm, Bessel correction (n-1).
  * Median   : IntroSort copy via vector-algorithms.
  * Quantile : Type-7 linear interpolation (matches R / NumPy default).
  * Skewness : bias-corrected Fisher-Pearson (matches R e1071 type=2).
  * Kurtosis : bias-corrected excess kurtosis (Joanes & Gill 1998, G2).
  * MAD      : median(|x_i - median(x)|) — unscaled.

== Liquid Haskell annotations (executable documentation)

{-@ type NonEmpty  a = {v:V.Vector a | vlen v > 0}   @-}
{-@ type AtLeast2  a = {v:V.Vector a | vlen v >= 2}  @-}
{-@ type Prob        = {v:Double     | 0 <= v && v <= 100} @-}
-}
module SCE.Core.Statistics
  ( -- * Central tendency
    computeMean
  , computeMedian
  , computeMode

    -- * Dispersion
  , computeStdDev
  , computeVariance

    -- * Distribution shape (Phase 2.2)
  , computeSkewness
  , computeKurtosis
  , computeMAD
  , computeIQR

    -- * Range helpers
  , computeRange
  , computeMin
  , computeMax
  , computePercentile

    -- * Comprehensive summary (Phase 2.2)
  , DescriptiveStats(..)
  , computeDescriptiveStats

    -- * Backward-compat summary
  , SummaryStats(..)
  , computeSummaryStats

    -- * Time-series helpers
  , computeDelta
  , computePercentChange
  , computeCumulative

    -- * Stable building blocks (used by Correlation module)
  , kahanSum
  , kahanMean
  , welfordVariance
  ) where

import SCE.Core.Types
  ( SCEResult, mkError, ErrorCode(..), Severity(..)
  , MeasurementScale(..)
  )
import SCE.Core.MeasurementScale
  ( canComputeMean, canComputeMedian )

import qualified Data.Text                    as T
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as VA
import           Data.List                    (sort, group)

-- ---------------------------------------------------------------------------
-- Kahan compensated summation
-- ---------------------------------------------------------------------------

-- | Kahan compensated sum.
-- Reduces floating-point error from O(n·ε) to O(ε).
-- Reference: Kahan (1965), "Further remarks on reducing truncation errors".
kahanSum :: Vector Double -> Double
kahanSum = snd . V.foldl' step (0.0, 0.0)
  where
    step :: (Double, Double) -> Double -> (Double, Double)
    step (c, s) x =
      let y  = x - c
          t  = s + y
          c' = (t - s) - y
      in (c', t)

-- | Kahan mean — numerically stable for large n and mixed magnitudes.
kahanMean :: Vector Double -> Double
kahanMean v = kahanSum v / fromIntegral (V.length v)

-- ---------------------------------------------------------------------------
-- Welford's online variance
-- ---------------------------------------------------------------------------

-- | One-pass sample variance (Welford 1962, Bessel correction: divides by n-1).
-- Numerically stable for nearly-equal values (avoids catastrophic cancellation).
--
-- Precondition: @V.length v >= 2@ — callers MUST guard before calling.
welfordVariance :: Vector Double -> Double
welfordVariance v =
  let go (cnt, mean, m2) x =
        let cnt'   = cnt + 1.0
            delta  = x - mean
            mean'  = mean + delta / cnt'
            delta2 = x - mean'
        in (cnt', mean', m2 + delta * delta2)
      (finalCnt, _, finalM2) = V.foldl' go (0.0, 0.0, 0.0) v
  in if finalCnt < 2.0 then 0.0 else finalM2 / (finalCnt - 1.0)

-- ---------------------------------------------------------------------------
-- Central tendency
-- ---------------------------------------------------------------------------

-- | Scale-aware mean.  Uses Kahan summation.
computeMean :: MeasurementScale -> Vector Double -> SCEResult Double
computeMean scale values
  | V.null values           = Left $ mkError E2002
      "Cannot compute mean: dataset is empty"
      ["Provide at least one data point."] Error
  | not (canComputeMean scale) = Left $ mkError E2003
      "Mean is not defined for this measurement scale"
      ["Use Interval or Ratio scale data for mean computation."] Error
  | otherwise               = Right (kahanMean values)

-- | Scale-aware median.  Sorts a copy with IntroSort.
computeMedian :: MeasurementScale -> Vector Double -> SCEResult Double
computeMedian scale values
  | V.null values              = Left $ mkError E2002
      "Cannot compute median: dataset is empty"
      ["Provide at least one data point."] Error
  | not (canComputeMedian scale) = Left $ mkError E2003
      "Median is not defined for this measurement scale"
      ["Use Ordinal, Interval, or Ratio scale data."] Error
  | otherwise                  = Right (unsafeMedian values)

-- | Mode: smallest value among those with maximum frequency.
-- Total: returns smallest mode when all values are equally frequent.
computeMode :: Vector Double -> SCEResult Double
computeMode values
  | V.null values = Left $ mkError E2002
      "Cannot compute mode: dataset is empty"
      ["Provide at least one data point."] Error
  | otherwise     =
      let sorted    = sort (V.toList values)
          grouped   = group sorted
          -- Data.List.group always produces non-empty sublists; pattern is safe
          freqPairs = [(v, length g) | g@(v:_) <- grouped]
          maxFreq   = maximum (map snd freqPairs)
          modes     = [v | (v, f) <- freqPairs, f == maxFreq]
      in Right (minimum modes)   -- minimum of non-empty list: safe

-- ---------------------------------------------------------------------------
-- Dispersion
-- ---------------------------------------------------------------------------

-- | Sample standard deviation (Welford + Bessel).
computeStdDev :: MeasurementScale -> Vector Double -> SCEResult Double
computeStdDev scale values
  | V.null values          = Left $ mkError E2002
      "Cannot compute std dev: dataset is empty"
      ["Provide at least two data points."] Error
  | V.length values < 2   = Left $ mkError E2002
      ("Need at least 2 points for std dev, got "
        <> T.pack (show (V.length values)))
      ["Collect more data."] Error
  | not (canComputeMean scale) = Left $ mkError E2003
      "Std dev is not defined for this measurement scale"
      ["Use Interval or Ratio scale data."] Error
  | otherwise              = Right . sqrt $ welfordVariance values

-- | Sample variance (Welford + Bessel).
computeVariance :: MeasurementScale -> Vector Double -> SCEResult Double
computeVariance scale values
  | V.null values          = Left $ mkError E2002
      "Cannot compute variance: dataset is empty"
      ["Provide at least two data points."] Error
  | V.length values < 2   = Left $ mkError E2002
      ("Need at least 2 points for variance, got "
        <> T.pack (show (V.length values)))
      ["Collect more data."] Error
  | not (canComputeMean scale) = Left $ mkError E2003
      "Variance is not defined for this measurement scale"
      ["Use Interval or Ratio scale data."] Error
  | otherwise              = Right $ welfordVariance values

-- ---------------------------------------------------------------------------
-- Distribution shape  (Phase 2.2)
-- ---------------------------------------------------------------------------

-- | Bias-corrected sample skewness (Fisher-Pearson, matches R e1071 type=2).
-- Returns Right 0 for constant data.
computeSkewness :: Vector Double -> SCEResult Double
computeSkewness values
  | V.length values < 3 = Left $ mkError E2002
      ("Skewness requires >= 3 points, got "
        <> T.pack (show (V.length values)))
      ["Collect at least 3 observations."] Error
  | otherwise =
      let n    = fromIntegral (V.length values) :: Double
          mean = kahanMean values
          var  = welfordVariance values
      in if var < 1e-300
           then Right 0.0
           else
             let std  = sqrt var
                 -- Use Kahan for the sum of cubed z-scores
                 m3   = kahanSum $ V.map (\x -> ((x - mean) / std) ^ (3 :: Int)) values
                 g1   = m3 / n
                 -- Bias-correction factor: sqrt(n*(n-1)) / (n-2)
                 adj  = g1 * sqrt (n * (n - 1)) / (n - 2)
             in Right adj

-- | Bias-corrected excess kurtosis (Joanes & Gill 1998, type G2).
-- Normal distribution gives 0.  Returns Right (-3) for constant data.
computeKurtosis :: Vector Double -> SCEResult Double
computeKurtosis values
  | V.length values < 4 = Left $ mkError E2002
      ("Excess kurtosis requires >= 4 points, got "
        <> T.pack (show (V.length values)))
      ["Collect at least 4 observations."] Error
  | otherwise =
      let n    = fromIntegral (V.length values) :: Double
          mean = kahanMean values
          var  = welfordVariance values
      in if var < 1e-300
           then Right (-3.0)
           else
             let std  = sqrt var
                 m4   = kahanSum $ V.map (\x -> ((x - mean) / std) ^ (4 :: Int)) values
                 -- G2 formula
                 kurt = (n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * m4
                      - 3.0 * (n - 1) ^ (2 :: Int) / ((n - 2) * (n - 3))
             in Right kurt

-- | Median Absolute Deviation (unscaled).
-- MAD = median( |x_i - median(x)| )
-- Multiply by 1.4826 to obtain a normal-consistent scale estimate.
computeMAD :: Vector Double -> SCEResult Double
computeMAD values
  | V.null values = Left $ mkError E2002
      "Cannot compute MAD: dataset is empty"
      ["Provide at least one data point."] Error
  | otherwise     =
      let med        = unsafeMedian values
          deviations = V.map (\x -> abs (x - med)) values
      in Right (unsafeMedian deviations)

-- | Inter-Quartile Range: Q3 − Q1.
computeIQR :: Vector Double -> SCEResult Double
computeIQR values
  | V.null values = Left $ mkError E2002
      "Cannot compute IQR: dataset is empty"
      ["Provide at least one data point."] Error
  | otherwise = do
      q1 <- computePercentile 25.0 values
      q3 <- computePercentile 75.0 values
      Right (q3 - q1)

-- ---------------------------------------------------------------------------
-- Range helpers
-- ---------------------------------------------------------------------------

computeRange :: Vector Double -> SCEResult Double
computeRange values
  | V.null values = Left $ mkError E2002
      "Cannot compute range: dataset is empty"
      ["Provide at least one data point."] Error
  | otherwise     = Right $ V.maximum values - V.minimum values

computeMin :: Vector Double -> SCEResult Double
computeMin values
  | V.null values = Left $ mkError E2002
      "Cannot compute min: dataset is empty"
      ["Provide at least one data point."] Error
  | otherwise     = Right $ V.minimum values

computeMax :: Vector Double -> SCEResult Double
computeMax values
  | V.null values = Left $ mkError E2002
      "Cannot compute max: dataset is empty"
      ["Provide at least one data point."] Error
  | otherwise     = Right $ V.maximum values

-- | Type-7 linear interpolation percentile (matches R and NumPy defaults).
-- @p@ must be in [0, 100].
computePercentile :: Double -> Vector Double -> SCEResult Double
computePercentile p values
  | V.null values    = Left $ mkError E2002
      "Cannot compute percentile: dataset is empty"
      ["Provide at least one data point."] Error
  | p < 0 || p > 100 = Left $ mkError E2002
      ("Percentile must be in [0, 100], got " <> T.pack (show p))
      ["Use a value between 0 and 100 inclusive."] Error
  | V.length values == 1 = Right (V.unsafeHead values)
  | otherwise =
      let sorted = V.modify VA.sort values
          n      = V.length sorted
          pos    = (p / 100.0) * fromIntegral (n - 1)
          lo     = floor   pos :: Int
          hi     = ceiling pos :: Int
          frac   = pos - fromIntegral lo
      in if lo == hi
           then case sorted V.!? lo of
                  Just v  -> Right v
                  Nothing -> Left $ mkError E3001
                    ("Percentile index out of bounds: " <> T.pack (show lo))
                    [] Error
           else case (sorted V.!? lo, sorted V.!? hi) of
                  (Just vLo, Just vHi) ->
                    Right $ vLo * (1.0 - frac) + vHi * frac
                  _ -> Left $ mkError E3001
                    ("Percentile indices out of bounds: "
                      <> T.pack (show (lo, hi, n)))
                    [] Error

-- ---------------------------------------------------------------------------
-- Comprehensive descriptive statistics  (Phase 2.2)
-- ---------------------------------------------------------------------------

-- | Complete descriptive statistics for a numeric column.
-- Fields set to 'Nothing' when sample size is too small for that statistic.
data DescriptiveStats = DescriptiveStats
  { dsCount    :: Int
  , dsMean     :: Double
  , dsMedian   :: Double
  , dsMode     :: Maybe Double
  , dsStdDev   :: Maybe Double   -- Nothing when n < 2
  , dsVariance :: Maybe Double   -- Nothing when n < 2
  , dsSkewness :: Maybe Double   -- Nothing when n < 3
  , dsKurtosis :: Maybe Double   -- Nothing when n < 4
  , dsMAD      :: Double
  , dsIQR      :: Double
  , dsMin      :: Double
  , dsMax      :: Double
  , dsRange    :: Double
  , dsQ1       :: Double
  , dsQ3       :: Double
  } deriving stock (Show, Eq)

-- | Compute 'DescriptiveStats'.  Only fails on empty input.
computeDescriptiveStats :: Vector Double -> SCEResult DescriptiveStats
computeDescriptiveStats values
  | V.null values = Left $ mkError E2002
      "Cannot compute descriptive stats: dataset is empty"
      ["Provide at least one data point."] Error
  | otherwise =
      let n     = V.length values
          mean' = kahanMean values
          med'  = unsafeMedian values
          minV  = V.minimum values
          maxV  = V.maximum values

          modeV = case computeMode values of
                    Right m -> Just m
                    Left _  -> Nothing

          varV  = if n >= 2 then Just (welfordVariance values) else Nothing
          stdV  = fmap sqrt varV

          skewV = case computeSkewness values of
                    Right s -> Just s
                    Left _  -> Nothing

          kurtV = case computeKurtosis values of
                    Right k -> Just k
                    Left _  -> Nothing

          madV  = case computeMAD values of
                    Right m -> m
                    Left _  -> 0.0   -- can't fail: we guarded on empty above

          q1V   = case computePercentile 25.0 values of
                    Right q -> q
                    Left _  -> minV

          q3V   = case computePercentile 75.0 values of
                    Right q -> q
                    Left _  -> maxV

      in Right DescriptiveStats
           { dsCount    = n
           , dsMean     = mean'
           , dsMedian   = med'
           , dsMode     = modeV
           , dsStdDev   = stdV
           , dsVariance = varV
           , dsSkewness = skewV
           , dsKurtosis = kurtV
           , dsMAD      = madV
           , dsIQR      = q3V - q1V
           , dsMin      = minV
           , dsMax      = maxV
           , dsRange    = maxV - minV
           , dsQ1       = q1V
           , dsQ3       = q3V
           }

-- ---------------------------------------------------------------------------
-- Backward-compat SummaryStats
-- ---------------------------------------------------------------------------

-- | Retained for modules already importing 'SummaryStats'.
-- New code should prefer 'DescriptiveStats'.
data SummaryStats = SummaryStats
  { statsCount  :: Int
  , statsMean   :: Maybe Double
  , statsMedian :: Maybe Double
  , statsMode   :: Maybe Double
  , statsStdDev :: Maybe Double
  , statsMin    :: Double
  , statsMax    :: Double
  , statsRange  :: Double
  } deriving stock (Show, Eq)

computeSummaryStats :: MeasurementScale -> Vector Double -> SCEResult SummaryStats
computeSummaryStats scale values
  | V.null values = Left $ mkError E2002
      "Cannot compute summary stats: dataset is empty"
      ["Provide at least one data point."] Error
  | otherwise = do
      minV   <- computeMin   values
      maxV   <- computeMax   values
      rangeV <- computeRange values
      meanV  <- if canComputeMean scale
                  then Just <$> computeMean scale values
                  else Right Nothing
      medV   <- if canComputeMedian scale
                  then Just <$> computeMedian scale values
                  else Right Nothing
      modeV  <- case computeMode values of
                  Right m -> Right (Just m)
                  Left _  -> Right Nothing
      stdV   <- if canComputeMean scale && V.length values >= 2
                  then Just <$> computeStdDev scale values
                  else Right Nothing
      Right SummaryStats
        { statsCount  = V.length values
        , statsMean   = meanV
        , statsMedian = medV
        , statsMode   = modeV
        , statsStdDev = stdV
        , statsMin    = minV
        , statsMax    = maxV
        , statsRange  = rangeV
        }

-- ---------------------------------------------------------------------------
-- Time-series helpers
-- ---------------------------------------------------------------------------

computeDelta :: Vector Double -> Vector Double
computeDelta values
  | V.length values < 2 = V.empty
  | otherwise           =
      -- V.slice is total; bounds are safe because length >= 2
      let n    = V.length values
          rest = V.slice 1 (n - 1) values
          init' = V.slice 0 (n - 1) values
      in V.zipWith (-) rest init'

computePercentChange :: Vector Double -> Vector Double
computePercentChange values
  | V.length values < 2 = V.empty
  | otherwise           =
      let n     = V.length values
          curr' = V.slice 1 (n - 1) values
          prev' = V.slice 0 (n - 1) values
      in V.zipWith pct curr' prev'
  where
    pct curr prev
      | abs prev < 1e-10 =
          if abs curr < 1e-10 then 0.0
          else if curr > 0    then  1.0e308
                              else -1.0e308
      | otherwise = ((curr - prev) / prev) * 100.0

computeCumulative :: Vector Double -> Vector Double
computeCumulative = V.scanl1 (+)

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Non-partial median.  Precondition: v is non-empty.
unsafeMedian :: Vector Double -> Double
unsafeMedian v =
  let sorted = V.modify VA.sort v
      len    = V.length sorted
      mid    = len `div` 2
  in if odd len
       then sorted V.! mid
       else sorted V.! (mid - 1) * 0.5 + sorted V.! mid * 0.5

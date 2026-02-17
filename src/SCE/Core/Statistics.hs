{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module SCE.Core.Statistics
  ( -- * Basic Statistics
    computeMean
  , computeMedian
  , computeMode
  , computeStdDev
  , computeVariance
    -- * Distribution Statistics
  , computeRange
  , computeMin
  , computeMax
  , computePercentile
    -- * Summary Statistics
  , SummaryStats(..)
  , computeSummaryStats
    -- * Time Series
  , computeDelta
  , computePercentChange
  , computeCumulative
  ) where

import SCE.Core.Types
import SCE.Core.MeasurementScale

import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import Data.List (sort, group)

-- | Summary statistics for a dataset
data SummaryStats = SummaryStats
  { statsCount  :: Int
  , statsMean   :: Maybe Double
  , statsMedian :: Maybe Double
  , statsMode   :: Maybe Double
  , statsStdDev :: Maybe Double
  , statsMin    :: Double
  , statsMax    :: Double
  , statsRange  :: Double
  }
  deriving stock (Show, Eq)

------------------------------------------------------------
-- Basic Statistics
------------------------------------------------------------

computeMean :: MeasurementScale -> Vector Double -> SCEResult Double
computeMean scale values
  | V.null values = Left (mkError E2002 "Cannot compute mean: dataset is empty" ["Provide at least one data point."] Error)
  | not (canComputeMean scale) =
      Left (mkError E2003 "Mean is not valid for this measurement scale" ["Use Interval or Ratio scale data for mean computation."] Error)
  | otherwise =
      Right $ V.sum values / fromIntegral (V.length values)

computeMedian :: MeasurementScale -> Vector Double -> SCEResult Double
computeMedian scale values
  | V.null values = Left (mkError E2002 "Cannot compute median: dataset is empty" ["Provide at least one data point."] Error)
  | not (canComputeMedian scale) =
      Left (mkError E2003 "Median is not valid for this measurement scale" ["Use Ordinal, Interval, or Ratio scale data for median."] Error)
  | otherwise =
      Right $ median values
  where
    median v =
      let sorted = V.modify VA.sort v
          len    = V.length sorted
          mid    = len `div` 2
      in if odd len
           then sorted V.! mid
           -- Use numerically stable formula to avoid overflow
           else sorted V.! (mid - 1) * 0.5 + sorted V.! mid * 0.5

-- | Compute the mode(s) of a dataset
-- Returns the most frequent value, or the first value if all are equally frequent
-- This is a practical approach for summary statistics
computeMode :: Vector Double -> SCEResult Double
computeMode values
  | V.null values = Left (mkError E2002 "Cannot compute statistic: dataset is empty" ["Provide at least one data point."] Error)
  | otherwise =
      let sorted = sort (V.toList values)
          grouped = group sorted
          -- Count frequency of each value (safe: guard ensures non-empty groups)
          freqList = [(v, length g) | g@(v:_) <- grouped]
          maxFreq = maximum (map snd freqList)
          modes = [val | (val, freq) <- freqList, freq == maxFreq]
      in case modes of
           [] -> Left (mkError E2002 "Empty dataset: mode computation reached unexpected empty list" [] Error)
           [m] -> Right m  -- Single mode - unambiguous
           ms  -> if length ms == length grouped
                  then -- All values equally frequent - return the smallest
                       Right (minimum ms)
                  else -- Multiple modes but not all equal - return smallest mode
                       Right (minimum ms)


computeStdDev :: MeasurementScale -> Vector Double -> SCEResult Double
computeStdDev scale values
  | V.null values = Left (mkError E2002 "Cannot compute std dev: dataset is empty" ["Provide at least two data points."] Error)
  | V.length values < 2 = Left (mkError E2002 ("Cannot compute std dev: need at least 2 points, got " <> T.pack (show (V.length values))) ["Collect more data."] Error)
  | not (canComputeMean scale) =
      Left (mkError E2003 "Std dev is not valid for this measurement scale" ["Use Interval or Ratio scale data."] Error)
  | otherwise =
      -- Use sample standard deviation (Bessel's correction: n-1)
      -- This is the unbiased estimator for sample data
      Right $ sqrt $ sampleVariance values

computeVariance :: MeasurementScale -> Vector Double -> SCEResult Double
computeVariance scale values
  | V.null values = Left (mkError E2002 "Cannot compute variance: dataset is empty" ["Provide at least two data points."] Error)
  | V.length values < 2 = Left (mkError E2002 ("Cannot compute variance: need at least 2 points, got " <> T.pack (show (V.length values))) ["Collect more data."] Error)
  | not (canComputeMean scale) =
      Left (mkError E2003 "Variance is not valid for this measurement scale" ["Use Interval or Ratio scale data."] Error)
  | otherwise =
      -- Use sample variance (Bessel's correction: divide by n-1)
      -- This is the unbiased estimator for sample data, which is what
      -- is almost always needed in statistics for inference
      Right $ sampleVariance values

-- | Compute sample variance with Bessel's correction (divide by n-1).
-- Two-pass algorithm: first compute mean, then sum squared deviations.
-- Numerically stable for typical statistical workloads.
sampleVariance :: Vector Double -> Double
sampleVariance values =
  let n               = V.length values
      mean            = V.sum values / fromIntegral n
      sumSquaredDiffs = V.foldl' (\acc x -> acc + (x - mean) ^ (2 :: Int)) 0.0 values
  in sumSquaredDiffs / fromIntegral (n - 1)  -- Bessel's correction

------------------------------------------------------------
-- Distribution Statistics
------------------------------------------------------------

computeRange :: Vector Double -> SCEResult Double
computeRange values
  | V.null values = Left (mkError E2002 "Cannot compute statistic: dataset is empty" ["Provide at least one data point."] Error)
  | otherwise     = Right $ V.maximum values - V.minimum values

computeMin :: Vector Double -> SCEResult Double
computeMin values
  | V.null values = Left (mkError E2002 "Cannot compute statistic: dataset is empty" ["Provide at least one data point."] Error)
  | otherwise     = Right $ V.minimum values

computeMax :: Vector Double -> SCEResult Double
computeMax values
  | V.null values = Left (mkError E2002 "Cannot compute statistic: dataset is empty" ["Provide at least one data point."] Error)
  | otherwise     = Right $ V.maximum values

-- | Compute percentile using linear interpolation (Type 7 in R's quantile)
-- This is the most commonly used method in statistical software
computePercentile :: Double -> Vector Double -> SCEResult Double
computePercentile p values
  | V.null values = Left (mkError E2002 "Cannot compute statistic: dataset is empty" ["Provide at least one data point."] Error)
  | p < 0 || p > 100 = Left (mkError E2002 ("Percentile must be in [0,100], got " <> T.pack (show p)) ["Use a value between 0 and 100 inclusive."] Error)
  | V.length values == 1 = Right (V.head values)  -- Single value case
  | otherwise =
      let sorted = V.modify VA.sort values
          n = V.length sorted
          -- Linear interpolation: position in 0-indexed array
          pos = (p / 100.0) * fromIntegral (n - 1)
          lower = floor pos
          upper = ceiling pos
          fraction = pos - fromIntegral lower
      in if lower == upper
         then case sorted V.!? lower of
                Just val -> Right val
                Nothing -> Left (mkError E3001 ("Index out of bounds in percentile calculation: " <>
                          T.pack (show lower) <> " for length " <> T.pack (show n)) [] Critical)
         else case (sorted V.!? lower, sorted V.!? upper) of
                (Just lowerVal, Just upperVal) -> 
                  Right $ lowerVal * (1 - fraction) + upperVal * fraction
                _ -> Left (mkError E3001 ("Index out of bounds in percentile calculation: indices " <>
                     T.pack (show (lower, upper)) <> " for length " <> T.pack (show n)) [] Critical)

------------------------------------------------------------
-- Summary Statistics
------------------------------------------------------------

computeSummaryStats
  :: MeasurementScale
  -> Vector Double
  -> SCEResult SummaryStats
computeSummaryStats scale values
  | V.null values = Left (mkError E2002 "Cannot compute statistic: dataset is empty" ["Provide at least one data point."] Error)
  | otherwise = do
      minVal   <- computeMin values
      maxVal   <- computeMax values
      rangeVal <- computeRange values

      meanVal   <- if canComputeMean scale
                     then Just <$> computeMean scale values
                     else Right Nothing

      medianVal <- if canComputeMedian scale
                     then Just <$> computeMedian scale values
                     else Right Nothing

      -- Mode is optional - if it fails, we just report Nothing
      modeVal   <- case computeMode values of
                     Right m -> Right (Just m)
                     Left _  -> Right Nothing

      stdDevVal <- if canComputeMean scale && V.length values >= 2
                     then Just <$> computeStdDev scale values
                     else Right Nothing

      return SummaryStats
        { statsCount  = V.length values
        , statsMean   = meanVal
        , statsMedian = medianVal
        , statsMode   = modeVal
        , statsStdDev = stdDevVal
        , statsMin    = minVal
        , statsMax    = maxVal
        , statsRange  = rangeVal
        }

------------------------------------------------------------
-- Time Series
------------------------------------------------------------

computeDelta :: Vector Double -> Vector Double
computeDelta values
  | V.length values < 2 = V.empty  -- Need at least 2 values for delta
  | otherwise = V.zipWith (-) (V.tail values) (V.init values)

computePercentChange :: Vector Double -> Vector Double
computePercentChange values
  | V.null values = V.empty
  | V.length values < 2 = V.empty  -- Need at least 2 values
  | otherwise =
      V.zipWith calculateChange (V.tail values) (V.init values)
  where
    -- Safely calculate percent change, handling zero denominators
    calculateChange :: Double -> Double -> Double
    calculateChange curr prev
      | abs prev < 1e-10 = 
          -- Previous value is effectively zero
          if abs curr < 1e-10
          then 0.0  -- 0 to 0 is 0% change
          else if curr > 0
               then 1.0e308  -- Positive infinity replacement (large number)
               else -1.0e308 -- Negative infinity replacement
      | otherwise = ((curr - prev) / prev) * 100.0

computeCumulative :: Vector Double -> Vector Double
computeCumulative = V.scanl1 (+)



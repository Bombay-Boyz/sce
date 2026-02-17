{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.Histogram
Description : ASCII histogram generation with statistical validation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

This module implements histogram generation following statistical first principles.
Histograms are appropriate for:
- Continuous quantitative data (Interval or Ratio scale)
- Showing distribution shape, central tendency, and spread
- Identifying skewness, modality, and outliers

The module automatically determines optimal bin count using Sturges' rule,
Freedman-Diaconis rule, or Scott's rule depending on data characteristics.
-}
module SCE.Chart.Histogram
  ( -- * Histogram Generation
    generateHistogram
  , HistogramData(..)
  , BinningMethod(..)
  , Bin(..)
    -- * Bin Calculation
  , calculateBins
  , sturgesRule
  , freedmanDiaconisRule
  , scottsRule
    -- * Validation
  , validateHistogramData
  , isHistogramAppropriate
  ) where

import SCE.Core.Types
import SCE.Validation.LiquidTypes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import Text.Printf (printf)
import Data.List (group, sort)

-- | Methods for determining bin count
data BinningMethod
  = SturgesRule      -- ^ k = ceil(log2(n) + 1), good for normal distributions
  | FreedmanDiaconis -- ^ Uses IQR, robust to outliers
  | ScottsRule       -- ^ Uses standard deviation, assumes normality
  | Manual Int       -- ^ User-specified bin count
  deriving stock (Show, Eq)

-- | Represents a single histogram bin
data Bin = Bin
  { binLowerBound :: Double
  , binUpperBound :: Double
  , binCount      :: Int
  , binFrequency  :: Double  -- Relative frequency (0-1)
  }
  deriving stock (Show, Eq)

-- | Data required for histogram generation
data HistogramData = HistogramData
  { histValues       :: Vector Double
  , histTitle        :: Maybe Text
  , histXAxisLabel   :: Text
  , histConfig       :: ChartConfig
  , histScale        :: MeasurementScale
  , histBinningMethod :: BinningMethod
  }
  deriving stock (Show)

------------------------------------------------------------
-- Public API
------------------------------------------------------------

-- | Generate a histogram from continuous data
generateHistogram :: HistogramData -> SCEResult [Text]
generateHistogram histData = do
  -- Validate data
  _ <- validateHistogramData histData
  
  let values = histValues histData
  let n = V.length values
  
  -- Must have sufficient data
  when (n < 3) $
    Left (mkError E2002 ("Need at least " <> T.pack (show 3) <> " data points, got " <> T.pack (show n)) ["Collect more data before running this operation."] Error)
  
  -- Validate all values are finite
  _ <- V.mapM validateFinite values
  
  -- Calculate statistics
  let sorted = V.modify VA.sort values
  (minVal, maxVal) <- case (V.uncons sorted, V.unsnoc sorted) of
    (Just (minV, _), Just (_, maxV)) -> Right (minV, maxV)
    _ -> Left (mkError E2001 "Empty dataset" [] Error)
  let range = maxVal - minVal
  
  -- Check for sufficient spread
  when (range < 1e-10) $
    Left (mkError E2001 "All values are identical - cannot create meaningful histogram" [] Error)
  
  -- Determine bin count
  binCount <- case histBinningMethod histData of
    Manual k -> validatePos k
    SturgesRule -> return $ sturgesRule n
    FreedmanDiaconis -> return $ freedmanDiaconisRule sorted
    ScottsRule -> return $ scottsRule sorted
  
  -- Ensure reasonable bin count
  when (binCount < 3) $
    Left (mkError E2001 "Calculated bin count too small (< 3)" [] Error)
  
  when (binCount > 50) $
    Left (mkError E2001 "Calculated bin count too large (> 50)" [] Error)
  
  -- Create bins
  let bins = calculateBins sorted minVal maxVal binCount
  
  -- Validate bins
  _ <- validateNonEmpty bins
  
  -- Calculate statistics for display
  let mean = V.sum values / fromIntegral n
  let median = if n `mod` 2 == 0
                then (sorted V.! (n `div` 2 - 1) + sorted V.! (n `div` 2)) / 2
                else sorted V.! (n `div` 2)
  let variance = V.sum (V.map (\x -> (x - mean) ^ (2::Int)) values) / fromIntegral n
  let stdDev = sqrt variance
  
  -- Generate chart
  let header = generateHistogramHeader histData n mean median stdDev
  let chart = renderHistogram bins (histConfig histData)
  let xAxis = renderHistogramXAxis minVal maxVal (length bins)
  let stats = renderHistogramStats n mean median stdDev minVal maxVal binCount
  
  return $ header ++ chart ++ [xAxis, ""] ++ stats

-- | Validate that histogram is appropriate for the data
validateHistogramData :: HistogramData -> SCEResult ()
validateHistogramData histData = do
  -- Histograms require continuous quantitative data (Interval or Ratio scale)
  case histScale histData of
    Interval -> Right ()
    Ratio    -> Right ()
    scale    -> Left (mkError E2003
                  ("Histogram requires Interval or Ratio scale, got " <> T.pack (show scale))
                  ["For categorical data, use a bar chart instead."] Error)

-- | Check if histogram is the appropriate visualization
isHistogramAppropriate :: MeasurementScale -> Int -> [Double] -> Bool
isHistogramAppropriate scale n values
  | scale /= Interval && scale /= Ratio = False
  | n < 3 = False
  | n > 10000 = False  -- Too many points, consider density plot
  | otherwise = 
      let minVal = minimum values
          maxVal = maximum values
          range = maxVal - minVal
          uniqueCount = length $ group $ sort values
      in range > 1e-10 && uniqueCount >= 3

------------------------------------------------------------
-- Bin Calculation Methods
------------------------------------------------------------

-- | Calculate bins for the histogram
-- Uses half-open intervals [lower, upper) except for the last bin which is [lower, upper]
calculateBins :: Vector Double -> Double -> Double -> Int -> [Bin]
calculateBins values minVal maxVal binCount
  | binCount <= 0 = []  -- Safety check
  | otherwise =
      let binWidth = (maxVal - minVal) / fromIntegral binCount
          n = V.length values
          
          -- Use relative epsilon for numerical stability across different ranges
          relativeEpsilon = max (abs maxVal * 1e-10) 1e-10
          
          makeBin i =
            let lower = minVal + fromIntegral i * binWidth
                -- Last bin is closed interval to include maxVal
                -- Other bins are half-open: [lower, upper)
                upper = if i == binCount - 1
                        then maxVal + relativeEpsilon  -- Include max value
                        else minVal + fromIntegral (i + 1) * binWidth
                -- Use >= for lower bound, < for upper (except last bin)
                count = if i == binCount - 1
                        then V.length $ V.filter (\v -> v >= lower && v <= maxVal) values
                        else V.length $ V.filter (\v -> v >= lower && v < upper) values
                freq = fromIntegral count / fromIntegral n
            in Bin lower upper count freq
      
      in map makeBin [0 .. binCount - 1]

-- | Sturges' rule: k = ceil(log2(n) + 1)
-- Works well for normal distributions, simple and fast
sturgesRule :: Int -> Int
sturgesRule n = ceiling (logBase 2 (fromIntegral n) + 1)

-- | Freedman-Diaconis rule: bin width = 2 * IQR / n^(1/3)
-- Robust to outliers, uses interquartile range
-- Clamps result to [3, 50] range for practical visualization
freedmanDiaconisRule :: Vector Double -> Int
freedmanDiaconisRule sorted =
  let n = V.length sorted
      -- Proper quartile calculation with linear interpolation
      percentile p =
        let pos = p * fromIntegral (n - 1)
            lower = floor pos
            upper = ceiling pos
            fraction = pos - fromIntegral lower
        in if lower == upper
           then sorted V.! lower
           else sorted V.! lower * (1 - fraction) + 
                sorted V.! upper * fraction
      
      q1 = percentile 0.25
      q3 = percentile 0.75
      iqr = q3 - q1
      binWidth = 2 * iqr / (fromIntegral n ** (1/3))
      (minVal, maxVal) = case (V.uncons sorted, V.unsnoc sorted) of
        (Just (minV, _), Just (_, maxV)) -> (minV, maxV)
        _ -> (0, 0)  -- Should never happen as we validate n > 0
      range = maxVal - minVal
      k = if binWidth > 0
          then ceiling (range / binWidth)
          else sturgesRule n
      -- Clamp to practical range: minimum 3 bins for distribution shape,
      -- maximum 50 bins for readability
      -- Note: In production, consider making these limits configurable
      minBins = 3
      maxBins = 50
  in max minBins (min maxBins k)

-- | Scott's rule: bin width = 3.5 * σ / n^(1/3)
-- Assumes normality, uses standard deviation
-- Clamps result to [3, 50] range for practical visualization
scottsRule :: Vector Double -> Int
scottsRule sorted =
  let n = V.length sorted
      mean = V.sum sorted / fromIntegral n
      variance = V.sum (V.map (\x -> (x - mean) ^ (2::Int)) sorted) / fromIntegral n
      stdDev = sqrt variance
      binWidth = 3.5 * stdDev / (fromIntegral n ** (1/3))
      (minVal, maxVal) = case (V.uncons sorted, V.unsnoc sorted) of
        (Just (minV, _), Just (_, maxV)) -> (minV, maxV)
        _ -> (0, 0)  -- Should never happen
      range = maxVal - minVal
      k = if binWidth > 0
          then ceiling (range / binWidth)
          else sturgesRule n
      -- Clamp to practical range
      minBins = 3
      maxBins = 50
  in max minBins (min maxBins k)

------------------------------------------------------------
-- Rendering Functions
------------------------------------------------------------

renderHistogram :: [Bin] -> ChartConfig -> [Text]
renderHistogram bins config =
  let maxCount = maximum (map binCount bins)
      maxBarLength = chartMaxBarLength config
      
      renderBin bin =
        let count = binCount bin
            barLength = if maxCount > 0
                        then (count * maxBarLength) `div` maxCount
                        else 0
            bar = T.replicate barLength (T.singleton $ chartFillChar config)
            label = formatBinLabel (binLowerBound bin) (binUpperBound bin)
            countStr = T.pack $ printf "%4d" count
            freqStr = T.pack $ printf "(%.1f%%)" (binFrequency bin * 100)
        in label <> " │" <> bar <> " " <> countStr <> " " <> freqStr
  
  in map renderBin bins

formatBinLabel :: Double -> Double -> Text
formatBinLabel lower upper =
  let lowerStr = formatValue lower
      upperStr = formatValue upper
  in T.justifyLeft 15 ' ' $ "[" <> lowerStr <> " - " <> upperStr <> ")"

formatValue :: Double -> Text
formatValue v
  | abs v >= 1000000 = T.pack $ printf "%.1fM" (v / 1000000)
  | abs v >= 1000    = T.pack $ printf "%.1fK" (v / 1000)
  | abs v >= 100     = T.pack $ printf "%.0f" v
  | abs v >= 1       = T.pack $ printf "%.1f" v
  | otherwise        = T.pack $ printf "%.2f" v

renderHistogramXAxis :: Double -> Double -> Int -> Text
renderHistogramXAxis minVal maxVal binCount =
  "               └" <> T.replicate 50 "─" <> "┘" <>
  "  Range: " <> formatValue minVal <> " to " <> formatValue maxVal

generateHistogramHeader :: HistogramData -> Int -> Double -> Double -> Double -> [Text]
generateHistogramHeader histData n mean median stdDev =
  let title = case histTitle histData of
                Just t  -> t <> " (Histogram)"
                Nothing -> "Distribution of " <> histXAxisLabel histData
      binMethod = case histBinningMethod histData of
                    SturgesRule -> "Sturges' Rule"
                    FreedmanDiaconis -> "Freedman-Diaconis Rule"
                    ScottsRule -> "Scott's Rule"
                    Manual k -> "Manual (" <> T.pack (show k) <> " bins)"
  in [ T.replicate 60 "="
     , title
     , T.replicate 60 "="
     , ""
     , "Binning Method: " <> binMethod
     , ""
     ]

renderHistogramStats :: Int -> Double -> Double -> Double -> Double -> Double -> Int -> [Text]
renderHistogramStats n mean median stdDev minVal maxVal binCount =
  [ "Statistics:"
  , "  Sample size: " <> T.pack (show n)
  , "  Mean:        " <> T.pack (printf "%.2f" mean)
  , "  Median:      " <> T.pack (printf "%.2f" median)
  , "  Std Dev:     " <> T.pack (printf "%.2f" stdDev)
  , "  Range:       " <> T.pack (printf "%.2f - %.2f" minVal maxVal)
  , "  Bins:        " <> T.pack (show binCount)
  , ""
  , "Interpretation:"
  , "  - Each bar represents the frequency of values in that range"
  , "  - Taller bars indicate more common values"
  , "  - The shape reveals the distribution (normal, skewed, etc.)"
  , ""
  ]

-- Helper function (matches ScatterPlot module)
when :: Bool -> SCEResult () -> SCEResult ()
when True action = action
when False _     = Right ()

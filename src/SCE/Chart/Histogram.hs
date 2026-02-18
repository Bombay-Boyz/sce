{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.Histogram
Description : ASCII histogram generation with statistical validation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
-}
module SCE.Chart.Histogram
  ( generateHistogram
  , HistogramData(..)
  , BinningMethod(..)
  , Bin(..)
  , calculateBins
  , sturgesRule
  , freedmanDiaconisRule
  , scottsRule
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

data BinningMethod
  = SturgesRule
  | FreedmanDiaconis
  | ScottsRule
  | Manual Int
  deriving stock (Show, Eq)

data Bin = Bin
  { binLowerBound :: Double
  , binUpperBound :: Double
  , binCount      :: Int
  , binFrequency  :: Double
  } deriving stock (Show, Eq)

data HistogramData = HistogramData
  { histValues        :: Vector Double
  , histTitle         :: Maybe Text
  , histXAxisLabel    :: Text
  , histConfig        :: ChartConfig
  , histScale         :: MeasurementScale
  , histBinningMethod :: BinningMethod
  } deriving stock (Show)

generateHistogram :: HistogramData -> SCEResult [Text]
generateHistogram histData = do
  _ <- validateHistogramData histData

  let values = histValues histData
  let n      = V.length values

  when (n < 3) $
    Left (mkError E2002
      ("Need at least 3 data points, got " <> T.pack (show n))
      ["Collect more data before running this operation."] Error)

  _ <- V.mapM validateFinite values

  let sorted = V.modify VA.sort values

  -- Safe: n >= 3 guaranteed above, so sorted is non-empty.
  (minVal, maxVal) <- case (V.uncons sorted, V.unsnoc sorted) of
    (Just (minV, _), Just (_, maxV)) -> Right (minV, maxV)
    _ -> Left (mkError E2001 "Empty dataset after sorting" [] Error)

  let range = maxVal - minVal

  when (range < 1e-10) $
    Left (mkError E2001 "All values are identical - cannot create meaningful histogram" [] Error)

  binCount' <- case histBinningMethod histData of
    Manual k         -> validatePos k
    SturgesRule      -> return $ sturgesRule n
    FreedmanDiaconis -> return $ freedmanDiaconisRule sorted
    ScottsRule       -> return $ scottsRule sorted

  when (binCount' < 3) $
    Left (mkError E2001 "Calculated bin count too small (< 3)" [] Error)

  when (binCount' > 50) $
    Left (mkError E2001 "Calculated bin count too large (> 50)" [] Error)

  let bins = calculateBins sorted minVal maxVal binCount'

  _ <- validateNonEmpty bins

  let mean    = V.sum values / fromIntegral n
      median  = if n `mod` 2 == 0
                  then (sorted V.! (n `div` 2 - 1) + sorted V.! (n `div` 2)) / 2
                  else sorted V.! (n `div` 2)
      variance = V.sum (V.map (\x -> (x - mean) ^ (2::Int)) values) / fromIntegral n
      stdDev  = sqrt variance
      header  = generateHistogramHeader histData n mean median stdDev
      chart   = renderHistogram bins (histConfig histData)
      xAxis   = renderHistogramXAxis minVal maxVal (length bins)
      stats   = renderHistogramStats n mean median stdDev minVal maxVal binCount'

  return $ header ++ chart ++ [xAxis, ""] ++ stats

validateHistogramData :: HistogramData -> SCEResult ()
validateHistogramData histData =
  case histScale histData of
    Interval -> Right ()
    Ratio    -> Right ()
    scale    -> Left (mkError E2003
                  ("Histogram requires Interval or Ratio scale, got " <> T.pack (show scale))
                  ["For categorical data, use a bar chart instead."] Error)

-- | Check if histogram is appropriate. Pure predicate — no partial functions.
isHistogramAppropriate :: MeasurementScale -> Int -> [Double] -> Bool
isHistogramAppropriate scale n values
  | scale /= Interval && scale /= Ratio = False
  | n < 3    = False
  | n > 10000 = False
  | null values = False
  | otherwise =
      -- Safe: values is non-empty (null guard above)
      let minVal     = foldl1 min values
          maxVal     = foldl1 max values
          range      = maxVal - minVal
          uniqueCount = length $ group $ sort values
      in range > 1e-10 && uniqueCount >= 3

calculateBins :: Vector Double -> Double -> Double -> Int -> [Bin]
calculateBins values minVal maxVal binCount'
  | binCount' <= 0 = []
  | otherwise =
      let binWidth         = (maxVal - minVal) / fromIntegral binCount'
          n                = V.length values
          relativeEpsilon  = max (abs maxVal * 1e-10) 1e-10
          makeBin i =
            let lower = minVal + fromIntegral i * binWidth
                upper = if i == binCount' - 1
                          then maxVal + relativeEpsilon
                          else minVal + fromIntegral (i + 1) * binWidth
                count = if i == binCount' - 1
                          then V.length $ V.filter (\v -> v >= lower && v <= maxVal) values
                          else V.length $ V.filter (\v -> v >= lower && v < upper) values
                freq  = fromIntegral count / fromIntegral n
            in Bin lower upper count freq
      in map makeBin [0 .. binCount' - 1]

sturgesRule :: Int -> Int
sturgesRule n = ceiling (logBase 2 (fromIntegral n) + 1)

-- | Freedman-Diaconis rule. Clamps to [3, 50].
-- Precondition: sorted is non-empty (callers guarantee n >= 3).
freedmanDiaconisRule :: Vector Double -> Int
freedmanDiaconisRule sorted =
  let n = V.length sorted
      percentile p =
        let pos      = p * fromIntegral (n - 1)
            lo       = floor pos
            hi       = ceiling pos
            fraction = pos - fromIntegral lo
        in if lo == hi
             then sorted V.! lo
             else sorted V.! lo * (1 - fraction) + sorted V.! hi * fraction
      q1       = percentile 0.25
      q3       = percentile 0.75
      iqr      = q3 - q1
      -- Safe: n >= 3 guaranteed by caller; V.uncons/V.unsnoc cannot fail.
      (minVal, maxVal) = case (V.uncons sorted, V.unsnoc sorted) of
        (Just (mn, _), Just (_, mx)) -> (mn, mx)
        _                            -> (0, 0)  -- unreachable
      range    = maxVal - minVal
      binWidth = 2 * iqr / (fromIntegral n ** (1/3))
      k        = if binWidth > 0 then ceiling (range / binWidth) else sturgesRule n
  in max 3 (min 50 k)

-- | Scott's rule. Clamps to [3, 50].
-- Precondition: sorted is non-empty.
scottsRule :: Vector Double -> Int
scottsRule sorted =
  let n        = V.length sorted
      mean     = V.sum sorted / fromIntegral n
      variance = V.sum (V.map (\x -> (x - mean) ^ (2::Int)) sorted) / fromIntegral n
      stdDev   = sqrt variance
      binWidth = 3.5 * stdDev / (fromIntegral n ** (1/3))
      (minVal, maxVal) = case (V.uncons sorted, V.unsnoc sorted) of
        (Just (mn, _), Just (_, mx)) -> (mn, mx)
        _                            -> (0, 0)  -- unreachable
      range    = maxVal - minVal
      k        = if binWidth > 0 then ceiling (range / binWidth) else sturgesRule n
  in max 3 (min 50 k)

renderHistogram :: [Bin] -> ChartConfig -> [Text]
renderHistogram [] _      = []
renderHistogram bins config =
  -- Safe: bins is non-empty (guarded above).
  let maxCount    = foldl1 max (map binCount bins)
      maxBarLength = chartMaxBarLength config
      renderBin bin =
        let count     = binCount bin
            barLength = if maxCount > 0
                          then (count * maxBarLength) `div` maxCount
                          else 0
            bar       = T.replicate barLength (T.singleton $ chartFillChar config)
            label     = formatBinLabel (binLowerBound bin) (binUpperBound bin)
            countStr  = T.pack $ printf "%4d" count
            freqStr   = T.pack $ printf "(%.1f%%)" (binFrequency bin * 100)
        in label <> " \x2502" <> bar <> " " <> countStr <> " " <> freqStr
  in map renderBin bins

formatBinLabel :: Double -> Double -> Text
formatBinLabel lower upper =
  T.justifyLeft 15 ' ' $ "[" <> formatValue lower <> " - " <> formatValue upper <> ")"

formatValue :: Double -> Text
formatValue v
  | abs v >= 1000000 = T.pack $ printf "%.1fM" (v / 1000000)
  | abs v >= 1000    = T.pack $ printf "%.1fK" (v / 1000)
  | abs v >= 100     = T.pack $ printf "%.0f" v
  | abs v >= 1       = T.pack $ printf "%.1f" v
  | otherwise        = T.pack $ printf "%.2f" v

renderHistogramXAxis :: Double -> Double -> Int -> Text
renderHistogramXAxis minVal maxVal _binCount =
  "               \x2514" <> T.replicate 50 "\x2500" <> "\x2518"
  <> "  Range: " <> formatValue minVal <> " to " <> formatValue maxVal

generateHistogramHeader :: HistogramData -> Int -> Double -> Double -> Double -> [Text]
generateHistogramHeader histData _n _mean _median _stdDev =
  let title = case histTitle histData of
                Just t  -> t <> " (Histogram)"
                Nothing -> "Distribution of " <> histXAxisLabel histData
      binMethod = case histBinningMethod histData of
                    SturgesRule      -> "Sturges' Rule"
                    FreedmanDiaconis -> "Freedman-Diaconis Rule"
                    ScottsRule       -> "Scott's Rule"
                    Manual k         -> "Manual (" <> T.pack (show k) <> " bins)"
  in [T.replicate 60 "=", title, T.replicate 60 "=", "", "Binning Method: " <> binMethod, ""]

renderHistogramStats :: Int -> Double -> Double -> Double -> Double -> Double -> Int -> [Text]
renderHistogramStats n mean median stdDev minVal maxVal binCount' =
  [ "Statistics:"
  , "  Sample size: " <> T.pack (show n)
  , "  Mean:        " <> T.pack (printf "%.2f" mean)
  , "  Median:      " <> T.pack (printf "%.2f" median)
  , "  Std Dev:     " <> T.pack (printf "%.2f" stdDev)
  , "  Range:       " <> T.pack (printf "%.2f - %.2f" minVal maxVal)
  , "  Bins:        " <> T.pack (show binCount')
  , ""
  ]

when :: Bool -> SCEResult () -> SCEResult ()
when True  action = action
when False _      = Right ()

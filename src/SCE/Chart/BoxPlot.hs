{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.BoxPlot
Description : ASCII box plot generation with statistical validation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
-}
module SCE.Chart.BoxPlot
  ( generateBoxPlot
  , generateCategoricalBoxPlot
  , BoxPlotData(..)
  , CategoricalBoxPlotData(..)
  , BoxStats(..)
  , calculateBoxStats
  , identifyOutliers
  , validateBoxPlotData
  , isBoxPlotAppropriate
  ) where

import SCE.Core.Types
import SCE.Validation.LiquidTypes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import Text.Printf (printf)
import qualified Data.Map.Strict as M
import Data.List (sortBy, group, sort, foldl')
import Data.Ord (comparing)

data BoxStats = BoxStats
  { boxMin      :: Double
  , boxQ1       :: Double
  , boxMedian   :: Double
  , boxQ3       :: Double
  , boxMax      :: Double
  , boxIQR      :: Double
  , boxOutliers :: [Double]
  , boxMean     :: Double
  , boxN        :: Int
  } deriving stock (Show, Eq)

data BoxPlotData = BoxPlotData
  { boxValues    :: Vector Double
  , boxTitle     :: Maybe Text
  , boxAxisLabel :: Text
  , boxConfig    :: ChartConfig
  , boxScale     :: MeasurementScale
  } deriving stock (Show)

data CategoricalBoxPlotData = CategoricalBoxPlotData
  { catBoxData      :: M.Map Text (Vector Double)
  , catBoxTitle     :: Maybe Text
  , catBoxAxisLabel :: Text
  , catBoxConfig    :: ChartConfig
  , catBoxScale     :: MeasurementScale
  } deriving stock (Show)

generateBoxPlot :: BoxPlotData -> SCEResult [Text]
generateBoxPlot boxData = do
  _ <- validateBoxPlotData boxData

  let values = boxValues boxData
  let n      = V.length values

  when (n < 5) $
    Left (mkError E2002
      ("Need at least 5 data points, got " <> T.pack (show n))
      ["Collect more data before running this operation."] Error)

  _ <- mapM validateFinite (V.toList values)

  let stats       = calculateBoxStats values
      header      = generateBoxPlotHeader boxData stats
      chart       = renderBoxPlot stats (boxConfig boxData) (boxAxisLabel boxData)
      statsDisplay = renderBoxStats stats

  return $ header ++ chart ++ [""] ++ statsDisplay

generateCategoricalBoxPlot :: CategoricalBoxPlotData -> SCEResult [Text]
generateCategoricalBoxPlot catData = do
  when (M.null $ catBoxData catData) $
    Left (mkError E2001 "No categories provided for box plot" [] Error)

  let categories = M.toList $ catBoxData catData
  _ <- mapM (\(name, vals) ->
               when (V.length vals < 5) $
                 Left (mkError E2002
                   ("Category '" <> name <> "' has only "
                    <> T.pack (show (V.length vals))
                    <> " values (minimum 5 required)")
                   ["Collect at least 5 data points per category."] Error))
             categories

  let categoryStats = M.map calculateBoxStats (catBoxData catData)
      sortedCats    = sortBy (comparing (boxMedian . snd)) (M.toList categoryStats)
      header        = generateCatBoxPlotHeader catData
      chart         = renderCategoricalBoxPlot sortedCats (catBoxConfig catData)
      legend        = renderBoxPlotLegend

  return $ header ++ chart ++ [""] ++ legend

validateBoxPlotData :: BoxPlotData -> SCEResult ()
validateBoxPlotData boxData = do
  case boxScale boxData of
    Interval -> Right ()
    Ratio    -> Right ()
    scale    -> Left (mkError E2003
                  ("Box plot requires Interval or Ratio scale, got " <> T.pack (show scale))
                  ["For categorical data, use a bar chart instead."] Error)

  let values = boxValues boxData
      sorted = V.modify VA.sort values

  (minVal, maxVal) <- case (V.uncons sorted, V.unsnoc sorted) of
    (Just (minV, _), Just (_, maxV)) -> Right (minV, maxV)
    _ -> Left (mkError E2001 "Empty dataset" [] Error)

  let range     = maxVal - minVal
      uniqueVals = length $ group $ sort $ V.toList sorted

  when (range < 1e-10) $
    Left (mkError E5001
      ("All values are identical (range ~= 0, value=" <> T.pack (show minVal) <> ")")
      ["Report as a single value, not a distribution."] Error)

  when (uniqueVals < 5) $
    Left (mkError E5001
      ("Too few unique values: " <> T.pack (show uniqueVals) <> " distinct values found")
      ["Use a frequency table or bar chart for discrete data."] Error)

  let stats = calculateBoxStats values
  when (boxQ1 stats == boxMedian stats && boxMedian stats == boxQ3 stats) $
    Left (mkError E5001
      ("Quartiles have collapsed: Q1=Median=Q3=" <> T.pack (show (boxMedian stats)))
      ["Increase sample size or choose a different chart type."] Error)

-- | Pure predicate — no partial functions.
isBoxPlotAppropriate :: MeasurementScale -> Int -> [Double] -> Bool
isBoxPlotAppropriate scale n values
  | scale /= Interval && scale /= Ratio = False
  | n < 5     = False
  | n > 10000 = False
  | null values = False
  | otherwise =
      -- Safe: values is non-empty (null guard above).
      let minVal     = foldl' min (head' values) (tail' values)
          maxVal     = foldl' max (head' values) (tail' values)
          range      = maxVal - minVal
          uniqueCount = length $ group $ sort values
      in range > 1e-10 && uniqueCount >= 5
  where
    head' (x:_) = x
    head' []    = 0   -- unreachable: null guard above
    tail' (_:xs) = xs
    tail' []     = []

calculateBoxStats :: Vector Double -> BoxStats
calculateBoxStats values =
  let sorted = V.modify VA.sort values
      n      = V.length sorted

      percentile p =
        let pos      = p * fromIntegral (n - 1)
            lo       = floor pos
            hi       = ceiling pos
            fraction = pos - fromIntegral lo
        in if lo == hi
             then sorted V.! lo
             else (sorted V.! lo) * (1 - fraction) + (sorted V.! hi) * fraction

      q1     = percentile 0.25
      median = percentile 0.50
      q3     = percentile 0.75
      iqr    = q3 - q1
      mean   = V.sum values / fromIntegral n

      (outliers, whiskerMin, whiskerMax) =
        if iqr < 1e-10
          then
            -- Safe: calculateBoxStats is only called after n >= 5 check.
            let mn = V.foldl1' min sorted
                mx = V.foldl1' max sorted
            in ([], mn, mx)
          else
            let lowerFence  = q1 - 1.5 * iqr
                upperFence  = q3 + 1.5 * iqr
                outliersList = V.toList $ V.filter (\x -> x < lowerFence || x > upperFence) sorted
                nonOutliers  = V.filter (\x -> x >= lowerFence && x <= upperFence) sorted
                wMin = case V.uncons nonOutliers of
                         Just (mn, _) -> mn
                         Nothing      -> case V.uncons sorted of
                                           Just (mn, _) -> mn
                                           Nothing      -> q1
                wMax = case V.unsnoc nonOutliers of
                         Just (_, mx) -> mx
                         Nothing      -> case V.unsnoc sorted of
                                           Just (_, mx) -> mx
                                           Nothing      -> q3
            in (outliersList, wMin, wMax)

  in BoxStats
       { boxMin      = whiskerMin
       , boxQ1       = q1
       , boxMedian   = median
       , boxQ3       = q3
       , boxMax      = whiskerMax
       , boxIQR      = iqr
       , boxOutliers = outliers
       , boxMean     = mean
       , boxN        = n
       }

identifyOutliers :: Vector Double -> (Double, Double, [Double])
identifyOutliers values =
  let stats = calculateBoxStats values
  in (boxMin stats, boxMax stats, boxOutliers stats)

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

renderBoxPlot :: BoxStats -> ChartConfig -> Text -> [Text]
renderBoxPlot stats config _axisLabel =
  let width  = chartMaxBarLength config
      -- Five-element list; foldl1 min/max are safe.
      vals   = [boxMin stats, boxQ1 stats, boxMedian stats, boxQ3 stats, boxMax stats]
      minVal = foldl1 min vals
      maxVal = foldl1 max vals
      range  = maxVal - minVal

      scalePos :: Double -> Int
      scalePos val
        | range > 0 = round ((val - minVal) / range * fromIntegral (width - 1))
        | otherwise = width `div` 2

      minPos    = scalePos (boxMin stats)
      q1Pos     = scalePos (boxQ1 stats)
      medianPos = scalePos (boxMedian stats)
      q3Pos     = scalePos (boxQ3 stats)
      maxPos    = scalePos (boxMax stats)

      line = V.generate width $ \i ->
        case () of
          _ | i == minPos && i == maxPos -> '\x25cf'
            | i == minPos               -> '\x251c'
            | i == maxPos               -> '\x2524'
            | i == medianPos            -> '\x2588'
            | i >= q1Pos && i <= q3Pos  -> '\x2500'
            | (i > minPos && i < q1Pos) || (i > q3Pos && i < maxPos) -> '\x2500'
            | otherwise                 -> ' '

      outlierLines = map (\val ->
          let pos    = scalePos val
              marker = V.generate width (\i -> if i == pos then '\x25e6' else ' ')
          in "     " <> T.pack (V.toList marker)
             <> "  outlier: " <> T.pack (printf "%.2f" val)
        ) (boxOutliers stats)

  in ["     " <> T.pack (V.toList line), formatScale minVal maxVal width] ++ outlierLines
  where
    formatScale minV maxV w =
      let minStr = T.pack $ printf "%.2f" minV
          maxStr = T.pack $ printf "%.2f" maxV
          gap    = max 0 (w - T.length minStr - T.length maxStr)
      in "     " <> minStr <> T.replicate gap " " <> maxStr

renderCategoricalBoxPlot :: [(Text, BoxStats)] -> ChartConfig -> [Text]
renderCategoricalBoxPlot [] _      = []
renderCategoricalBoxPlot categories config =
  let width    = chartMaxBarLength config
      allStats = map snd categories

      -- Safe: categories is non-empty (empty guard above).
      -- Collect all values that contribute to the axis bounds.
      boundVals = concatMap (\s -> [boxMin s, boxQ1 s, boxMedian s, boxQ3 s, boxMax s]) allStats
      globalMin = foldl1 min boundVals
      globalMax = foldl1 max boundVals
      range     = globalMax - globalMin

      scalePos :: Double -> Int
      scalePos val
        | range > 0 = round ((val - globalMin) / range * fromIntegral (width - 1))
        | otherwise = width `div` 2

      renderCategory (name, stats) =
        let minPos    = scalePos (boxMin stats)
            q1Pos     = scalePos (boxQ1 stats)
            medianPos = scalePos (boxMedian stats)
            q3Pos     = scalePos (boxQ3 stats)
            maxPos    = scalePos (boxMax stats)
            line = V.generate width $ \i ->
              case () of
                _ | i == minPos && i == maxPos -> '\x25cf'
                  | i == minPos               -> '\x251c'
                  | i == maxPos               -> '\x2524'
                  | i == medianPos            -> '\x2588'
                  | i >= q1Pos && i <= q3Pos  -> '\x2500'
                  | (i > minPos && i < q1Pos) || (i > q3Pos && i < maxPos) -> '\x2500'
                  | otherwise                 -> ' '
            label = T.justifyLeft 15 ' ' name
        in label <> "\x2502" <> T.pack (V.toList line)
           <> "  n=" <> T.pack (show $ boxN stats)

      categoryLines = map renderCategory categories
      axisLine      = formatScale globalMin globalMax width

  in categoryLines ++ ["", axisLine]
  where
    formatScale minV maxV w =
      let minStr = T.pack $ printf "%.2f" minV
          maxStr = T.pack $ printf "%.2f" maxV
          gap    = max 0 (w - T.length minStr - T.length maxStr)
      in "               " <> minStr <> T.replicate gap " " <> maxStr

generateBoxPlotHeader :: BoxPlotData -> BoxStats -> [Text]
generateBoxPlotHeader boxData _stats =
  let title = case boxTitle boxData of
                Just t  -> t <> " (Box Plot)"
                Nothing -> "Distribution of " <> boxAxisLabel boxData
  in [T.replicate 60 "=", title, T.replicate 60 "=", "", "Five-Number Summary:", ""]

generateCatBoxPlotHeader :: CategoricalBoxPlotData -> [Text]
generateCatBoxPlotHeader catData =
  let title = case catBoxTitle catData of
                Just t  -> t <> " (Box Plots by Category)"
                Nothing -> "Distribution of " <> catBoxAxisLabel catData <> " by Category"
      nCats = M.size (catBoxData catData)
  in [ T.replicate 60 "=", title, T.replicate 60 "=", ""
     , "Comparing " <> T.pack (show nCats) <> " categories", ""
     ]

renderBoxStats :: BoxStats -> [Text]
renderBoxStats stats =
  [ "Statistics:"
  , "  Sample size (n):  " <> T.pack (show $ boxN stats)
  , "  Minimum:          " <> T.pack (printf "%.2f" $ boxMin stats)
  , "  Q1 (25th %ile):   " <> T.pack (printf "%.2f" $ boxQ1 stats)
  , "  Median (50th):    " <> T.pack (printf "%.2f" $ boxMedian stats)
  , "  Q3 (75th %ile):   " <> T.pack (printf "%.2f" $ boxQ3 stats)
  , "  Maximum:          " <> T.pack (printf "%.2f" $ boxMax stats)
  , "  IQR:              " <> T.pack (printf "%.2f" $ boxIQR stats)
  , "  Mean:             " <> T.pack (printf "%.2f" $ boxMean stats)
  , "  Outliers:         " <> T.pack (show $ length $ boxOutliers stats)
  , ""
  ]

renderBoxPlotLegend :: [Text]
renderBoxPlotLegend =
  [ "Legend:"
  , "  \x251c\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2588\x2500\x2500\x2500\x2500\x2500\x2500\x2500\x2524  Box and whiskers"
  , "  \x251c   Whisker min  |  \x2588  Median  |  \x2524  Whisker max"
  , "  \x25e6   Outlier (>1.5 x IQR from box)"
  , ""
  ]

when :: Bool -> SCEResult () -> SCEResult ()
when True  action = action
when False _      = Right ()

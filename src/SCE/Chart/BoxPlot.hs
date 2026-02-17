{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.BoxPlot
Description : ASCII box plot generation with statistical validation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

This module implements box plot (box-and-whisker plot) generation following 
statistical first principles. Box plots are appropriate for:
- Continuous quantitative data (Interval or Ratio scale)
- Showing distribution summary (five-number summary)
- Comparing distributions across categories
- Identifying outliers and skewness

Box plots display:
- Minimum (excluding outliers)
- Q1 (25th percentile)
- Median (50th percentile)
- Q3 (75th percentile)
- Maximum (excluding outliers)
- Outliers (values beyond 1.5 * IQR from quartiles)
-}
module SCE.Chart.BoxPlot
  ( -- * Box Plot Generation
    generateBoxPlot
  , generateCategoricalBoxPlot
  , BoxPlotData(..)
  , CategoricalBoxPlotData(..)
  , BoxStats(..)
    -- * Statistics Calculation
  , calculateBoxStats
  , identifyOutliers
    -- * Validation
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
import Data.List (sortBy, group, sort)
import Data.Ord (comparing)

-- | Five-number summary plus outlier information
data BoxStats = BoxStats
  { boxMin     :: Double      -- ^ Minimum (excluding outliers)
  , boxQ1      :: Double      -- ^ First quartile (25th percentile)
  , boxMedian  :: Double      -- ^ Median (50th percentile)
  , boxQ3      :: Double      -- ^ Third quartile (75th percentile)
  , boxMax     :: Double      -- ^ Maximum (excluding outliers)
  , boxIQR     :: Double      -- ^ Interquartile range (Q3 - Q1)
  , boxOutliers :: [Double]   -- ^ Outlier values
  , boxMean    :: Double      -- ^ Arithmetic mean (for reference)
  , boxN       :: Int         -- ^ Sample size
  }
  deriving stock (Show, Eq)

-- | Data required for single box plot
data BoxPlotData = BoxPlotData
  { boxValues       :: Vector Double
  , boxTitle        :: Maybe Text
  , boxAxisLabel    :: Text
  , boxConfig       :: ChartConfig
  , boxScale        :: MeasurementScale
  }
  deriving stock (Show)

-- | Data required for categorical box plots (multiple categories)
data CategoricalBoxPlotData = CategoricalBoxPlotData
  { catBoxData      :: M.Map Text (Vector Double)  -- ^ Category name -> values
  , catBoxTitle     :: Maybe Text
  , catBoxAxisLabel :: Text
  , catBoxConfig    :: ChartConfig
  , catBoxScale     :: MeasurementScale
  }
  deriving stock (Show)

------------------------------------------------------------
-- Public API
------------------------------------------------------------

-- | Generate a single box plot from continuous data
generateBoxPlot :: BoxPlotData -> SCEResult [Text]
generateBoxPlot boxData = do
  -- Validate data
  _ <- validateBoxPlotData boxData
  
  let values = boxValues boxData
  let n = V.length values
  
  -- Must have sufficient data (minimum 5 for five-number summary)
  when (n < 5) $
    Left (mkError E2002 ("Need at least " <> T.pack (show 5) <> " data points, got " <> T.pack (show n)) ["Collect more data before running this operation."] Error)
  
  -- Validate all values are finite
  _ <- mapM validateFinite (V.toList values)
  
  -- Calculate box plot statistics
  let stats = calculateBoxStats values
  
  -- Generate chart
  let header = generateBoxPlotHeader boxData stats
  let chart = renderBoxPlot stats (boxConfig boxData) (boxAxisLabel boxData)
  let statsDisplay = renderBoxStats stats
  
  return $ header ++ chart ++ [""] ++ statsDisplay

-- | Generate box plots for multiple categories
generateCategoricalBoxPlot :: CategoricalBoxPlotData -> SCEResult [Text]
generateCategoricalBoxPlot catData = do
  -- Validate that we have at least one category
  when (M.null $ catBoxData catData) $
    Left (mkError E2001 "No categories provided for box plot" [] Error)
  
  -- Validate each category has enough data
  let categories = M.toList $ catBoxData catData
  _ <- mapM (\(name, vals) -> 
              when (V.length vals < 5) $
                Left (mkError E2002 ("Category '" <> name <> "' has only " <> T.pack (show (V.length vals)) <> " values (minimum 5 required)") ["Collect at least 5 data points per category."] Error))
           categories
  
  -- Calculate statistics for each category
  let categoryStats = M.map calculateBoxStats (catBoxData catData)
  
  -- Sort categories by median for better readability
  let sortedCats = sortBy (comparing (boxMedian . snd)) 
                          (M.toList categoryStats)
  
  -- Generate chart
  let header = generateCatBoxPlotHeader catData
  let chart = renderCategoricalBoxPlot sortedCats (catBoxConfig catData)
  let legend = renderBoxPlotLegend
  
  return $ header ++ chart ++ [""] ++ legend

-- | Validate that box plot is appropriate for the data
validateBoxPlotData :: BoxPlotData -> SCEResult ()
validateBoxPlotData boxData = do
  -- Box plots require continuous quantitative data (Interval or Ratio scale)
  case boxScale boxData of
    Interval -> Right ()
    Ratio    -> Right ()
    scale    -> Left (mkError E2003 ("Box plot requires Interval or Ratio scale, got " <> T.pack (show scale)) ["For categorical data, use a bar chart instead."] Error)
  
  -- Check for sufficient variation and distinct values
  let values = boxValues boxData
  let sorted = V.modify VA.sort values
  (minVal, maxVal) <- case (V.uncons sorted, V.unsnoc sorted) of
    (Just (minV, _), Just (_, maxV)) -> Right (minV, maxV)
    _ -> Left (mkError E2001 "Empty dataset" [] Error)
  let range = maxVal - minVal
  let uniqueVals = length $ group $ sort $ V.toList sorted
  
  -- All values identical or nearly identical
  when (range < 1e-10) $
    Left (mkError E5001 ("All values are identical (range ≈ 0, value=" <> T.pack (show minVal) <> ")") ["Report as a single value, not a distribution."] Error)
  
  -- Too few unique values for meaningful box plot
  when (uniqueVals < 5) $
    Left (mkError E5001 ("Too few unique values: " <> T.pack (show uniqueVals) <> " distinct values found") ["Use a frequency table or bar chart for discrete data."] Error)
  
  -- Check for collapsed quartiles (additional safety check)
  let stats = calculateBoxStats values
  when (boxQ1 stats == boxMedian stats && boxMedian stats == boxQ3 stats) $
    Left (mkError E5001 ("Quartiles have collapsed: Q1=Median=Q3=" <> T.pack (show (boxMedian stats))) ["Increase sample size or choose a different chart type."] Error)

-- | Check if box plot is the appropriate visualization
isBoxPlotAppropriate :: MeasurementScale -> Int -> [Double] -> Bool
isBoxPlotAppropriate scale n values
  | scale /= Interval && scale /= Ratio = False
  | n < 5 = False  -- Need at least 5 points for five-number summary
  | n > 10000 = False  -- Too many points, consider other visualizations
  | otherwise = 
      let minVal = minimum values
          maxVal = maximum values
          range = maxVal - minVal
          uniqueCount = length $ group $ sort values
      in range > 1e-10 && uniqueCount >= 5  -- Need variation and sufficient unique values

------------------------------------------------------------
-- Statistics Calculation
------------------------------------------------------------

-- | Calculate five-number summary and outliers
calculateBoxStats :: Vector Double -> BoxStats
calculateBoxStats values =
  let sorted = V.modify VA.sort values
      n = V.length sorted
      
      -- Quartile calculation using linear interpolation
      percentile p =
        let pos = p * fromIntegral (n - 1)
            lower = floor pos
            upper = ceiling pos
            fraction = pos - fromIntegral lower
        in if lower == upper
           then sorted V.! lower
           else (sorted V.! lower) * (1 - fraction) + 
                (sorted V.! upper) * fraction
      
      q1 = percentile 0.25
      median = percentile 0.50
      q3 = percentile 0.75
      iqr = q3 - q1
      
      -- Mean for reference
      mean = V.sum values / fromIntegral n
      
      -- Handle collapsed IQR (low variation case)
      (outliers, whiskerMin, whiskerMax) = 
        if iqr < 1e-10  -- IQR essentially zero
        then 
          -- All values concentrated around quartiles
          -- Don't use outlier detection, use actual min/max
          ([], V.minimum sorted, V.maximum sorted)
        else
          -- Normal case: use Tukey's method for outliers
          let lowerFence = q1 - 1.5 * iqr
              upperFence = q3 + 1.5 * iqr
              
              -- Identify outliers
              outliersList = V.toList $ V.filter (\x -> x < lowerFence || x > upperFence) sorted
              
              -- Non-outlier min and max (whiskers)
              nonOutliers = V.filter (\x -> x >= lowerFence && x <= upperFence) sorted
              wMin = case (V.uncons nonOutliers, V.uncons sorted) of
                (Just (minNO, _), _) -> minNO
                (Nothing, Just (minS, _)) -> minS
                _ -> q1  -- Fallback
              wMax = case (V.unsnoc nonOutliers, V.unsnoc sorted) of
                (Just (_, maxNO), _) -> maxNO
                (Nothing, Just (_, maxS)) -> maxS
                _ -> q3  -- Fallback
          in (outliersList, wMin, wMax)
      
  in BoxStats
       { boxMin = whiskerMin
       , boxQ1 = q1
       , boxMedian = median
       , boxQ3 = q3
       , boxMax = whiskerMax
       , boxIQR = iqr
       , boxOutliers = outliers
       , boxMean = mean
       , boxN = n
       }

-- | Identify outliers using Tukey's method
identifyOutliers :: Vector Double -> (Double, Double, [Double])
identifyOutliers values =
  let stats = calculateBoxStats values
  in (boxMin stats, boxMax stats, boxOutliers stats)

------------------------------------------------------------
-- Rendering Functions
------------------------------------------------------------

renderBoxPlot :: BoxStats -> ChartConfig -> Text -> [Text]
renderBoxPlot stats config axisLabel =
  let width = chartMaxBarLength config
      
      -- Scale all values to fit in the available width
      minVal = minimum [boxMin stats, boxQ1 stats, boxMedian stats, 
                        boxQ3 stats, boxMax stats]
      maxVal = maximum [boxMin stats, boxQ1 stats, boxMedian stats, 
                        boxQ3 stats, boxMax stats]
      range = maxVal - minVal
      
      scale :: Double -> Int
      scale val = if range > 0
                  then round ((val - minVal) / range * fromIntegral (width - 1))
                  else width `div` 2
      
      minPos = scale (boxMin stats)
      q1Pos = scale (boxQ1 stats)
      medianPos = scale (boxMedian stats)
      q3Pos = scale (boxQ3 stats)
      maxPos = scale (boxMax stats)
      
      -- Build the box plot line
      line = V.generate width $ \i ->
        case () of
          _ | i == minPos && i == maxPos -> '●'  -- Single point
            | i == minPos -> '├'
            | i == maxPos -> '┤'
            | i == medianPos -> '█'
            | i >= q1Pos && i <= q3Pos -> '─'
            | (i > minPos && i < q1Pos) || (i > q3Pos && i < maxPos) -> '─'
            | otherwise -> ' '
      
      -- Add outliers if any
      outlierLines = map (\val -> 
          let pos = scale val
              marker = V.generate width $ \i -> if i == pos then '◦' else ' '
          in "     " <> T.pack (V.toList marker) <> "  outlier: " <> 
             T.pack (printf "%.2f" val)
        ) (boxOutliers stats)
      
  in [ "     " <> T.pack (V.toList line)
     , formatScale minVal maxVal width
     ] ++ outlierLines
  where
    formatScale minV maxV w =
      let minStr = T.pack $ printf "%.2f" minV
          maxStr = T.pack $ printf "%.2f" maxV
          gap = w - T.length minStr - T.length maxStr
      in "     " <> minStr <> T.replicate gap " " <> maxStr

renderCategoricalBoxPlot :: [(Text, BoxStats)] -> ChartConfig -> [Text]
renderCategoricalBoxPlot categories config =
  let width = chartMaxBarLength config
      
      -- Find global min and max across all categories
      allStats = map snd categories
      globalMin = minimum $ map boxMin allStats ++ 
                           map boxQ1 allStats ++ 
                           map boxMedian allStats
      globalMax = maximum $ map boxMax allStats ++ 
                           map boxQ3 allStats ++ 
                           map boxMedian allStats
      range = globalMax - globalMin
      
      scale :: Double -> Int
      scale val = if range > 0
                  then round ((val - globalMin) / range * fromIntegral (width - 1))
                  else width `div` 2
      
      -- Render each category
      renderCategory (name, stats) =
        let minPos = scale (boxMin stats)
            q1Pos = scale (boxQ1 stats)
            medianPos = scale (boxMedian stats)
            q3Pos = scale (boxQ3 stats)
            maxPos = scale (boxMax stats)
            
            line = V.generate width $ \i ->
              case () of
                _ | i == minPos && i == maxPos -> '●'
                  | i == minPos -> '├'
                  | i == maxPos -> '┤'
                  | i == medianPos -> '█'
                  | i >= q1Pos && i <= q3Pos -> '─'
                  | (i > minPos && i < q1Pos) || (i > q3Pos && i < maxPos) -> '─'
                  | otherwise -> ' '
            
            label = T.justifyLeft 15 ' ' name
        in label <> "│" <> T.pack (V.toList line) <> "  n=" <> T.pack (show $ boxN stats)
      
      categoryLines = map renderCategory categories
      axisLine = formatScale globalMin globalMax width
      
  in categoryLines ++ ["", axisLine]
  where
    formatScale minV maxV w =
      let minStr = T.pack $ printf "%.2f" minV
          maxStr = T.pack $ printf "%.2f" maxV
          gap = w - T.length minStr - T.length maxStr
      in "               " <> minStr <> T.replicate gap " " <> maxStr

generateBoxPlotHeader :: BoxPlotData -> BoxStats -> [Text]
generateBoxPlotHeader boxData stats =
  let title = case boxTitle boxData of
                Just t  -> t <> " (Box Plot)"
                Nothing -> "Distribution of " <> boxAxisLabel boxData
  in [ T.replicate 60 "="
     , title
     , T.replicate 60 "="
     , ""
     , "Five-Number Summary:"
     , ""
     ]

generateCatBoxPlotHeader :: CategoricalBoxPlotData -> [Text]
generateCatBoxPlotHeader catData =
  let title = case catBoxTitle catData of
                Just t  -> t <> " (Box Plots by Category)"
                Nothing -> "Distribution of " <> catBoxAxisLabel catData <> " by Category"
      nCats = M.size (catBoxData catData)
  in [ T.replicate 60 "="
     , title
     , T.replicate 60 "="
     , ""
     , "Comparing " <> T.pack (show nCats) <> " categories"
     , ""
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
  , "Interpretation:"
  , "  - The box shows Q1 to Q3 (middle 50% of data)"
  , "  - The █ marker shows the median"
  , "  - Whiskers (├ ┤) extend to min/max excluding outliers"
  , "  - ◦ markers indicate outliers (>1.5×IQR from quartiles)"
  , "  - Compare median to mean to assess skewness"
  , ""
  ]

renderBoxPlotLegend :: [Text]
renderBoxPlotLegend =
  [ "Legend:"
  , "  ├───────█───────┤  Box and whiskers"
  , "      ├   Q1 (25th percentile)"
  , "      █   Median"
  , "          ┤   Q3 (75th percentile)"
  , "  ─       Whiskers (extend to min/max, excluding outliers)"
  , "  ◦       Outlier (>1.5 × IQR from box)"
  , ""
  , "Reading the plot:"
  , "  - Longer box = more spread in middle 50%"
  , "  - Median left of center = right-skewed"
  , "  - Median right of center = left-skewed"
  , "  - Many outliers on one side = skewed distribution"
  , ""
  ]

-- Helper function (matches other chart modules)
when :: Bool -> SCEResult () -> SCEResult ()
when True action = action
when False _     = Right ()

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.Admissibility
Description : Chart admissibility rules and validation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Determines whether a chart type is appropriate for given data.
-}
module SCE.Chart.Admissibility
  ( -- * Admissibility Checks
    checkAdmissibility
  , checkDotPlotAdmissibility
  , isChartAdmissible
  , AdmissibilityResult(..)
  ) where

import SCE.Core.Types
import SCE.Chart.Rejection
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (group, sort)

-- | Result of admissibility check
data AdmissibilityResult
  = Admissible
  | NotAdmissible RejectionReason
  deriving stock (Show, Eq)

-- | Check if a chart type is admissible for the given data
checkAdmissibility :: ChartType -> MeasurementScale -> Int -> [Double] -> AdmissibilityResult
checkAdmissibility ct scale dataCount values =
  case ct of
    HorizontalBarChart -> checkBarChartAdmissibility scale dataCount values
    VerticalBarChart   -> checkBarChartAdmissibility scale dataCount values
    LineChart          -> checkLineChartAdmissibility scale dataCount
    ScatterPlot        -> checkScatterPlotAdmissibility scale dataCount
    DotPlot            -> checkDotPlotAdmissibility scale dataCount
    Histogram          -> checkHistogramAdmissibility scale dataCount values
    BoxPlot            -> checkBoxPlotAdmissibility scale dataCount values
    ConfidenceInterval -> checkConfidenceIntervalAdmissibility scale dataCount

-- | Check bar chart admissibility, including tightly-clustered data guard
checkBarChartAdmissibility :: MeasurementScale -> Int -> [Double] -> AdmissibilityResult
checkBarChartAdmissibility scale dataCount values
  | dataCount < 1  = NotAdmissible $ InsufficientDataRejection dataCount 1
  | dataCount > 50 = NotAdmissible $ TooManyCategoriesRejection dataCount 50
  | otherwise      = checkCluster values
  where
    -- Threshold: spread must be at least 15% of the maximum value.
    -- Below this the bars are visually almost identical and convey
    -- nothing beyond what the table already shows.
    clusterThreshold :: Double
    clusterThreshold = 0.15

    checkCluster [] = Admissible
    checkCluster (v:vs) =
      let minV      = foldl min v vs
          maxV      = foldl max v vs
          spreadRatio = if maxV > 0
                          then (maxV - minV) / maxV
                          else 0
      in if spreadRatio < clusterThreshold && maxV > 0
           then NotAdmissible $ TightlyClusteredRejection minV maxV spreadRatio
           else Admissible

-- | Check line chart admissibility
checkLineChartAdmissibility :: MeasurementScale -> Int -> AdmissibilityResult
checkLineChartAdmissibility scale dataCount
  | dataCount < 2 = NotAdmissible $ InsufficientDataRejection dataCount 2
  | scale == Nominal = NotAdmissible $
      InvalidScaleRejection scale LineChart
  | otherwise = Admissible

-- | Check scatter plot admissibility
-- | Check scatter plot admissibility
-- Scatter plots require:
-- - At least 3 points (preferably 10+ for meaningful patterns)
-- - Both variables at Interval or Ratio scale
-- - No more than 500 points (readability limit for ASCII)
checkScatterPlotAdmissibility :: MeasurementScale -> Int -> AdmissibilityResult
checkScatterPlotAdmissibility scale dataCount
  | dataCount < 3 = NotAdmissible $ InsufficientDataRejection dataCount 3
  | dataCount > 500 = NotAdmissible $ TooManyCategoriesRejection dataCount 500
  | scale == Nominal = NotAdmissible $
      InvalidScaleRejection scale ScatterPlot
  | scale == Ordinal = NotAdmissible $
      InvalidScaleRejection scale ScatterPlot
  | otherwise = Admissible

-- | Check dot plot admissibility.
--   Dot plots work for any scale and any count >= 2.
--   They are specifically well-suited for tightly-clustered data
--   because they encode value as position, not bar length.
checkDotPlotAdmissibility :: MeasurementScale -> Int -> AdmissibilityResult
checkDotPlotAdmissibility _scale dataCount
  | dataCount < 2 = NotAdmissible $ InsufficientDataRejection dataCount 2
  | dataCount > 50 = NotAdmissible $ TooManyCategoriesRejection dataCount 50
  | otherwise = Admissible

-- | Check histogram admissibility.
--   Histograms are for continuous quantitative data (Interval or Ratio scale)
--   They show the distribution of a single continuous variable.
--   Requirements:
--   - At least 10 observations (preferably 30+ for meaningful distribution)
--   - Interval or Ratio scale (continuous data)
--   - Sufficient unique values (at least 3)
--   - Reasonable data size (not > 10000 for ASCII rendering)
checkHistogramAdmissibility :: MeasurementScale -> Int -> [Double] -> AdmissibilityResult
checkHistogramAdmissibility scale dataCount values
  | dataCount < 10 = NotAdmissible $ InsufficientDataRejection dataCount 10
  | dataCount > 10000 = NotAdmissible $ TooManyCategoriesRejection dataCount 10000
  | scale == Nominal = NotAdmissible $
      InvalidScaleRejection scale Histogram
  | scale == Ordinal = NotAdmissible $
      InvalidScaleRejection scale Histogram
  | otherwise = checkVariability values
  where
    checkVariability [] = NotAdmissible $
      DataQualityRejection "Empty dataset"
    checkVariability (v:vs) =
      let minV = foldl min v vs
          maxV = foldl max v vs
          range = maxV - minV
          uniqueVals = length $ group $ sort vs
      in if range < 1e-10
         then NotAdmissible $ 
                DataQualityRejection "All values are identical - cannot show distribution"
         else if uniqueVals < 3
              then NotAdmissible $
                     DataQualityRejection $
                       "Too few unique values (" <> T.pack (show uniqueVals) <>
                       "). Histograms require continuous data with variation. " <>
                       "Consider using a bar chart for discrete categories."
              else Admissible

-- | Check box plot admissibility.
--   Box plots are for continuous quantitative data (Interval or Ratio scale)
--   They show the five-number summary and outliers.
--   Requirements:
--   - At least 5 observations (need min, Q1, median, Q3, max)
--   - Interval or Ratio scale (continuous data)
--   - Some variation in the data
--   - At least 5 unique values (for meaningful quartiles)
--   - Reasonable data size (not > 10000 for ASCII rendering)
checkBoxPlotAdmissibility :: MeasurementScale -> Int -> [Double] -> AdmissibilityResult
checkBoxPlotAdmissibility scale dataCount values
  | dataCount < 5 = NotAdmissible $ InsufficientDataRejection dataCount 5
  | dataCount > 10000 = NotAdmissible $ TooManyCategoriesRejection dataCount 10000
  | scale == Nominal = NotAdmissible $
      InvalidScaleRejection scale BoxPlot
  | scale == Ordinal = NotAdmissible $
      InvalidScaleRejection scale BoxPlot
  | otherwise = checkVariability values
  where
    checkVariability [] = NotAdmissible $
      DataQualityRejection "Empty dataset"
    checkVariability (v:vs) =
      let minV = foldl min v vs
          maxV = foldl max v vs
          range = maxV - minV
          uniqueVals = length $ group $ sort vs
      in if range < 1e-10
         then NotAdmissible $ 
                DataQualityRejection $
                  "All values are identical or nearly identical (range ≈ 0). " <>
                  "Box plots require variation in data to show distribution."
         else if uniqueVals < 5
              then NotAdmissible $
                     DataQualityRejection $
                       "Too few unique values (" <> T.pack (show uniqueVals) <>
                       "). Box plots require continuous data with meaningful variation. " <>
                       "With only " <> T.pack (show uniqueVals) <> " distinct values, " <>
                       "the five-number summary is not meaningful. " <>
                       "Consider using a frequency table or bar chart instead."
              else Admissible

-- | Check confidence interval chart admissibility.
--   Confidence interval charts are for time-series data with uncertainty estimates.
--   Requirements:
--   - At least 2 time points
--   - Interval or Ratio scale (continuous data)
--   - Must have confidence bounds (lower, upper)
checkConfidenceIntervalAdmissibility :: MeasurementScale -> Int -> AdmissibilityResult
checkConfidenceIntervalAdmissibility scale dataCount
  | dataCount < 2 = NotAdmissible $ InsufficientDataRejection dataCount 2
  | dataCount > 100 = NotAdmissible $ TooManyCategoriesRejection dataCount 100
  | scale == Nominal = NotAdmissible $
      InvalidScaleRejection scale ConfidenceInterval
  | scale == Ordinal = NotAdmissible $
      InvalidScaleRejection scale ConfidenceInterval
  | otherwise = Admissible

-- | Simple boolean check for admissibility
isChartAdmissible :: ChartType -> MeasurementScale -> Int -> [Double] -> Bool
isChartAdmissible ct scale dataCount values =
  case checkAdmissibility ct scale dataCount values of
    Admissible       -> True
    NotAdmissible _  -> False

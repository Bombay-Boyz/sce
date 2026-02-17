{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.Inference
Description : Multi-recommendation chart ranking system
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

This module implements an interactive ranking system that evaluates ALL chart types
and provides ranked recommendations with detailed scoring and rationale.
-}
module SCE.Chart.Inference
  ( -- * Multi-Recommendation System
    ChartRecommendations(..)
  , ChartRecommendation(..)
  , ScoreBreakdown(..)
  , ProsAndCons(..)
  , evaluateAllCharts
    -- * Legacy
  , inferBestChartType
    -- * Analysis
  , DataCharacteristics(..)
  , analyzeDataCharacteristics
  , detectDataStructure
  ) where

import SCE.Core.Types
import SCE.Core.DataModel
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.List (group, sort, sortBy)
import Data.Ord (Down(..), comparing)
import Text.Printf (printf)

-- | Multiple chart recommendations with ranking
data ChartRecommendations = ChartRecommendations
  { primaryRecommendation    :: ChartRecommendation
  , secondaryRecommendations :: [ChartRecommendation]
  , viableAlternatives       :: [ChartRecommendation]
  , allEvaluations           :: [ChartRecommendation]
  }
  deriving stock (Show, Eq)

-- | Enhanced recommendation with detailed scoring
data ChartRecommendation = ChartRecommendation
  { recChartType       :: ChartType
  , recConfidence      :: Double
  , recRationale       :: Text
  , recAlternatives    :: [ChartType]
  , recScoreBreakdown  :: ScoreBreakdown
  , recProsAndCons     :: ProsAndCons
  }
  deriving stock (Show, Eq)

data ScoreBreakdown = ScoreBreakdown
  { dataStructureScore    :: Double
  , measurementScaleScore :: Double
  , sampleSizeScore       :: Double
  , purposeFitScore       :: Double
  , clarityScore          :: Double
  , rigorScore            :: Double
  }
  deriving stock (Show, Eq)

data ProsAndCons = ProsAndCons
  { pros :: [Text]
  , cons :: [Text]
  }
  deriving stock (Show, Eq)

data ScoreWeights = ScoreWeights
  { dataStructureWeight :: Double
  , scaleWeight         :: Double
  , sampleSizeWeight    :: Double
  , purposeWeight       :: Double
  , clarityWeight       :: Double
  , rigorWeight         :: Double
  }

defaultWeights :: ScoreWeights
defaultWeights = ScoreWeights 0.30 0.25 0.15 0.15 0.10 0.05

data DataCharacteristics = DataCharacteristics
  { dcNumVariables     :: Int
  , dcSampleSize       :: Int
  , dcHasTimeOrdering  :: Bool
  , dcHasUncertainty   :: Bool
  , dcPrimaryScale     :: MeasurementScale
  , dcUniqueValues     :: Int
  , dcHasCategorical   :: Bool
  , dcIsTimeSeries     :: Bool
  , dcColumnNames      :: Vector Text
  }
  deriving stock (Show, Eq)

evaluateAllCharts :: DataTable -> ChartRecommendations
evaluateAllCharts table =
  let chars = analyzeDataCharacteristics table
      allRecs = [ evaluateConfidenceInterval chars
                , evaluateLineChart chars
                , evaluateScatterPlot chars
                , evaluateHistogram chars
                , evaluateBoxPlot chars
                , evaluateBarChart chars
                , evaluateDotPlot chars
                ]
      sorted = sortBy (comparing (Down . recConfidence)) allRecs
      -- Safe handling: use head if available, otherwise create default
      primary = case sorted of
        (p:_) -> p
        [] -> -- Logically impossible, but handle safely
          ChartRecommendation
            { recChartType = HorizontalBarChart
            , recConfidence = 0.0
            , recRationale = "No suitable chart type found for this data"
            , recAlternatives = []
            , recScoreBreakdown = ScoreBreakdown
                { dataStructureScore = 0.0
                , measurementScaleScore = 0.0
                , sampleSizeScore = 0.0
                , purposeFitScore = 0.0
                , clarityScore = 0.0
                , rigorScore = 0.0
                }
            , recProsAndCons = ProsAndCons [] []
            }
      secondary = filter (\r -> recConfidence r >= 0.75 && r /= primary) sorted
      viable = filter (\r -> recConfidence r >= 0.50 && recConfidence r < 0.75) sorted
  in ChartRecommendations primary secondary viable sorted

inferBestChartType :: DataTable -> ChartRecommendation
inferBestChartType table = primaryRecommendation $ evaluateAllCharts table

analyzeDataCharacteristics :: DataTable -> DataCharacteristics
analyzeDataCharacteristics table =
  let numVars = V.length (tableColumns table)
      sampleSize = V.length (tableRows table)
      colNames = tableColumns table
      scales = tableScales table
      isTimeSeries = detectTimeSeries colNames
      hasUncertainty = detectUncertaintyColumns colNames
      primaryScale = determinePrimaryScale scales
      firstDataCol = case V.find (\c -> not $ isTimeColumn c) colNames of
        Just col -> col
        Nothing -> if V.null colNames then "" else case V.uncons colNames of
          Just (col, _) -> col
          Nothing -> ""
      uniqueVals = if T.null firstDataCol then 0 
                   else countUniqueValues table firstDataCol
      hasCategorical = any (\s -> s == Nominal || s == Ordinal) (M.elems scales)
  in DataCharacteristics numVars sampleSize isTimeSeries hasUncertainty primaryScale 
                        uniqueVals hasCategorical isTimeSeries colNames

calculateConfidence :: ScoreBreakdown -> Double
calculateConfidence breakdown =
  let w = defaultWeights
  in dataStructureScore breakdown * dataStructureWeight w +
     measurementScaleScore breakdown * scaleWeight w +
     sampleSizeScore breakdown * sampleSizeWeight w +
     purposeFitScore breakdown * purposeWeight w +
     clarityScore breakdown * clarityWeight w +
     rigorScore breakdown * rigorWeight w

evaluateConfidenceInterval :: DataCharacteristics -> ChartRecommendation
evaluateConfidenceInterval chars =
  let breakdown = ScoreBreakdown
        { dataStructureScore = if dcIsTimeSeries chars && dcHasUncertainty chars then 1.0
                               else if dcIsTimeSeries chars then 0.3 else 0.1
        , measurementScaleScore = if dcPrimaryScale chars `elem` [Interval, Ratio] then 1.0 else 0.0
        , sampleSizeScore = let n = dcSampleSize chars
                            in if n >= 2 && n <= 100 then 1.0 - (abs (fromIntegral n - 20) / 100)
                               else if n < 2 then 0.0 else 0.5
        , purposeFitScore = if dcHasUncertainty chars then 1.0 else 0.3
        , clarityScore = 0.9
        , rigorScore = 1.0
        }
      prosAndCons = ProsAndCons
        { pros = ["Preserves temporal ordering", "Shows point estimates clearly",
                  "Displays uncertainty/confidence bounds", "Based on statistical theory",
                  "No visual manipulation"]
        , cons = ["Requires uncertainty data (Lower/Upper bounds)",
                  "Less familiar to general audiences", "Can be cluttered with many time points (>50)"]
        }
      rationale = if dataStructureScore breakdown >= 0.9
                  then "Data has time ordering with confidence bounds. Perfect match for forecasts or estimates with error bounds."
                  else if dataStructureScore breakdown >= 0.3
                       then "Data has time ordering but lacks uncertainty information."
                       else "Data is not time-ordered. Confidence interval charts require temporal sequence."
      alternatives = if dcIsTimeSeries chars then [LineChart, ScatterPlot] else [ScatterPlot, Histogram]
  in ChartRecommendation ConfidenceInterval (calculateConfidence breakdown) rationale alternatives breakdown prosAndCons

evaluateLineChart :: DataCharacteristics -> ChartRecommendation
evaluateLineChart chars =
  let breakdown = ScoreBreakdown
        { dataStructureScore = if dcIsTimeSeries chars then 1.0
                               else if dcHasTimeOrdering chars then 0.8 else 0.2
        , measurementScaleScore = if dcPrimaryScale chars `elem` [Interval, Ratio] then 1.0
                                  else if dcPrimaryScale chars == Ordinal then 0.6 else 0.0
        , sampleSizeScore = let n = dcSampleSize chars
                            in if n >= 3 && n <= 200 then 1.0 else if n < 3 then 0.0 else 0.7
        , purposeFitScore = if dcIsTimeSeries chars then 1.0 else 0.5
        , clarityScore = 0.95
        , rigorScore = 0.85
        }
      prosAndCons = ProsAndCons
        { pros = ["Shows trends over time clearly", "Preserves temporal sequence",
                  "Easy to interpret", "Good for showing patterns", "Familiar to most audiences"]
        , cons = ["Requires ordered data (time or sequence)", "Can be misleading if data is not continuous",
                  "Less effective for non-temporal data"]
        }
      rationale = if dcIsTimeSeries chars
                  then "Data has temporal ordering. Line charts excel at showing trends and patterns over time."
                  else "Data could be ordered sequentially. Line charts work if there's a natural progression."
  in ChartRecommendation LineChart (calculateConfidence breakdown) rationale [ScatterPlot, ConfidenceInterval] breakdown prosAndCons

evaluateScatterPlot :: DataCharacteristics -> ChartRecommendation
evaluateScatterPlot chars =
  let breakdown = ScoreBreakdown
        { dataStructureScore = if dcNumVariables chars >= 2 && not (dcHasCategorical chars) then 1.0 else 0.2
        , measurementScaleScore = if dcPrimaryScale chars `elem` [Interval, Ratio] then 1.0 else 0.0
        , sampleSizeScore = let n = dcSampleSize chars
                            in if n >= 3 && n <= 500 then 1.0 else if n > 500 then 0.6 else 0.0
        , purposeFitScore = if dcNumVariables chars >= 2 then 1.0 else 0.1
        , clarityScore = 0.90
        , rigorScore = 0.95
        }
      prosAndCons = ProsAndCons
        { pros = ["Shows relationships between variables", "Reveals correlations and patterns",
                  "Each point represents one observation", "Good for identifying outliers", "No arbitrary grouping"]
        , cons = ["Requires two quantitative variables", "Can be cluttered with too many points (>500)",
                  "Doesn't show distribution of individual variables"]
        }
      rationale = if dcNumVariables chars >= 2
                  then T.pack $ printf "Data has %d variables. Scatter plots reveal relationships between quantitative variables." (dcNumVariables chars)
                  else "Only one variable detected. Scatter plots require two variables."
  in ChartRecommendation ScatterPlot (calculateConfidence breakdown) rationale [LineChart] breakdown prosAndCons

evaluateHistogram :: DataCharacteristics -> ChartRecommendation
evaluateHistogram chars =
  let breakdown = ScoreBreakdown
        { dataStructureScore = if dcNumVariables chars == 1 && dcUniqueValues chars >= 5 then 1.0
                               else if dcNumVariables chars == 1 then 0.6 else 0.2
        , measurementScaleScore = if dcPrimaryScale chars `elem` [Interval, Ratio] then 1.0 else 0.0
        , sampleSizeScore = let n = dcSampleSize chars
                            in if n >= 10 && n <= 1000 then 1.0
                               else if n >= 5 && n < 10 then 0.7
                               else if n > 1000 then 0.8 else 0.0
        , purposeFitScore = 1.0
        , clarityScore = 0.85
        , rigorScore = 0.95
        }
      prosAndCons = ProsAndCons
        { pros = ["Shows distribution shape (normal, skewed, etc.)", "Reveals central tendency and spread",
                  "Identifies gaps and clusters", "Good for large datasets", "Statistical foundation (density estimation)"]
        , cons = ["Requires continuous data", "Bin width choice affects appearance",
                  "Not ideal for small samples (<10)", "Loses individual data points"]
        }
      rationale = T.pack $ printf "Single continuous variable with %d observations. Histograms show distribution shape and patterns." (dcSampleSize chars)
  in ChartRecommendation Histogram (calculateConfidence breakdown) rationale [BoxPlot, DotPlot] breakdown prosAndCons

evaluateBoxPlot :: DataCharacteristics -> ChartRecommendation
evaluateBoxPlot chars =
  let breakdown = ScoreBreakdown
        { dataStructureScore = if dcNumVariables chars == 1 && dcUniqueValues chars >= 5 then 0.95
                               else if dcNumVariables chars == 1 then 0.7 else 0.3
        , measurementScaleScore = if dcPrimaryScale chars `elem` [Interval, Ratio] then 1.0 else 0.0
        , sampleSizeScore = let n = dcSampleSize chars
                            in if n >= 5 && n <= 200 then 1.0 else if n < 5 then 0.0 else 0.7
        , purposeFitScore = 0.95
        , clarityScore = 0.75
        , rigorScore = 1.0
        }
      prosAndCons = ProsAndCons
        { pros = ["Shows five-number summary efficiently", "Identifies outliers using Tukey's method",
                  "Compact representation", "Good for comparing groups", "Based on robust statistics (quartiles)"]
        , cons = ["Less intuitive than histograms", "Hides distribution shape (bimodality, etc.)",
                  "Requires at least 5 observations", "Quartile calculation varies by method"]
        }
      rationale = T.pack $ printf "Single continuous variable with %d observations. Box plots provide statistical summary with outlier detection." (dcSampleSize chars)
  in ChartRecommendation BoxPlot (calculateConfidence breakdown) rationale [Histogram, DotPlot] breakdown prosAndCons

evaluateBarChart :: DataCharacteristics -> ChartRecommendation
evaluateBarChart chars =
  let breakdown = ScoreBreakdown
        { dataStructureScore = if dcHasCategorical chars && dcNumVariables chars >= 1 then 1.0 else 0.3
        , measurementScaleScore = if dcPrimaryScale chars `elem` [Interval, Ratio] || dcHasCategorical chars then 1.0 else 0.5
        , sampleSizeScore = let n = dcSampleSize chars
                            in if n >= 1 && n <= 50 then 1.0
                               else if n > 50 && n <= 100 then 0.7 else 0.3
        , purposeFitScore = if dcHasCategorical chars then 1.0 else 0.5
        , clarityScore = 1.0
        , rigorScore = 0.80
        }
      prosAndCons = ProsAndCons
        { pros = ["Universally understood", "Easy to compare categories", "Clear visual hierarchy",
                  "Works for all audiences", "Length proportional to value"]
        , cons = ["Limited to ~50 categories max", "Can be misleading if not starting at zero",
                  "Poor for tightly clustered data", "Takes up vertical space"]
        }
      rationale = if dcHasCategorical chars
                  then "Categorical data with quantitative values. Bar charts excel at comparing across categories."
                  else "Could work as categorical comparison. Consider if data has natural groupings."
  in ChartRecommendation HorizontalBarChart (calculateConfidence breakdown) rationale [DotPlot] breakdown prosAndCons

evaluateDotPlot :: DataCharacteristics -> ChartRecommendation
evaluateDotPlot chars =
  let breakdown = ScoreBreakdown
        { dataStructureScore = if dcUniqueValues chars >= 2 then 0.90 else 0.4
        , measurementScaleScore = if dcPrimaryScale chars `elem` [Interval, Ratio] then 1.0
                                  else if dcPrimaryScale chars == Ordinal then 0.7 else 0.3
        , sampleSizeScore = let n = dcSampleSize chars
                            in if n >= 2 && n <= 50 then 1.0
                               else if n > 50 && n <= 100 then 0.6 else 0.2
        , purposeFitScore = 0.85
        , clarityScore = 0.95
        , rigorScore = 0.90
        }
      prosAndCons = ProsAndCons
        { pros = ["Shows precise values clearly", "No distortion from bar length",
                  "Excellent for tightly clustered data", "Every value visible", "Clean, minimal design"]
        , cons = ["Less familiar than bar charts", "Limited to smaller datasets (<100)",
                  "Requires careful labeling", "May not show magnitude as clearly"]
        }
      rationale = "Dot plots show precise values without the visual weight of bars. Particularly good for comparing values or showing distributions."
  in ChartRecommendation DotPlot (calculateConfidence breakdown) rationale [HorizontalBarChart] breakdown prosAndCons

detectTimeSeries :: Vector Text -> Bool
detectTimeSeries = any isTimeColumn . V.toList

isTimeColumn :: Text -> Bool
isTimeColumn name =
  let lower = T.toLower name
  in any (`T.isInfixOf` lower) ["date", "time", "year", "month", "day", "period", "quarter"]

detectUncertaintyColumns :: Vector Text -> Bool
detectUncertaintyColumns colNames =
  let names = V.toList colNames
      hasLower = any hasLowerPattern names
      hasUpper = any hasUpperPattern names
  in hasLower && hasUpper
  where
    hasLowerPattern name = let lower = T.toLower name
                           in any (`T.isInfixOf` lower) ["lower", "min", "ci_lower", "lower_bound", "lower_ci", "error_min"]
    hasUpperPattern name = let lower = T.toLower name
                           in any (`T.isInfixOf` lower) ["upper", "max", "ci_upper", "upper_bound", "upper_ci", "error_max"]

determinePrimaryScale :: M.Map Text MeasurementScale -> MeasurementScale
determinePrimaryScale scales =
  if M.null scales then Ratio
  else mostRestrictive (M.elems scales)
  where
    mostRestrictive ss
      | Ratio `elem` ss = Ratio
      | Interval `elem` ss = Interval
      | Ordinal `elem` ss = Ordinal
      | otherwise = Nominal

countUniqueValues :: DataTable -> Text -> Int
countUniqueValues table colName =
  case getColumn colName table of
    Left _ -> 0
    Right col ->
      let vals = V.toList col
          textVals = map dataValueToText vals
          uniqueCount = length $ group $ sort textVals
      in uniqueCount
  where
    dataValueToText (TextValue t) = t
    dataValueToText (NumericValue d) = T.pack $ show d
    dataValueToText (IntegerValue i) = T.pack $ show i
    dataValueToText MissingValue = ""

detectDataStructure :: DataTable -> Text
detectDataStructure table =
  let chars = analyzeDataCharacteristics table
  in case () of
       _ | dcIsTimeSeries chars && dcHasUncertainty chars -> "Time-series with confidence intervals"
         | dcIsTimeSeries chars -> "Time-series data"
         | dcNumVariables chars >= 2 && dcPrimaryScale chars `elem` [Interval, Ratio] -> "Multivariate continuous"
         | dcHasCategorical chars -> "Categorical with quantitative values"
         | dcPrimaryScale chars `elem` [Interval, Ratio] -> "Single continuous variable"
         | otherwise -> "General tabular data"

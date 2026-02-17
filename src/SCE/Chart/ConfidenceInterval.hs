{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.ConfidenceInterval
Description : Confidence interval chart generation with statistical validation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

This module implements confidence interval charts following statistical first principles.
Confidence interval charts are appropriate for:
- Time-series data with uncertainty/error estimates
- Forecasts or predictions with confidence bounds
- Measurements with known standard errors
- A/B test results with statistical significance
- Economic estimates subject to revision

Confidence interval charts display:
- Point estimates (mean, median, or latest value)
- Lower confidence bound (e.g., 2.5th percentile for 95% CI)
- Upper confidence bound (e.g., 97.5th percentile for 95% CI)
- Time ordering (preserves temporal sequence)
- Optional: Interquartile range or narrower confidence band
-}
module SCE.Chart.ConfidenceInterval
  ( -- * Chart Generation
    generateConfidenceInterval
  , ConfidenceIntervalData(..)
  , ConfidencePoint(..)
    -- * Validation
    , validateConfidenceIntervalData
    , isConfidenceIntervalAppropriate
    -- * Data Detection
    , detectConfidenceIntervalColumns
  ) where

import SCE.Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf (printf)
import Data.Maybe (isJust, fromMaybe, catMaybes)

-- | Single data point with confidence interval
data ConfidencePoint = ConfidencePoint
  { cpLabel    :: Text      -- ^ Time period or category label
  , cpEstimate :: Double    -- ^ Point estimate (mean, median, or value)
  , cpLower    :: Double    -- ^ Lower confidence bound
  , cpUpper    :: Double    -- ^ Upper confidence bound
  , cpInnerLower :: Maybe Double  -- ^ Optional: inner bound (e.g., IQR or 50% CI)
  , cpInnerUpper :: Maybe Double  -- ^ Optional: inner bound
  }
  deriving stock (Show, Eq)

-- | Data required for confidence interval chart
data ConfidenceIntervalData = ConfidenceIntervalData
  { ciPoints         :: Vector ConfidencePoint
  , ciTitle          :: Maybe Text
  , ciYAxisLabel     :: Text
  , ciConfig         :: ChartConfig
  , ciConfidenceLevel :: Double  -- ^ e.g., 0.95 for 95% CI
  }
  deriving stock (Show)

------------------------------------------------------------
-- Public API
------------------------------------------------------------

-- | Generate a confidence interval chart
generateConfidenceInterval :: ConfidenceIntervalData -> SCEResult [Text]
generateConfidenceInterval ciData = do
  -- Validate data
  _ <- validateConfidenceIntervalData ciData
  
  let points = ciPoints ciData
  let n = V.length points
  
  -- Must have at least 2 points for meaningful time-series
  when (n < 2) $
    Left (mkError E2002 ("Need at least " <> T.pack (show 2) <> " data points, got " <> T.pack (show n)) ["Collect more data before running this operation."] Error)
  
  -- Validate each point
  _ <- V.mapM validatePoint points
  
  -- Generate chart
  let header = generateCIHeader ciData
  let chart = renderConfidenceInterval ciData
  let legend = renderCILegend ciData
  let interpretation = renderCIInterpretation ciData
  
  return $ header ++ chart ++ [""] ++ legend ++ [""] ++ interpretation

-- | Validate that confidence interval chart is appropriate
validateConfidenceIntervalData :: ConfidenceIntervalData -> SCEResult ()
validateConfidenceIntervalData ciData = do
  -- Confidence level must be reasonable
  let cl = ciConfidenceLevel ciData
  when (cl < 0.5 || cl > 0.999) $
    Left (mkError E2001 ("Confidence level must be in [50%, 99.9%], got " <> T.pack (show (cl * 100)) <> "%") ["Standard values are 90%, 95%, or 99%."] Error)
  
  -- Must have at least some points
  let n = V.length (ciPoints ciData)
  when (n < 2) $
    Left (mkError E2002 ("Need at least " <> T.pack (show 2) <> " data points, got " <> T.pack (show n)) ["Collect more data before running this operation."] Error)

-- | Validate a single confidence point
validatePoint :: ConfidencePoint -> SCEResult ConfidencePoint
validatePoint point = do
  -- Lower bound must be <= estimate
  when (cpLower point > cpEstimate point) $
    Left (mkError E2001 ("Lower bound " <> T.pack (show (cpLower point)) <> " > estimate " <> T.pack (show (cpEstimate point)) <> " for " <> cpLabel point) ["Ensure lower bound <= point estimate <= upper bound."] Error)
  
  -- Estimate must be <= upper bound
  when (cpEstimate point > cpUpper point) $
    Left (mkError E2001 ("Estimate " <> T.pack (show (cpEstimate point)) <> " > upper bound " <> T.pack (show (cpUpper point)) <> " for " <> cpLabel point) ["Ensure lower bound <= point estimate <= upper bound."] Error)
  
  -- If inner bounds exist, validate them
  case (cpInnerLower point, cpInnerUpper point) of
    (Just il, Just iu) -> do
      when (il < cpLower point || il > cpEstimate point) $
        Left (mkError E2001 "Inner lower bound must be between outer lower bound and estimate" [] Error)
      when (iu > cpUpper point || iu < cpEstimate point) $
        Left (mkError E2001 "Inner upper bound must be between estimate and outer upper bound" [] Error)
    _ -> Right ()
  
  return point

-- | Check if confidence interval chart is appropriate
isConfidenceIntervalAppropriate :: Int -> Bool -> SCEResult Bool
isConfidenceIntervalAppropriate n hasUncertainty
  | n < 2 = return False  -- Need at least 2 points for time-series
  | not hasUncertainty = return False  -- Need confidence bounds
  | otherwise = return True

-- | Detect if columns represent confidence interval data
-- Looks for patterns like: Label, Estimate, Lower, Upper
-- Or: Date, Value, CI_Lower, CI_Upper
detectConfidenceIntervalColumns :: Vector Text -> Maybe (Text, Text, Text, Text)
detectConfidenceIntervalColumns colNames =
  let names = V.toList colNames
      hasLabel = any (matchesPattern ["date", "time", "period", "year", "label"]) names
      hasEstimate = any (matchesPattern ["estimate", "value", "mean", "median", "point"]) names
      hasLower = any (matchesPattern ["lower", "min", "ci_lower", "lower_bound"]) names
      hasUpper = any (matchesPattern ["upper", "max", "ci_upper", "upper_bound"]) names
  in if hasLabel && hasEstimate && hasLower && hasUpper
     then Just ( findFirst ["date", "time", "period", "year", "label"] names
               , findFirst ["estimate", "value", "mean", "median", "point"] names
               , findFirst ["lower", "min", "ci_lower", "lower_bound"] names
               , findFirst ["upper", "max", "ci_upper", "upper_bound"] names
               )
     else Nothing
  where
    matchesPattern :: [Text] -> Text -> Bool
    matchesPattern patterns name =
      let lower = T.toLower name
      in any (\p -> p `T.isInfixOf` lower) patterns
    
    findFirst :: [Text] -> [Text] -> Text
    findFirst patterns names =
      case filter (\name -> matchesPattern patterns name) names of
        (n:_) -> n
        [] -> ""

------------------------------------------------------------
-- Rendering Functions
------------------------------------------------------------

renderConfidenceInterval :: ConfidenceIntervalData -> [Text]
renderConfidenceInterval ciData =
  let points = V.toList $ ciPoints ciData
      
      -- Find global min/max for scaling
      allVals = concatMap (\p -> [cpLower p, cpEstimate p, cpUpper p]) points
      globalMin = minimum allVals
      globalMax = maximum allVals
      range = globalMax - globalMin
      
      width = 60  -- Width for the interval bars
      
      -- Scale value to position
      scale :: Double -> Int
      scale val = if range > 0
                  then round ((val - globalMin) / range * fromIntegral (width - 1))
                  else width `div` 2
      
      -- Render each time point
      renderPoint p =
        let label = T.justifyLeft 12 ' ' (cpLabel p)
            estPos = scale (cpEstimate p)
            lowerPos = scale (cpLower p)
            upperPos = scale (cpUpper p)
            
            -- Build the visualization line
            line = V.generate width $ \i ->
              case () of
                _ | i == estPos -> '●'                    -- Point estimate
                  | i == lowerPos -> '├'                  -- Lower bound
                  | i == upperPos -> '┤'                  -- Upper bound
                  | i > lowerPos && i < upperPos ->       -- Interval range
                      if isJust (cpInnerLower p) && isJust (cpInnerUpper p)
                      then let ilPos = scale (fromMaybe 0 $ cpInnerLower p)
                               iuPos = scale (fromMaybe 0 $ cpInnerUpper p)
                           in if i >= ilPos && i <= iuPos
                              then '▓'  -- Inner confidence band
                              else '─'  -- Outer confidence band
                      else '─'
                  | otherwise -> ' '
            
            estVal = T.pack $ printf "%6.2f" (cpEstimate p)
        in label <> " │" <> T.pack (V.toList line) <> "│ " <> estVal
      
      pointLines = map renderPoint points
      
      -- Add scale axis
      axisLine = formatScaleAxis globalMin globalMax width
      
  in pointLines ++ ["", axisLine]

formatScaleAxis :: Double -> Double -> Int -> Text
formatScaleAxis minVal maxVal width =
  let minStr = T.pack $ printf "%.1f" minVal
      maxStr = T.pack $ printf "%.1f" maxVal
      midVal = (minVal + maxVal) / 2
      midStr = T.pack $ printf "%.1f" midVal
      
      -- Position labels
      midPos = width `div` 2
      gap1 = midPos - T.length minStr - 1
      gap2 = width - midPos - T.length midStr - T.length maxStr - 1
      
  in "             " <> minStr <> T.replicate gap1 " " <> 
     midStr <> T.replicate gap2 " " <> maxStr

generateCIHeader :: ConfidenceIntervalData -> [Text]
generateCIHeader ciData =
  let title = fromMaybe "Confidence Interval Chart" (ciTitle ciData)
      cl = ciConfidenceLevel ciData
      clPercent = T.pack $ printf "%.0f%%" (cl * 100)
  in [ T.replicate 70 "="
     , title
     , T.replicate 70 "="
     , ""
     , "Confidence Level: " <> clPercent
     , "Y-axis: " <> ciYAxisLabel ciData
     , ""
     ]

renderCILegend :: ConfidenceIntervalData -> [Text]
renderCILegend ciData =
  let cl = ciConfidenceLevel ciData
      clPercent = T.pack $ printf "%.0f%%" (cl * 100)
      hasInner = case V.uncons (ciPoints ciData) of
                   Just (p, _) -> isJust (cpInnerLower p)
                   Nothing -> False
  in [ "Legend:"
     , "  ●     Point estimate (latest value, mean, or median)"
     , "  ├─┤   " <> clPercent <> " confidence interval"
     , "  ─     Uncertainty range"
     ] ++ if hasInner
          then ["  ▓     50% confidence band (interquartile range)"]
          else []

renderCIInterpretation :: ConfidenceIntervalData -> [Text]
renderCIInterpretation ciData =
  let cl = ciConfidenceLevel ciData
      clPercent = T.pack $ printf "%.0f%%" (cl * 100)
      points = V.toList $ ciPoints ciData
      
      -- Calculate average interval width
      avgWidth = if null points 
                 then 0 
                 else sum (map (\p -> cpUpper p - cpLower p) points) / fromIntegral (length points)
      
      -- Check if uncertainty is increasing or decreasing
      (firstWidth, lastWidth) = case (points, reverse points) of
        (p1:_, p2:_) -> (cpUpper p1 - cpLower p1, cpUpper p2 - cpLower p2)
        _ -> (0, 0)
      
      trend = if lastWidth > firstWidth * 1.2
              then "Uncertainty is INCREASING over time (wider intervals)"
              else if lastWidth < firstWidth * 0.8
                   then "Uncertainty is DECREASING over time (narrower intervals)"
                   else "Uncertainty is STABLE over time (consistent intervals)"
      
  in [ "Interpretation:"
     , "  - Each point shows an estimate with " <> clPercent <> " confidence bounds"
     , "  - We are " <> clPercent <> " confident the true value lies within the interval"
     , "  - Average interval width: ±" <> T.pack (printf "%.2f" (avgWidth / 2))
     , "  - " <> trend
     , ""
     , "Reading the chart:"
     , "  - Narrower intervals = more certain estimates"
     , "  - Wider intervals = more uncertain estimates"
     , "  - Non-overlapping intervals = statistically significant difference"
     , "  - Overlapping intervals = difference may not be significant"
     , ""
     ]

-- Helper function
when :: Bool -> SCEResult () -> SCEResult ()
when True action = action
when False _     = Right ()

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.ScatterPlot
Description : ASCII scatter plot generation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
-}
module SCE.Chart.ScatterPlot
  ( generateScatterPlot
  , ScatterPlotData(..)
  , ScatterPoint(..)
  , CorrelationType(..)
  , computeCorrelation
  ) where

import SCE.Core.Types
import SCE.Validation.LiquidTypes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf (printf)

data ScatterPoint = ScatterPoint
  { scatterX     :: Double
  , scatterY     :: Double
  , scatterLabel :: Maybe Text
  }
  deriving stock (Show, Eq)

data CorrelationType
  = StrongPositive | ModeratePositive | WeakPositive
  | NoCorrelation
  | WeakNegative | ModerateNegative | StrongNegative
  deriving stock (Show, Eq)

data ScatterPlotData = ScatterPlotData
  { scatterPoints     :: Vector ScatterPoint
  , scatterXAxisLabel :: Text
  , scatterYAxisLabel :: Text
  , scatterTitle      :: Maybe Text
  , scatterConfig     :: ChartConfig
  , scatterXScale     :: MeasurementScale
  , scatterYScale     :: MeasurementScale
  }
  deriving stock (Show)

generateScatterPlot :: ScatterPlotData -> SCEResult [Text]
generateScatterPlot plotData = do
  let pointCount = V.length (scatterPoints plotData)
  _ <- validatePos pointCount
  
  when (pointCount < 3) $
    Left (mkError E2002 ("Need at least " <> T.pack (show 3) <> " data points, got " <> T.pack (show pointCount)) ["Collect more data before running this operation."] Error)
  
  when (pointCount > 500) $
    Left (mkError E2008 ("Too many categories: " <> T.pack (show pointCount) <> ", maximum is " <> T.pack (show 500)) ["Reduce the number of categories or use a different chart type."] Error)
  
  _ <- validateScaleForScatter (scatterXScale plotData) "X"
  _ <- validateScaleForScatter (scatterYScale plotData) "Y"
  
  let points = scatterPoints plotData
  let config = scatterConfig plotData
  let xValues = V.map scatterX points
  let yValues = V.map scatterY points
  
  _ <- V.mapM validateFinite xValues
  _ <- V.mapM validateFinite yValues
  
  let xMin = V.minimum xValues
  let xMax = V.maximum xValues
  let yMin = V.minimum yValues
  let yMax = V.maximum yValues
  
  when (xMax - xMin < 1e-10) $
    Left (mkError E2001 "All X values are identical - cannot create meaningful scatter plot" [] Error)
  
  when (yMax - yMin < 1e-10) $
    Left (mkError E2001 "All Y values are identical - cannot create meaningful scatter plot" [] Error)
  
  let plotWidth = chartMaxBarLength config
  let plotHeight = max 15 (min 30 (plotWidth `div` 2))
  let correlation = computeCorrelation (V.toList xValues) (V.toList yValues)
  let corrType = classifyCorrelation correlation
  
  let header = generateScatterHeader plotData
  let plot = renderScatterPlot points xMin xMax yMin yMax plotWidth plotHeight
  let xAxis = renderXAxis xMin xMax plotWidth
  let yAxisLabel = "  X-axis: " <> scatterXAxisLabel plotData
  let xAxisLabel = "  Y-axis: " <> scatterYAxisLabel plotData
  let stats = renderStats pointCount correlation corrType
  
  return $ header ++ plot ++ [xAxis, "", yAxisLabel, xAxisLabel] ++ stats

validateScaleForScatter :: MeasurementScale -> Text -> SCEResult ()
validateScaleForScatter scale axisName =
  case scale of
    Interval -> Right ()
    Ratio    -> Right ()
    _        -> Left (mkError E2003
                  (axisName <> "-axis has " <> T.pack (show scale) <>
                   " scale; scatter plots require Interval or Ratio scale for both axes.")
                  ["Ensure both axes use Interval or Ratio scale data."] Error)

renderScatterPlot :: Vector ScatterPoint -> Double -> Double -> Double -> Double -> Int -> Int -> [Text]
renderScatterPlot points xMin xMax yMin yMax width height =
  let xRange = xMax - xMin
      yRange = yMax - yMin
      emptyGrid = V.replicate height (V.replicate width ' ')
      gridWithPoints = V.foldl' (placePoint xMin yMin xRange yRange width height) emptyGrid points
      yStep = yRange / fromIntegral (height - 1)
  in V.toList $ V.imap (\row line -> renderGridLine row line yMax yStep) gridWithPoints

placePoint :: Double -> Double -> Double -> Double -> Int -> Int -> Vector (Vector Char) -> ScatterPoint -> Vector (Vector Char)
placePoint xMin yMin xRange yRange width height grid point =
  let x = scatterX point
      y = scatterY point
      xPos = floor $ (x - xMin) / xRange * fromIntegral (width - 1)
      yPos = height - 1 - floor ((y - yMin) / yRange * fromIntegral (height - 1))
      xPosClamped = max 0 (min (width - 1) xPos)
      yPosClamped = max 0 (min (height - 1) yPos)
  in if xPosClamped >= 0 && xPosClamped < width && yPosClamped >= 0 && yPosClamped < height
     then 
       let currentRow = grid V.! yPosClamped
           currentChar = currentRow V.! xPosClamped
           newChar = case currentChar of
                       ' ' -> '●'
                       '●' -> '◆'
                       _   -> '◆'
           updatedRow = currentRow V.// [(xPosClamped, newChar)]
       in grid V.// [(yPosClamped, updatedRow)]
     else grid

renderGridLine :: Int -> Vector Char -> Double -> Double -> Text
renderGridLine rowIdx line yMax yStep =
  let yValue = yMax - (fromIntegral rowIdx * yStep)
      yLabel = formatAxisValue yValue
      yLabelPadded = T.justifyRight 4 ' ' yLabel
      boundary = if rowIdx == 0 then "┤" else "│"
  in yLabelPadded <> boundary <> T.pack (V.toList line)

renderXAxis :: Double -> Double -> Int -> Text
renderXAxis xMin xMax width =
  let xRange = xMax - xMin
      numTicks = min 10 (width `div` 5)
      tickStep = xRange / fromIntegral numTicks
      baseline = "     └" <> T.replicate (width - 1) "─" <> "┐"
      tickPositions = [0..numTicks]
      tickLabels = map (\i -> 
        let xVal = xMin + fromIntegral i * tickStep
        in formatAxisValue xVal) tickPositions
      labelWidth = 6
      spacing = max 1 (width `div` numTicks)
      labelLine = "     " <> T.concat 
        [ T.justifyRight labelWidth ' ' label
        | (i, label) <- zip [0..] tickLabels
        , let pos = i * spacing
        , pos < width
        ]
  in T.unlines [baseline, labelLine]

formatAxisValue :: Double -> Text
formatAxisValue v
  | abs v >= 1000000 = T.pack $ printf "%.0fM" (v / 1000000)
  | abs v >= 1000    = T.pack $ printf "%.0fK" (v / 1000)
  | abs v >= 100     = T.pack $ printf "%.0f" v
  | abs v >= 1       = T.pack $ printf "%.1f" v
  | otherwise        = T.pack $ printf "%.2f" v

computeCorrelation :: [Double] -> [Double] -> Double
computeCorrelation xs ys
  | length xs /= length ys = 0
  | length xs < 2 = 0
  | otherwise =
      let n = fromIntegral (length xs)
          meanX = sum xs / n
          meanY = sum ys / n
          deviationsX = map (\x -> x - meanX) xs
          deviationsY = map (\y -> y - meanY) ys
          sumProducts = sum $ zipWith (*) deviationsX deviationsY
          sumSquaresX = sum $ map (^(2::Int)) deviationsX
          sumSquaresY = sum $ map (^(2::Int)) deviationsY
          denominator = sqrt (sumSquaresX * sumSquaresY)
      in if denominator < 1e-10 then 0 else sumProducts / denominator

classifyCorrelation :: Double -> CorrelationType
classifyCorrelation r
  | r > 0.7      = StrongPositive
  | r > 0.3      = ModeratePositive
  | r > 0        = WeakPositive
  | r > -0.3     = WeakNegative
  | r > -0.7     = ModerateNegative
  | r <= -0.7    = StrongNegative
  | otherwise    = NoCorrelation

describeCorrelation :: CorrelationType -> Text
describeCorrelation StrongPositive    = "strong positive"
describeCorrelation ModeratePositive  = "moderate positive"
describeCorrelation WeakPositive      = "weak positive"
describeCorrelation NoCorrelation     = "none"
describeCorrelation WeakNegative      = "weak negative"
describeCorrelation ModerateNegative  = "moderate negative"
describeCorrelation StrongNegative    = "strong negative"

generateScatterHeader :: ScatterPlotData -> [Text]
generateScatterHeader plotData =
  let title = case scatterTitle plotData of
                Just t  -> t <> " (Scatter Plot)"
                Nothing -> scatterXAxisLabel plotData <> " vs " <> scatterYAxisLabel plotData
  in [ T.replicate 60 "=", title, T.replicate 60 "=", "" ]

renderStats :: Int -> Double -> CorrelationType -> [Text]
renderStats pointCount correlation corrType =
  [ ""
  , "  Points: " <> T.pack (show pointCount) <> 
    "  |  Correlation: " <> formatCorrelation correlation <> 
    " (" <> describeCorrelation corrType <> ")"
  , ""
  ]
  where
    formatCorrelation r = T.pack $ printf "%.3f" r

when :: Bool -> SCEResult () -> SCEResult ()
when True action = action
when False _     = Right () 

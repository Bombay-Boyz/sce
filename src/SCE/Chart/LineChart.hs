{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.LineChart
Description : Line chart generation for time-series and ordered data
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Generates ASCII line charts that preserve temporal ordering and show trends.
Supports both single-series and multi-series line charts.
-}
module SCE.Chart.LineChart
  ( -- * Line Chart Generation
    generateLineChart
  , generateMultiSeriesLineChart
  , LineChartData(..)
  , LinePoint(..)
  , SeriesData(..)
  ) where

import SCE.Core.Types
import SCE.Validation.LiquidTypes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf (printf)
import Data.List (intercalate)

-- | Single point in a line chart
data LinePoint = LinePoint
  { pointLabel :: Text    -- ^ X-axis label (time/category)
  , pointValue :: Double  -- ^ Y-axis value
  }
  deriving stock (Show, Eq)

-- | Data for a single series
data SeriesData = SeriesData
  { seriesName   :: Text
  , seriesPoints :: Vector LinePoint
  , seriesSymbol :: Char  -- ^ Symbol to use for this series (●, ○, ■, etc.)
  }
  deriving stock (Show, Eq)

-- | Complete line chart data
data LineChartData = LineChartData
  { linePoints :: Vector LinePoint
  , lineConfig :: ChartConfig
  , lineScale  :: MeasurementScale
  , lineUnit   :: Maybe Text
  , lineTitle  :: Maybe Text
  , lineYLabel :: Maybe Text
  , lineXLabel :: Maybe Text
  }
  deriving stock (Show)

-- | Generate a single-series line chart
generateLineChart :: LineChartData -> SCEResult [Text]
generateLineChart chartData = do
  -- Validate point count (need at least 2 points for a line)
  let pointCount = V.length (linePoints chartData)
  if pointCount < 2
    then Left (mkError E2002 ("Need at least " <> T.pack (show 2) <> " data points, got " <> T.pack (show pointCount)) ["Collect more data before running this operation."] Error)
    else do
      let points = linePoints chartData
      let config = lineConfig chartData
      
      -- Calculate value range
      let values = V.map pointValue points
      let minValue = V.minimum values
      let maxValue = V.maximum values
      let range = maxValue - minValue
      
      -- Validate range
      if range <= 0
        then Left (mkError E2001 "All values are identical - cannot create line chart" [] Error)
        else do
          -- Generate the chart
          let header = generateLineHeader chartData
          let chart = renderLineChart points minValue maxValue config
          let footer = generateLineFooter chartData minValue maxValue
          
          return $ header ++ chart ++ footer

-- | Render the line chart visualization
renderLineChart :: Vector LinePoint -> Double -> Double -> ChartConfig -> [Text]
renderLineChart points minValue maxValue config =
  let height = 12  -- Fixed height for ASCII chart
      width = chartWidth config
      labelWidth = 12
      chartWidth' = width - labelWidth - 10  -- Reserve space for labels and axis
      
      -- Normalize values to chart coordinates (0 to height-1)
      range = maxValue - minValue
      normalizeY val = height - 1 - floor ((val - minValue) / range * fromIntegral (height - 1))
      
      -- Normalize x positions
      pointCount = V.length points
      normalizeX idx = floor (fromIntegral idx / fromIntegral (pointCount - 1) * fromIntegral (chartWidth' - 1))
      
      -- Create coordinate pairs
      coords = V.toList $ V.imap (\idx pt -> (normalizeX idx, normalizeY (pointValue pt))) points
      
      -- Build the grid
      grid = buildGrid height chartWidth' coords
      
      -- Add Y-axis labels and values
      valueLabels = generateValueLabels height minValue maxValue
      
  in zipWith (\row valLabel -> valLabel <> " │ " <> row) grid valueLabels

-- | Build ASCII grid with line
buildGrid :: Int -> Int -> [(Int, Int)] -> [Text]
buildGrid height width coords =
  let emptyRow = T.replicate width " "
      rows = replicate height emptyRow
      
      -- Plot points and connect them
      rowsWithPoints = case coords of
        [] -> rows
        [_] -> rows  -- Single point, no lines to draw
        _ -> foldl plotSegment rows (zip coords (drop 1 coords))
      
  in map (<> " │") rowsWithPoints
  where
    plotSegment rows' ((x1, y1), (x2, y2)) =
      let points = bresenhamLine x1 y1 x2 y2
          rows'' = foldl (\rs (x, y) -> 
                    if y >= 0 && y < height && x >= 0 && x < width
                    then updateRow rs y x
                    else rs
                  ) rows' points
      in rows''
    
    updateRow rows' y x =
      let (before, row:after) = splitAt y rows'
          newRow = replaceChar row x '●'
      in before ++ [newRow] ++ after
    
    replaceChar txt idx ch =
      let (before, after) = T.splitAt idx txt
          rest = T.drop 1 after  -- Skip the character at idx
      in before <> T.singleton ch <> rest

-- | Bresenham's line algorithm for connecting points
bresenhamLine :: Int -> Int -> Int -> Int -> [(Int, Int)]
bresenhamLine x0 y0 x1 y1 =
  let dx = abs (x1 - x0)
      dy = abs (y1 - y0)
      sx = if x0 < x1 then 1 else -1
      sy = if y0 < y1 then 1 else -1
      
      go x y err points
        | x == x1 && y == y1 = reverse ((x, y) : points)
        | otherwise =
            let e2 = 2 * err
                (x', err1) = if e2 > -dy then (x + sx, err - dy) else (x, err)
                (y', err2) = if e2 < dx then (y + sy, err1 + dx) else (y, err1)
            in go x' y' err2 ((x, y) : points)
  in go x0 y0 (dx - dy) []

-- | Generate value labels for Y-axis
generateValueLabels :: Int -> Double -> Double -> [Text]
generateValueLabels height minValue maxValue =
  let range = maxValue - minValue
      step = range / fromIntegral (height - 1)
      values = reverse [minValue + step * fromIntegral i | i <- [0..height-1]]
  in map formatValueLabel values

formatValueLabel :: Double -> Text
formatValueLabel val = T.pack $ printf "%10.2f" val

-- | Generate chart header
generateLineHeader :: LineChartData -> [Text]
generateLineHeader chartData =
  let unitSuffix = case lineUnit chartData of
                     Just unit -> " (" <> unit <> ")"
                     Nothing   -> ""
      title = case lineTitle chartData of
                Just t  -> t <> unitSuffix
                Nothing -> "Line Chart" <> unitSuffix
      yLabel = case lineYLabel chartData of
                 Just lbl -> "Y-axis: " <> lbl
                 Nothing  -> ""
  in [ T.replicate 70 "="
     , title
     , if T.null yLabel then "" else yLabel
     , T.replicate 70 "="
     , ""
     ]

-- | Generate chart footer with X-axis
generateLineFooter :: LineChartData -> Double -> Double -> [Text]
generateLineFooter chartData minValue maxValue =
  let points = linePoints chartData
      firstLabel = case V.uncons points of
                     Just (p, _) -> pointLabel p
                     Nothing -> "Start"
      lastLabel = if V.null points 
                  then "End"
                  else pointLabel (V.last points)
      xLabel = case lineXLabel chartData of
                 Just lbl -> lbl
                 Nothing  -> ""
      
      minValText = T.pack $ printf "%.2f" minValue
      maxValText = T.pack $ printf "%.2f" maxValue
      
  in [ ""
     , "             " <> firstLabel <> T.replicate 20 " " <> "..." <> T.replicate 20 " " <> lastLabel
     , if not (T.null xLabel) then "             " <> xLabel else ""
     , ""
     , "Value range: " <> minValText <> " to " <> maxValText
     ]

-- | Generate a multi-series line chart
generateMultiSeriesLineChart :: Vector SeriesData -> ChartConfig -> Maybe Text -> SCEResult [Text]
generateMultiSeriesLineChart series config titleMaybe = do
  -- Validate we have at least one series
  if V.null series
    then Left (mkError E2001 "No series data provided" [] Error)
    else do
      -- Validate all series have same number of points
      let pointCounts = V.map (V.length . seriesPoints) series
      case V.uncons pointCounts of
        Nothing -> Left (mkError E2001 "No series data provided" [] Error)
        Just (firstCount, restCounts) ->
          if not (V.all (== firstCount) restCounts)
          then Left (mkError E2001 "All series must have the same number of points" [] Error)
          else 
            let pointCount = firstCount
            in if pointCount < 2
               then Left (mkError E2002 ("Need at least " <> T.pack (show 2) <> " data points, got " <> T.pack (show pointCount)) ["Collect more data before running this operation."] Error)
               else do
                 -- Calculate global min/max across all series
                 let allValues = V.concatMap (V.map pointValue . seriesPoints) series
                 let minValue = V.minimum allValues
                 let maxValue = V.maximum allValues
                 
                 let header = generateMultiSeriesHeader series titleMaybe
                 let chart = renderMultiSeriesChart series minValue maxValue config
                 let legend = generateLegend series
                 
                 return $ header ++ chart ++ legend

generateMultiSeriesHeader :: Vector SeriesData -> Maybe Text -> [Text]
generateMultiSeriesHeader series titleMaybe =
  let title = case titleMaybe of
                Just t  -> t
                Nothing -> "Multi-Series Line Chart"
  in [ T.replicate 70 "="
     , title
     , T.replicate 70 "="
     , ""
     ]

renderMultiSeriesChart :: Vector SeriesData -> Double -> Double -> ChartConfig -> [Text]
renderMultiSeriesChart series minValue maxValue config =
  let height = 12
      width = chartWidth config
      labelWidth = 12
      chartWidth' = width - labelWidth - 10
      
      range = maxValue - minValue
      normalizeY val = height - 1 - floor ((val - minValue) / range * fromIntegral (height - 1))
      
      pointCount = case V.uncons series of
        Just (firstSeries, _) -> V.length (seriesPoints firstSeries)
        Nothing -> 0  -- Will be caught earlier
      normalizeX idx = floor (fromIntegral idx / fromIntegral (pointCount - 1) * fromIntegral (chartWidth' - 1))
      
      -- Build grid for each series
      emptyRow = T.replicate chartWidth' " "
      rows = replicate height emptyRow
      
      -- Plot all series
      finalRows = V.foldl' (\rs seriesData ->
        let coords = V.toList $ V.imap (\idx pt -> 
              (normalizeX idx, normalizeY (pointValue pt))) (seriesPoints seriesData)
            symbol = seriesSymbol seriesData
        in plotSeriesOnGrid rs coords symbol
        ) rows series
      
      valueLabels = generateValueLabels height minValue maxValue
      
  in zipWith (\row valLabel -> valLabel <> " │ " <> row <> " │") finalRows valueLabels
  where
    plotSeriesOnGrid rows' coords symbol =
      foldl (\rs (x, y) ->
        if y >= 0 && y < length rs && x >= 0 && x < chartWidth config - 22
        then updateRowWithSymbol rs y x symbol
        else rs
      ) rows' coords
    
    updateRowWithSymbol rows' y x symbol =
      let (before, row:after) = splitAt y rows'
          (rowBefore, rowAfter) = T.splitAt x row
          rowRest = T.drop 1 rowAfter  -- Skip the character at x
          newRow = rowBefore <> T.singleton symbol <> rowRest
      in before ++ [newRow] ++ after

generateLegend :: Vector SeriesData -> [Text]
generateLegend series =
  let legendItems = V.toList $ V.map (\s ->
        T.singleton (seriesSymbol s) <> " = " <> seriesName s
        ) series
  in [ ""
     , "Legend:"
     ] ++ map ("  " <>) legendItems

-- | Format a single value for display
formatValue :: Double -> Text
formatValue val = T.pack $ printf "%.2f" val

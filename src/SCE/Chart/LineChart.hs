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
  , seriesSymbol :: Char  -- ^ Symbol to use for this series (*, +, o, x, etc. -- ASCII only)
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
      
  in zipWith (\row valLabel -> valLabel <> " | " <> row) grid valueLabels

-- | Build ASCII grid.
-- Connects adjacent data points using '-' (horizontal), '|' (vertical),
-- '/' or '+' (diagonal). 'X' marks are stamped last so they always
-- sit on top of connecting lines.
-- All characters are single-byte ASCII so T.splitAt indexing is safe.
buildGrid :: Int -> Int -> [(Int, Int)] -> [Text]
buildGrid height width coords =
  let emptyRow     = T.replicate width " "
      rows         = replicate height emptyRow
      -- Step 1: draw connecting lines between consecutive points
      rowsWithLines = case coords of
        []      -> rows
        [_]     -> rows
        (c:cs)  -> foldl drawSegment rows (zip (c:cs) cs)
      -- Step 2: stamp 'X' on top at each actual data point
      rowsWithX    = foldl (\rs (x,y) -> setCell rs y x 'X') rowsWithLines coords
  in map (<> " |") rowsWithX
  where
    -- Draw a segment from (x1,y1) to (x2,y2) using directional ASCII chars
    drawSegment rows' ((x1,y1),(x2,y2)) =
      let pixels = segmentPixels x1 y1 x2 y2
      in foldl (\rs (x,y,ch) ->
           if y >= 0 && y < height && x >= 0 && x < width
           then setCell rs y x ch
           else rs
         ) rows' pixels

    -- Choose the right character for each pixel based on slope direction
    segmentPixels x1 y1 x2 y2 =
      let dx  = x2 - x1
          dy  = y2 - y1   -- positive = downward in grid (lower value)
          -- Pick connector character based on dominant direction
          ch  | dy == 0           = '-'   -- purely horizontal
              | dx == 0           = '|'   -- purely vertical
              | (dx > 0) == (dy > 0) = '+'  -- going down-right or up-left
              | otherwise         = '/'   -- going up-right or down-left
          -- Bresenham to get the pixel path
          pixels = bresenham x1 y1 x2 y2
          -- Tag each pixel with the appropriate character
          -- Endpoints will be overwritten by 'X' anyway
      in map (\(x,y) -> (x, y, dirChar x y x1 y1 x2 y2 dx dy)) pixels

    -- Per-pixel character: horizontal step -> '-', vertical step -> '|', diagonal -> '/' or '\'
    dirChar x y x1 y1 x2 y2 dx dy
      | dx == 0   = '|'
      | dy == 0   = '-'
      | abs dx >= abs dy =
          -- shallow slope — use '-' for most pixels, '/' or '+' at transitions
          let rising = (dx > 0) /= (dy > 0)
          in if rising then '/' else '+'
      | otherwise =
          let rising = (dx > 0) /= (dy > 0)
          in if rising then '/' else '+'

    -- Bresenham pixel enumeration (no character assignment)
    bresenham x0 y0 x1' y1' =
      let adx = abs (x1' - x0)
          ady = abs (y1' - y0)
          sx  = if x0 < x1' then 1 else -1
          sy  = if y0 < y1' then 1 else -1
          go x y err acc
            | x == x1' && y == y1' = reverse ((x,y):acc)
            | otherwise =
                let e2         = 2 * err
                    (x', err1) = if e2 > -ady then (x+sx, err-ady) else (x, err)
                    (y', err2) = if e2 <  adx then (y+sy, err1+adx) else (y, err1)
                in go x' y' err2 ((x,y):acc)
      in go x0 y0 (adx - ady) []

    setCell rows' y x ch =
      case splitAt y rows' of
        (before, row : after) ->
          let (pre, rest) = T.splitAt x row
              newRow      = pre <> T.singleton ch <> T.drop 1 rest
          in before ++ [newRow] ++ after
        _ -> rows'

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

-- | Generate chart footer with X-axis.
-- Renders every label at the same x-column used to plot its data point.
generateLineFooter :: LineChartData -> Double -> Double -> [Text]
generateLineFooter chartData minValue maxValue =
  let points     = linePoints chartData
      pointCount = V.length points
      yAxisWidth = 14   -- matches "  labelWidth + 3" used in renderLineChart
      chartW     = chartWidth (lineConfig chartData) - yAxisWidth - 10
      xLabel     = case lineXLabel chartData of
                     Just lbl -> lbl
                     Nothing  -> ""
      minValText = T.pack $ printf "%.2f" minValue
      maxValText = T.pack $ printf "%.2f" maxValue

      -- Build an axis row of spaces, then stamp each label at its x position
      axisRow    = buildXAxisRow pointCount chartW (V.toList points)

  in [ ""
     , T.replicate yAxisWidth " " <> axisRow
     , if not (T.null xLabel) then T.replicate yAxisWidth " " <> xLabel else ""
     , ""
     , "Value range: " <> minValText <> " to " <> maxValText
     ]

-- | Build the X-axis label row by placing each label at its grid x-position.
buildXAxisRow :: Int -> Int -> [LinePoint] -> Text
buildXAxisRow pointCount chartW points =
  let row0  = T.replicate (chartW + 2) " "
      pairs = zip [0..] points
  in foldl (placeLabel pointCount chartW) row0 pairs

placeLabel :: Int -> Int -> Text -> (Int, LinePoint) -> Text
placeLabel pointCount chartW row (idx, pt) =
  let xPos  = if pointCount <= 1
                then 0
                else round (fromIntegral idx / fromIntegral (pointCount - 1)
                             * fromIntegral (chartW - 1) :: Double)
      lbl   = pointLabel pt
      lblW  = T.length lbl
      -- Centre label under the point; clamp so it doesn't overflow
      start = max 0 (min (T.length row - lblW) (xPos - lblW `div` 2))
      (pre, rest) = T.splitAt start row
      -- Overwrite lblW characters
      newRow = pre <> lbl <> T.drop lblW rest
  in newRow

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
      
  in zipWith (\row valLabel -> valLabel <> " | " <> row <> " |") finalRows valueLabels
  where
    plotSeriesOnGrid rows' coords symbol =
      foldl (\rs (x, y) ->
        if y >= 0 && y < length rs && x >= 0 && x < chartWidth config - 22
        then setCell rs y x symbol
        else rs
      ) rows' coords

    -- Safe cell update: symbols must be single-byte ASCII
    setCell rows' y x ch =
      case splitAt y rows' of
        (before, row : after) ->
          let (pre, rest) = T.splitAt x row
              newRow      = pre <> T.singleton ch <> T.drop 1 rest
          in before ++ [newRow] ++ after
        _ -> rows'

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

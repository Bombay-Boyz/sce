{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.LineChart
Description : Line chart generation for time-series and ordered data
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
-}
module SCE.Chart.LineChart
  ( generateLineChart
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


data LinePoint = LinePoint
  { pointLabel :: Text
  , pointValue :: Double
  } deriving stock (Show, Eq)

data SeriesData = SeriesData
  { seriesName   :: Text
  , seriesPoints :: Vector LinePoint
  , seriesSymbol :: Char
  } deriving stock (Show, Eq)

data LineChartData = LineChartData
  { linePoints :: Vector LinePoint
  , lineConfig :: ChartConfig
  , lineScale  :: MeasurementScale
  , lineUnit   :: Maybe Text
  , lineTitle  :: Maybe Text
  , lineYLabel :: Maybe Text
  , lineXLabel :: Maybe Text
  } deriving stock (Show)

generateLineChart :: LineChartData -> SCEResult [Text]
generateLineChart chartData = do
  let pointCount = V.length (linePoints chartData)
  if pointCount < 2
    then Left (mkError E2002
                ("Need at least 2 data points, got " <> T.pack (show pointCount))
                ["Collect more data before running this operation."] Error)
    else do
      let points = linePoints chartData
          config = lineConfig chartData
          values = V.map pointValue points

      -- Safe: pointCount >= 2, so values is non-empty.
      (minValue, maxValue) <- safeMinMax "line chart values" values
      let range = maxValue - minValue

      if range <= 0
        then Left (mkError E2001 "All values are identical - cannot create line chart" [] Error)
        else do
          let header = generateLineHeader chartData
              chart  = renderLineChart points minValue maxValue config
              footer = generateLineFooter chartData minValue maxValue
          return $ header ++ chart ++ footer

renderLineChart :: Vector LinePoint -> Double -> Double -> ChartConfig -> [Text]
renderLineChart points minValue maxValue config =
  let height      = 12
      width       = chartWidth config
      labelWidth  = 12
      chartWidth' = width - labelWidth - 10
      range       = maxValue - minValue
      normalizeY val = height - 1 - floor ((val - minValue) / range * fromIntegral (height - 1))
      pointCount  = V.length points
      normalizeX idx = floor (fromIntegral idx / fromIntegral (pointCount - 1) * fromIntegral (chartWidth' - 1))
      coords      = V.toList $ V.imap (\idx pt -> (normalizeX idx, normalizeY (pointValue pt))) points
      grid        = buildGrid height chartWidth' coords
      valueLabels = generateValueLabels height minValue maxValue
  in zipWith (\row valLabel -> valLabel <> " | " <> row) grid valueLabels

buildGrid :: Int -> Int -> [(Int, Int)] -> [Text]
buildGrid height width coords =
  let emptyRow      = T.replicate width " "
      rows          = replicate height emptyRow
      rowsWithLines = case coords of
        []     -> rows
        [_]    -> rows
        (c:cs) -> foldl drawSegment rows (zip (c:cs) cs)
      rowsWithX = foldl (\rs (x,y) -> setCell rs y x 'X') rowsWithLines coords
  in map (<> " |") rowsWithX
  where
    drawSegment rows' ((x1,y1),(x2,y2)) =
      let pixels = segmentPixels x1 y1 x2 y2
      in foldl (\rs (x,y,ch) ->
           if y >= 0 && y < height && x >= 0 && x < width
           then setCell rs y x ch
           else rs
         ) rows' pixels

    segmentPixels x1 y1 x2 y2 =
      let dx = x2 - x1
          dy = y2 - y1
      in map (\(x,y) -> (x, y, dirChar dx dy)) (bresenham x1 y1 x2 y2)

    dirChar dx dy
      | dy == 0   = '-'
      | dx == 0   = '|'
      | (dx > 0) == (dy > 0) = '+'
      | otherwise = '/'

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

generateValueLabels :: Int -> Double -> Double -> [Text]
generateValueLabels height minValue maxValue =
  let range  = maxValue - minValue
      step   = range / fromIntegral (height - 1)
      values = reverse [minValue + step * fromIntegral i | i <- [0..height-1]]
  in map (\val -> T.pack $ printf "%10.2f" val) values

generateLineHeader :: LineChartData -> [Text]
generateLineHeader chartData =
  let unitSuffix = case lineUnit chartData of
                     Just unit -> " (" <> unit <> ")"
                     Nothing   -> ""
      title  = case lineTitle chartData of
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

generateLineFooter :: LineChartData -> Double -> Double -> [Text]
generateLineFooter chartData minValue maxValue =
  let points     = linePoints chartData
      pointCount = V.length points
      yAxisWidth = 14
      chartW     = chartWidth (lineConfig chartData) - yAxisWidth - 10
      xLabel     = case lineXLabel chartData of
                     Just lbl -> lbl
                     Nothing  -> ""
      minValText = T.pack $ printf "%.2f" minValue
      maxValText = T.pack $ printf "%.2f" maxValue
      axisRow    = buildXAxisRow pointCount chartW (V.toList points)
  in [ ""
     , T.replicate yAxisWidth " " <> axisRow
     , if not (T.null xLabel) then T.replicate yAxisWidth " " <> xLabel else ""
     , ""
     , "Value range: " <> minValText <> " to " <> maxValText
     ]

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
      start = max 0 (min (T.length row - lblW) (xPos - lblW `div` 2))
      (pre, rest) = T.splitAt start row
  in pre <> lbl <> T.drop lblW rest

generateMultiSeriesLineChart :: Vector SeriesData -> ChartConfig -> Maybe Text -> SCEResult [Text]
generateMultiSeriesLineChart series config titleMaybe = do
  if V.null series
    then Left (mkError E2001 "No series data provided" [] Error)
    else do
      let pointCounts = V.map (V.length . seriesPoints) series
      case V.uncons pointCounts of
        Nothing -> Left (mkError E2001 "No series data provided" [] Error)
        Just (firstCount, restCounts) ->
          if not (V.all (== firstCount) restCounts)
          then Left (mkError E2001 "All series must have the same number of points" [] Error)
          else
            let pointCount = firstCount
            in if pointCount < 2
               then Left (mkError E2002
                     ("Need at least 2 data points, got " <> T.pack (show pointCount))
                     ["Collect more data before running this operation."] Error)
               else do
                 let allValues = V.concatMap (V.map pointValue . seriesPoints) series
                 -- Safe: pointCount >= 2 and series non-empty, so allValues non-empty.
                 (minValue, maxValue) <- safeMinMax "multi-series values" allValues
                 let header = generateMultiSeriesHeader series titleMaybe
                     chart  = renderMultiSeriesChart series minValue maxValue config
                     legend = generateLegend series
                 return $ header ++ chart ++ legend

generateMultiSeriesHeader :: Vector SeriesData -> Maybe Text -> [Text]
generateMultiSeriesHeader _series titleMaybe =
  let title = case titleMaybe of
                Just t  -> t
                Nothing -> "Multi-Series Line Chart"
  in [T.replicate 70 "=", title, T.replicate 70 "=", ""]

renderMultiSeriesChart :: Vector SeriesData -> Double -> Double -> ChartConfig -> [Text]
renderMultiSeriesChart series minValue maxValue config =
  let height      = 12
      width       = chartWidth config
      labelWidth  = 12
      chartWidth' = width - labelWidth - 10
      range       = maxValue - minValue
      normalizeY val = height - 1 - floor ((val - minValue) / range * fromIntegral (height - 1))
      pointCount  = case V.uncons series of
                      Just (firstSeries, _) -> V.length (seriesPoints firstSeries)
                      Nothing               -> 0
      normalizeX idx = floor (fromIntegral idx / fromIntegral (pointCount - 1) * fromIntegral (chartWidth' - 1))
      emptyRow    = T.replicate chartWidth' " "
      rows        = replicate height emptyRow
      finalRows   = V.foldl' (\rs seriesData ->
                      let coords = V.toList $ V.imap (\idx pt ->
                                     (normalizeX idx, normalizeY (pointValue pt)))
                                     (seriesPoints seriesData)
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
        T.singleton (seriesSymbol s) <> " = " <> seriesName s) series
  in ["", "Legend:"] ++ map ("  " <>) legendItems

formatValue :: Double -> Text
formatValue val = T.pack $ printf "%.2f" val

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Return (min, max) of a non-empty vector, or Left on empty.
safeMinMax :: Text -> Vector Double -> SCEResult (Double, Double)
safeMinMax ctx v
  | V.null v  = Left (mkError E2002
      ("Cannot compute range for " <> ctx <> ": empty vector")
      ["Ensure at least one data point is present."] Error)
  | otherwise =
      let mn = V.foldl1' min v
          mx = V.foldl1' max v
      in Right (mn, mx)

when :: Bool -> SCEResult () -> SCEResult ()
when True  action = action
when False _      = Right ()

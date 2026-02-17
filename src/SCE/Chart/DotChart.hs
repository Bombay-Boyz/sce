{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.DotChart
Description : Horizontal dot plot generation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Generates ASCII horizontal dot plots.

A dot plot encodes value as the *position* of a single marker (·) along
a horizontal axis, rather than as bar length.  This makes it the correct
chart type for tightly-clustered data where bar charts would require a
truncated (non-zero) baseline to show any visual difference at all.

Rendering anatomy (track width = chartMaxBarLength):

  Label         |----·---------| $value
                ^    ^         ^
                0   dot        max

The track runs from the minimum value in the dataset to the maximum,
so every dot is placed honestly within the actual data range.
-}
module SCE.Chart.DotChart
  ( generateDotPlot
  ) where

import SCE.Core.Types
import SCE.Chart.BarChart (BarChartData(..), BarItem(..))
import SCE.Validation.LiquidTypes

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf (printf)

-- | Dot character used as the plot marker
dotChar :: Char
dotChar = 'X'

-- | Track fill character (background of the axis)
trackChar :: Char
trackChar = '-'

-- | Generate a horizontal dot plot from BarChartData.
--   Reuses BarChartData so the caller needs no new type.
generateDotPlot :: BarChartData -> SCEResult [Text]
generateDotPlot chartData = do
  -- Validate item count (same bounds as bar chart)
  _ <- validateValidBarCount (V.length $ barItems chartData)

  -- Validate all labels
  _ <- V.mapM (validateValidLabelLength . T.length . barLabel) (barItems chartData)

  let items    = barItems chartData
  let config   = barConfig chartData
  let trackLen = chartMaxBarLength config

  let allValues = V.map barValue items
  let minValue  = V.minimum allValues
  let maxValue  = V.maximum allValues
  let valueRange = maxValue - minValue

  -- Generate one dot line per item
  let dots = V.toList $ V.map (renderDot trackLen minValue maxValue valueRange config) items

  -- Header (same structure as BarChart so extractChartTitle works unchanged)
  let header = generateDotHeader chartData

  -- Scale indicator
  let scaleText =
        if chartShowScale config && valueRange > 0
          then [ "Scale: track spans "
                   <> formatValue minValue
                   <> " to "
                   <> formatValue maxValue
                   <> "  |  each character ≈ "
                   <> formatValue (valueRange / fromIntegral trackLen)
               ]
          else []

  return $ header ++ dots ++ [""] ++ scaleText

-- | Render a single dot line.
--
--   Position of the dot:
--     dotPos = round( (value - minValue) / valueRange * trackLen )
--   clamped to [0, trackLen].
--
--   When all values are identical (valueRange == 0) every dot lands at
--   position 0, which is honest: all values are the same.
renderDot
  :: Int        -- ^ track length in characters
  -> Double     -- ^ minimum value in dataset
  -> Double     -- ^ maximum value in dataset
  -> Double     -- ^ valueRange = maxValue - minValue
  -> ChartConfig
  -> BarItem
  -> Text
renderDot trackLen minValue maxValue valueRange config item =
  let label     = barLabel item
      value     = barValue item
      labelPad  = T.replicate (14 - min 14 (T.length label)) " "

      -- Dot position within [0, trackLen]
      dotPos
        | valueRange <= 0 = 0
        | otherwise       =
            min trackLen $ max 0 $
              round ((value - minValue) / valueRange * fromIntegral trackLen :: Double)

      -- Build the track: dashes with a dot at dotPos, then closing |
      track     = T.replicate dotPos (T.singleton trackChar)
               <> T.singleton dotChar
               <> T.replicate (trackLen - dotPos) (T.singleton trackChar)

      valueText =
        if chartShowValues config
          then " " <> formatValue value
          else ""

  in label <> labelPad <> "|" <> track <> "|" <> valueText

-- | Header for a dot plot — same "===/ title /===" structure as BarChart
--   so that extractChartTitle in TextRenderer works without modification.
generateDotHeader :: BarChartData -> [Text]
generateDotHeader chartData =
  let unitSuffix = case barUnit chartData of
                     Just u  -> " (" <> u <> ")"
                     Nothing -> ""
      title = case barTitle chartData of
                Just t  -> t <> unitSuffix
                Nothing -> "Dot Plot" <> unitSuffix
  in [T.replicate 60 "=", title, T.replicate 60 "=", ""]

-- | Format a numeric value for display (mirrors BarChart.formatValue)
formatValue :: Double -> Text
formatValue v
  | v >= 1000000 = T.pack $ printf "$%.1fM" (v / 1000000)
  | v >= 1000    = T.pack $ printf "$%.1fK" (v / 1000)
  | otherwise    = T.pack $ printf "$%.2f"  v

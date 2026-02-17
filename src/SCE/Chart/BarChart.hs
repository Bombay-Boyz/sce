{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.BarChart
Description : Horizontal and vertical bar chart generation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Generates ASCII horizontal bar charts with proper scaling.
-}
module SCE.Chart.BarChart
  ( -- * Bar Chart Generation
    generateHorizontalBarChart
  , generateVerticalBarChart
  , BarChartData(..)
  , BarItem(..)
  ) where

import SCE.Core.Types
import SCE.Validation.LiquidTypes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf (printf)

-- | Data for a single bar
data BarItem = BarItem
  { barLabel :: Text
  , barValue :: Double
  }
  deriving stock (Show, Eq)

-- | Complete bar chart data
data BarChartData = BarChartData
  { barItems  :: Vector BarItem
  , barConfig :: ChartConfig
  , barScale  :: MeasurementScale
  , barUnit   :: Maybe Text
  , barTitle  :: Maybe Text
  }
  deriving stock (Show)

-- | Generate a horizontal bar chart
generateHorizontalBarChart :: BarChartData -> SCEResult [Text]
generateHorizontalBarChart chartData = do
  -- Validate bar count
  _ <- validateValidBarCount (V.length $ barItems chartData)
  
  -- Validate all labels
  _ <- V.mapM (validateValidLabelLength . T.length . barLabel) (barItems chartData)
  
  let items = barItems chartData
  let config = barConfig chartData
  let maxValue = V.maximum $ V.map barValue items
  let maxBarLen = chartMaxBarLength config
  
  -- Calculate scale factor
  let scaleFactor = if maxValue > 0 
                      then fromIntegral maxBarLen / maxValue
                      else 1.0
  
  -- Generate bars
  let bars = V.toList $ V.map (renderBar scaleFactor config maxValue) items
  
  -- Add header
  let header = generateHeader chartData
  
  -- Add scale indicator if enabled
  let scaleText = if chartShowScale config
                    then [generateScaleIndicator maxValue maxBarLen (chartFillChar config)]
                    else []
  
  return $ header ++ bars ++ scaleText

-- | Render a single horizontal bar
renderBar :: Double -> ChartConfig -> Double -> BarItem -> Text
renderBar scaleFactor config maxValue item = 
  let label = barLabel item
      value = barValue item
      barLength = max 1 $ floor (value * scaleFactor)
      bar = T.replicate barLength (T.singleton $ chartFillChar config)
      padding = T.replicate (14 - T.length label) " "
      valueText = if chartShowValues config
                    then " " <> formatValue value
                    else ""
  in label <> padding <> "|" <> bar <> "|" <> valueText

-- | Generate chart header — uses barTitle if set, otherwise falls back to unit
generateHeader :: BarChartData -> [Text]
generateHeader chartData =
  let unitSuffix = case barUnit chartData of
                     Just unit -> " (" <> unit <> ")"
                     Nothing   -> ""
      title = case barTitle chartData of
                Just t  -> t <> unitSuffix
                Nothing -> "Bar Chart" <> unitSuffix
  in [T.replicate 60 "=", title, T.replicate 60 "=", ""]

-- | Generate scale indicator
generateScaleIndicator :: Double -> Int -> Char -> Text
generateScaleIndicator maxValue barLength fillChar =
  let valuePerChar = maxValue / fromIntegral barLength
  in T.pack $ "\nScale: Each '" ++ [fillChar] ++ "' represents " 
           ++ formatValueStr valuePerChar

-- | Format a numeric value for display
formatValue :: Double -> Text
formatValue v
  | v >= 1000000 = T.pack $ printf "$%.1fM" (v / 1000000)
  | v >= 1000 = T.pack $ printf "$%.1fK" (v / 1000)
  | otherwise = T.pack $ printf "$%.0f" v

formatValueStr :: Double -> String
formatValueStr v
  | v >= 1000000 = printf "$%.1fM" (v / 1000000)
  | v >= 1000 = printf "$%.1fK" (v / 1000)
  | otherwise = printf "$%.0f" v

-- | Generate vertical bar chart (stub for now)
generateVerticalBarChart :: BarChartData -> SCEResult [Text]
generateVerticalBarChart _ = 
  Left (mkError E2001 "Vertical bar charts not yet implemented" [] Error)

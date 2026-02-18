{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.BarChart
Description : Horizontal and vertical bar chart generation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
-}
module SCE.Chart.BarChart
  ( generateHorizontalBarChart
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

data BarItem = BarItem
  { barLabel :: Text
  , barValue :: Double
  } deriving stock (Show, Eq)

data BarChartData = BarChartData
  { barItems  :: Vector BarItem
  , barConfig :: ChartConfig
  , barScale  :: MeasurementScale
  , barUnit   :: Maybe Text
  , barTitle  :: Maybe Text
  } deriving stock (Show)

generateHorizontalBarChart :: BarChartData -> SCEResult [Text]
generateHorizontalBarChart chartData = do
  _ <- validateValidBarCount (V.length $ barItems chartData)
  _ <- V.mapM (validateValidLabelLength . T.length . barLabel) (barItems chartData)

  let items  = barItems chartData
  let config = barConfig chartData

  -- Safe: validateValidBarCount guarantees length >= 1
  maxValue <- case maximumOn barValue items of
                Just v  -> Right (barValue v)
                Nothing -> Left (mkError E2002 "No bar items to render" [] Error)

  let maxBarLen   = chartMaxBarLength config
      scaleFactor = if maxValue > 0
                      then fromIntegral maxBarLen / maxValue
                      else 1.0
      bars        = V.toList $ V.map (renderBar scaleFactor config maxValue) items
      header      = generateHeader chartData
      scaleText   = if chartShowScale config
                      then [generateScaleIndicator maxValue maxBarLen (chartFillChar config)]
                      else []

  return $ header ++ bars ++ scaleText

-- | 'V.maximumOn' equivalent — returns Nothing on empty vector.
maximumOn :: Ord b => (a -> b) -> Vector a -> Maybe a
maximumOn _ v | V.null v = Nothing
maximumOn f v = Just $ V.foldl1' (\acc x -> if f x > f acc then x else acc) v

renderBar :: Double -> ChartConfig -> Double -> BarItem -> Text
renderBar scaleFactor config _maxValue item =
  let label     = barLabel item
      value     = barValue item
      barLength = max 1 $ floor (value * scaleFactor)
      bar       = T.replicate barLength (T.singleton $ chartFillChar config)
      padding   = T.replicate (14 - T.length label) " "
      valueText = if chartShowValues config then " " <> formatValue value else ""
  in label <> padding <> "|" <> bar <> "|" <> valueText

generateHeader :: BarChartData -> [Text]
generateHeader chartData =
  let unitSuffix = case barUnit chartData of
                     Just unit -> " (" <> unit <> ")"
                     Nothing   -> ""
      title = case barTitle chartData of
                Just t  -> t <> unitSuffix
                Nothing -> "Bar Chart" <> unitSuffix
  in [T.replicate 60 "=", title, T.replicate 60 "=", ""]

generateScaleIndicator :: Double -> Int -> Char -> Text
generateScaleIndicator maxValue barLength fillChar =
  let valuePerChar = maxValue / fromIntegral barLength
  in T.pack $ "\nScale: Each '" ++ [fillChar] ++ "' represents "
           ++ formatValueStr valuePerChar

formatValue :: Double -> Text
formatValue v
  | v >= 1000000 = T.pack $ printf "$%.1fM" (v / 1000000)
  | v >= 1000    = T.pack $ printf "$%.1fK" (v / 1000)
  | otherwise    = T.pack $ printf "$%.0f" v

formatValueStr :: Double -> String
formatValueStr v
  | v >= 1000000 = printf "$%.1fM" (v / 1000000)
  | v >= 1000    = printf "$%.1fK" (v / 1000)
  | otherwise    = printf "$%.0f" v

generateVerticalBarChart :: BarChartData -> SCEResult [Text]
generateVerticalBarChart _ =
  Left (mkError E2001 "Vertical bar charts not yet implemented" [] Error)

{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.DotChart
Description : Horizontal dot plot generation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

A dot plot encodes value as the position of a single marker along a horizontal
axis rather than as bar length, making it ideal for tightly-clustered data.
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

dotChar :: Char
dotChar = 'X'

trackChar :: Char
trackChar = '-'

generateDotPlot :: BarChartData -> SCEResult [Text]
generateDotPlot chartData = do
  _ <- validateValidBarCount (V.length $ barItems chartData)
  _ <- V.mapM (validateValidLabelLength . T.length . barLabel) (barItems chartData)

  let items    = barItems chartData
  let config   = barConfig chartData
  let trackLen = chartMaxBarLength config
  let allValues = V.map barValue items

  -- Safe: validateValidBarCount guarantees length >= 1
  (minValue, maxValue) <- case (V.uncons allValues, V.unsnoc allValues) of
    (Just (mn, _), Just (_, mx)) ->
      Right (V.foldl' min mn allValues, V.foldl' max mx allValues)
    _ -> Left (mkError E2002 "No dot plot items to render" [] Error)

  let valueRange = maxValue - minValue
      dots       = V.toList $ V.map (renderDot trackLen minValue maxValue valueRange config) items
      header     = generateDotHeader chartData
      scaleText  =
        if chartShowScale config && valueRange > 0
          then [ "Scale: track spans "
                   <> formatValue minValue
                   <> " to "
                   <> formatValue maxValue
                   <> "  |  each character ~= "
                   <> formatValue (valueRange / fromIntegral trackLen)
               ]
          else []

  return $ header ++ dots ++ [""] ++ scaleText

renderDot :: Int -> Double -> Double -> Double -> ChartConfig -> BarItem -> Text
renderDot trackLen minValue _maxValue valueRange config item =
  let label    = barLabel item
      value    = barValue item
      labelPad = T.replicate (14 - min 14 (T.length label)) " "
      dotPos
        | valueRange <= 0 = 0
        | otherwise       =
            min trackLen $ max 0 $
              round ((value - minValue) / valueRange * fromIntegral trackLen :: Double)
      track     = T.replicate dotPos (T.singleton trackChar)
               <> T.singleton dotChar
               <> T.replicate (trackLen - dotPos) (T.singleton trackChar)
      valueText = if chartShowValues config then " " <> formatValue value else ""
  in label <> labelPad <> "|" <> track <> "|" <> valueText

generateDotHeader :: BarChartData -> [Text]
generateDotHeader chartData =
  let unitSuffix = case barUnit chartData of
                     Just u  -> " (" <> u <> ")"
                     Nothing -> ""
      title = case barTitle chartData of
                Just t  -> t <> unitSuffix
                Nothing -> "Dot Plot" <> unitSuffix
  in [T.replicate 60 "=", title, T.replicate 60 "=", ""]

formatValue :: Double -> Text
formatValue v
  | v >= 1000000 = T.pack $ printf "$%.1fM" (v / 1000000)
  | v >= 1000    = T.pack $ printf "$%.1fK" (v / 1000)
  | otherwise    = T.pack $ printf "$%.2f"  v

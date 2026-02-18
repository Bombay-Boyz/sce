{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Chart.ConfidenceInterval
Description : Confidence interval chart generation with statistical validation
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause
-}
module SCE.Chart.ConfidenceInterval
  ( generateConfidenceInterval
  , ConfidenceIntervalData(..)
  , ConfidencePoint(..)
  , validateConfidenceIntervalData
  , isConfidenceIntervalAppropriate
  , detectConfidenceIntervalColumns
  ) where

import SCE.Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Printf (printf)
import Data.Maybe (isJust, fromMaybe)

data ConfidencePoint = ConfidencePoint
  { cpLabel      :: Text
  , cpEstimate   :: Double
  , cpLower      :: Double
  , cpUpper      :: Double
  , cpInnerLower :: Maybe Double
  , cpInnerUpper :: Maybe Double
  } deriving stock (Show, Eq)

data ConfidenceIntervalData = ConfidenceIntervalData
  { ciPoints          :: Vector ConfidencePoint
  , ciTitle           :: Maybe Text
  , ciYAxisLabel      :: Text
  , ciConfig          :: ChartConfig
  , ciConfidenceLevel :: Double
  } deriving stock (Show)

generateConfidenceInterval :: ConfidenceIntervalData -> SCEResult [Text]
generateConfidenceInterval ciData = do
  _ <- validateConfidenceIntervalData ciData

  let points = ciPoints ciData
  let n      = V.length points

  when (n < 2) $
    Left (mkError E2002
      ("Need at least 2 data points, got " <> T.pack (show n))
      ["Collect more data before running this operation."] Error)

  _ <- V.mapM validatePoint points

  -- Safe: n >= 2 guaranteed above, so allVals is non-empty.
  chart <- renderConfidenceInterval ciData

  let header        = generateCIHeader ciData
      legend        = renderCILegend ciData
      interpretation = renderCIInterpretation ciData

  return $ header ++ chart ++ [""] ++ legend ++ [""] ++ interpretation

validateConfidenceIntervalData :: ConfidenceIntervalData -> SCEResult ()
validateConfidenceIntervalData ciData = do
  let cl = ciConfidenceLevel ciData
  when (cl < 0.5 || cl > 0.999) $
    Left (mkError E2001
      ("Confidence level must be in [50%, 99.9%], got "
       <> T.pack (show (cl * 100)) <> "%")
      ["Standard values are 90%, 95%, or 99%."] Error)

  let n = V.length (ciPoints ciData)
  when (n < 2) $
    Left (mkError E2002
      ("Need at least 2 data points, got " <> T.pack (show n))
      ["Collect more data before running this operation."] Error)

validatePoint :: ConfidencePoint -> SCEResult ConfidencePoint
validatePoint point = do
  when (cpLower point > cpEstimate point) $
    Left (mkError E2001
      ("Lower bound " <> T.pack (show (cpLower point))
       <> " > estimate " <> T.pack (show (cpEstimate point))
       <> " for " <> cpLabel point)
      ["Ensure lower bound <= point estimate <= upper bound."] Error)

  when (cpEstimate point > cpUpper point) $
    Left (mkError E2001
      ("Estimate " <> T.pack (show (cpEstimate point))
       <> " > upper bound " <> T.pack (show (cpUpper point))
       <> " for " <> cpLabel point)
      ["Ensure lower bound <= point estimate <= upper bound."] Error)

  case (cpInnerLower point, cpInnerUpper point) of
    (Just il, Just iu) -> do
      when (il < cpLower point || il > cpEstimate point) $
        Left (mkError E2001
          "Inner lower bound must be between outer lower bound and estimate" [] Error)
      when (iu > cpUpper point || iu < cpEstimate point) $
        Left (mkError E2001
          "Inner upper bound must be between estimate and outer upper bound" [] Error)
    _ -> Right ()

  return point

isConfidenceIntervalAppropriate :: Int -> Bool -> SCEResult Bool
isConfidenceIntervalAppropriate n hasUncertainty
  | n < 2            = return False
  | not hasUncertainty = return False
  | otherwise        = return True

detectConfidenceIntervalColumns :: Vector Text -> Maybe (Text, Text, Text, Text)
detectConfidenceIntervalColumns colNames =
  let names        = V.toList colNames
      hasLabel     = any (matchesPattern ["date","time","period","year","label"]) names
      hasEstimate  = any (matchesPattern ["estimate","value","mean","median","point"]) names
      hasLower     = any (matchesPattern ["lower","min","ci_lower","lower_bound"]) names
      hasUpper     = any (matchesPattern ["upper","max","ci_upper","upper_bound"]) names
  in if hasLabel && hasEstimate && hasLower && hasUpper
       then Just ( findFirst ["date","time","period","year","label"] names
                 , findFirst ["estimate","value","mean","median","point"] names
                 , findFirst ["lower","min","ci_lower","lower_bound"] names
                 , findFirst ["upper","max","ci_upper","upper_bound"] names
                 )
       else Nothing
  where
    matchesPattern patterns name =
      let lower = T.toLower name
      in any (`T.isInfixOf` lower) patterns

    findFirst patterns names =
      case filter (matchesPattern patterns) names of
        (n':_) -> n'
        []     -> ""

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | Render the CI chart. Returns Left when points vector is empty (should not
-- happen after validation, but we keep it in SCEResult to be safe).
renderConfidenceInterval :: ConfidenceIntervalData -> SCEResult [Text]
renderConfidenceInterval ciData = do
  let points = V.toList $ ciPoints ciData

  -- Build the list of all plotted values to find global bounds.
  let allVals = concatMap (\p -> [cpLower p, cpEstimate p, cpUpper p]) points

  -- Safe: points is non-empty (n >= 2 guaranteed by caller).
  (globalMin, globalMax) <- case allVals of
    []     -> Left (mkError E2002 "No values to render in CI chart" [] Error)
    (x:xs) -> Right (foldl min x xs, foldl max x xs)

  let range  = globalMax - globalMin
      width  = 60

      scalePos :: Double -> Int
      scalePos val
        | range > 0 = round ((val - globalMin) / range * fromIntegral (width - 1))
        | otherwise = width `div` 2

      renderPoint p =
        let label    = T.justifyLeft 12 ' ' (cpLabel p)
            estPos   = scalePos (cpEstimate p)
            lowerPos = scalePos (cpLower p)
            upperPos = scalePos (cpUpper p)
            line = V.generate width $ \i ->
              case () of
                _ | i == estPos   -> '\x25cf'
                  | i == lowerPos -> '\x251c'
                  | i == upperPos -> '\x2524'
                  | i > lowerPos && i < upperPos ->
                      if isJust (cpInnerLower p) && isJust (cpInnerUpper p)
                      then let ilPos = scalePos (fromMaybe 0 $ cpInnerLower p)
                               iuPos = scalePos (fromMaybe 0 $ cpInnerUpper p)
                           in if i >= ilPos && i <= iuPos then '\x2593' else '\x2500'
                      else '\x2500'
                  | otherwise -> ' '
            estVal = T.pack $ printf "%6.2f" (cpEstimate p)
        in label <> " \x2502" <> T.pack (V.toList line) <> "\x2502 " <> estVal

      pointLines = map renderPoint points
      axisLine   = formatScaleAxis globalMin globalMax width

  return $ pointLines ++ ["", axisLine]

formatScaleAxis :: Double -> Double -> Int -> Text
formatScaleAxis minVal maxVal width =
  let minStr = T.pack $ printf "%.1f" minVal
      maxStr = T.pack $ printf "%.1f" maxVal
      midVal = (minVal + maxVal) / 2
      midStr = T.pack $ printf "%.1f" midVal
      midPos = width `div` 2
      gap1   = midPos - T.length minStr - 1
      gap2   = width - midPos - T.length midStr - T.length maxStr - 1
  in "             " <> minStr
     <> T.replicate (max 0 gap1) " "
     <> midStr
     <> T.replicate (max 0 gap2) " "
     <> maxStr

generateCIHeader :: ConfidenceIntervalData -> [Text]
generateCIHeader ciData =
  let title     = fromMaybe "Confidence Interval Chart" (ciTitle ciData)
      cl        = ciConfidenceLevel ciData
      clPercent = T.pack $ printf "%.0f%%" (cl * 100)
  in [ T.replicate 70 "=", title, T.replicate 70 "=", ""
     , "Confidence Level: " <> clPercent
     , "Y-axis: " <> ciYAxisLabel ciData
     , ""
     ]

renderCILegend :: ConfidenceIntervalData -> [Text]
renderCILegend ciData =
  let cl        = ciConfidenceLevel ciData
      clPercent = T.pack $ printf "%.0f%%" (cl * 100)
      hasInner  = case V.uncons (ciPoints ciData) of
                    Just (p, _) -> isJust (cpInnerLower p)
                    Nothing     -> False
  in [ "Legend:"
     , "  \x25cf     Point estimate"
     , "  \x251c\x2500\x2524   " <> clPercent <> " confidence interval"
     , "  \x2500     Uncertainty range"
     ] ++ if hasInner then ["  \x2593     50% confidence band"] else []

renderCIInterpretation :: ConfidenceIntervalData -> [Text]
renderCIInterpretation ciData =
  let cl        = ciConfidenceLevel ciData
      clPercent = T.pack $ printf "%.0f%%" (cl * 100)
      points    = V.toList $ ciPoints ciData
      avgWidth  = if null points then 0
                  else sum (map (\p -> cpUpper p - cpLower p) points)
                       / fromIntegral (length points)
      (firstWidth, lastWidth) =
        case (points, reverse points) of
          (p1:_, p2:_) -> (cpUpper p1 - cpLower p1, cpUpper p2 - cpLower p2)
          _            -> (0, 0)
      trend
        | lastWidth > firstWidth * 1.2 = "Uncertainty is INCREASING over time"
        | lastWidth < firstWidth * 0.8 = "Uncertainty is DECREASING over time"
        | otherwise                    = "Uncertainty is STABLE over time"
  in [ "Interpretation:"
     , "  - Each point shows an estimate with " <> clPercent <> " confidence bounds"
     , "  - Average interval width: +/-" <> T.pack (printf "%.2f" (avgWidth / 2))
     , "  - " <> trend
     , ""
     ]

-- Helper
when :: Bool -> SCEResult () -> SCEResult ()
when True  action = action
when False _      = Right ()



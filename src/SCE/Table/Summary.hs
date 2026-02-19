{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Table.Summary
Description : ASCII summary statistics table formatter (Phase 4)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Formats a 'DescriptiveStats' record as a human-readable ASCII table.

Two entry points:

  * 'formatSummaryTable'   — single-variable summary
  * 'formatSummaryWithCI'  — side-by-side multi-variable summary

Design constraints:
  * No partial functions.
  * Output fits in 80 columns for up to 4 variables side-by-side.
  * Pure — no IO.
-}
module SCE.Table.Summary
  ( formatSummaryTable
  , formatSummaryWithCI
  ) where

import SCE.Statistics.Descriptive ( DescriptiveStats(..) )
import SCE.Statistics.TestResult  ( ConfidenceInterval(..) )

import           Data.Text ( Text )
import qualified Data.Text as T
import           Text.Printf ( printf )

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Format one variable's descriptive statistics as an ASCII table.
--
-- Example output:
--
-- > ============================================================
-- > Descriptive Statistics: score
-- > ============================================================
-- > n                          100
-- > Mean                      72.4  (SE: 1.82)
-- > 95% CI                [68.8, 76.0]
-- > Median                    73.0
-- > Std Dev                   18.2
-- > Variance                 331.2
-- > Min / Max              22.0 / 98.0
-- > Range                     76.0
-- > IQR                       24.0
-- > MAD                       12.3
-- > CV                        25.2%
-- > Skewness                  -0.31  (approx. symmetric)
-- > Kurtosis                  -0.12  (approx. normal tails)
-- > ============================================================
formatSummaryTable :: Text -> DescriptiveStats -> [Text]
formatSummaryTable varName ds =
  let rule = T.replicate 60 "="
      rows = buildRows ds
  in [ rule
     , "Descriptive Statistics: " <> varName
     , rule
     ]
  ++ map renderRow rows
  ++ [rule]

-- | Format several variables' descriptive stats side-by-side.
--
-- Each pair is (variable name, DescriptiveStats).
-- Produces one column per variable, with shared row labels on the left.
-- Columns are right-aligned at 14 characters each.
-- Up to 4 variables fit comfortably in 80 columns.
formatSummaryWithCI :: [(Text, DescriptiveStats)] -> [Text]
formatSummaryWithCI [] = ["(no variables)"]
formatSummaryWithCI pairs =
  let names   = map fst pairs
      stats   = map snd pairs
      colW    = 14
      labelW  = 16
      rule    = T.replicate (labelW + colW * length pairs + 2) "="

      -- Header: variable names
      headerCells = map (padCenter colW) names
      headerRow   = padRight labelW " " <> T.intercalate " " headerCells

      -- One shared set of row labels
      rowDefs = summaryRowDefs

      -- Build one value per variable per row
      dataRows = map (mkMultiRow labelW colW stats) rowDefs

  in [ rule
     , "Descriptive Statistics"
     , rule
     , headerRow
     , T.replicate (labelW + colW * length pairs + 2) "-"
     ]
  ++ dataRows
  ++ [rule]

-- ---------------------------------------------------------------------------
-- Row definitions — (label, extractor from DescriptiveStats -> Text)
-- ---------------------------------------------------------------------------

type RowDef = (Text, DescriptiveStats -> Text)

buildRows :: DescriptiveStats -> [(Text, Text)]
buildRows ds = map (\(lbl, f) -> (lbl, f ds)) summaryRowDefs

summaryRowDefs :: [RowDef]
summaryRowDefs =
  [ ("n",          \ds -> T.pack (show (dsCount ds)))
  , ("Mean",       \ds -> fmtMaybeMeanSE ds)
  , ("95% CI",     \ds -> fmtCI (dsMeanCI ds))
  , ("Median",     \ds -> fmtMaybeD (dsMedian ds))
  , ("Std Dev",    \ds -> fmtMaybeD (dsStdDev ds))
  , ("Variance",   \ds -> fmtMaybeD (dsVariance ds))
  , ("Min / Max",  \ds -> fmtMinMax ds)
  , ("Range",      \ds -> fmtD (dsRange ds))
  , ("IQR",        \ds -> fmtD (dsIQR ds))
  , ("MAD",        \ds -> fmtD (dsMAD ds))
  , ("CV",         \ds -> fmtCV (dsCV ds))
  , ("Skewness",   \ds -> fmtSkew (dsSkewness ds))
  , ("Kurtosis",   \ds -> fmtKurt (dsKurtosis ds))
  ]

-- ---------------------------------------------------------------------------
-- Field formatters
-- ---------------------------------------------------------------------------

-- | Mean with SE appended: "72.4  (SE: 1.82)"
fmtMaybeMeanSE :: DescriptiveStats -> Text
fmtMaybeMeanSE ds =
  case (dsMean ds, dsStdDev ds) of
    (Nothing, _)      -> "--"
    (Just mn, Nothing) -> fmtD mn
    (Just mn, Just sd) ->
      let se = sd / sqrt (fromIntegral (dsCount ds))
      in fmtD mn <> "  (SE: " <> fmtD se <> ")"

-- | 95% CI: "[68.8, 76.0]"
fmtCI :: Maybe ConfidenceInterval -> Text
fmtCI Nothing   = "--"
fmtCI (Just ci) =
  "[" <> fmtD (ciLower ci) <> ", " <> fmtD (ciUpper ci) <> "]"

-- | "22.0 / 98.0"
fmtMinMax :: DescriptiveStats -> Text
fmtMinMax ds = fmtD (dsMin ds) <> " / " <> fmtD (dsMax ds)

-- | CV as percentage: "25.2%"
fmtCV :: Maybe Double -> Text
fmtCV Nothing   = "--"
fmtCV (Just cv) = T.pack (printf "%.1f%%" (cv * 100.0))

-- | Skewness with qualitative label.
fmtSkew :: Maybe Double -> Text
fmtSkew Nothing  = "--"
fmtSkew (Just s) = fmtD s <> "  (" <> skewLabel s <> ")"
  where
    skewLabel v
      | abs v < 0.5 = "approx. symmetric"
      | v < 0       = "moderately left-skewed"
      | otherwise   = "moderately right-skewed"

-- | Excess kurtosis with qualitative label.
fmtKurt :: Maybe Double -> Text
fmtKurt Nothing  = "--"
fmtKurt (Just k) = fmtD k <> "  (" <> kurtLabel k <> ")"
  where
    kurtLabel v
      | abs v < 0.5 = "approx. normal tails"
      | v > 0       = "heavy tails (leptokurtic)"
      | otherwise   = "light tails (platykurtic)"

fmtMaybeD :: Maybe Double -> Text
fmtMaybeD Nothing  = "--"
fmtMaybeD (Just d) = fmtD d

fmtD :: Double -> Text
fmtD d = T.pack (printf "%.2f" d)

-- ---------------------------------------------------------------------------
-- Single-column row renderer
-- ---------------------------------------------------------------------------

-- | Render one "(label, value)" pair as a fixed-width row.
renderRow :: (Text, Text) -> Text
renderRow (lbl, val) =
  padRight 20 lbl <> padLeft 40 val

-- ---------------------------------------------------------------------------
-- Multi-column row renderer
-- ---------------------------------------------------------------------------

-- | Render one row across multiple variable columns.
mkMultiRow :: Int -> Int -> [DescriptiveStats] -> RowDef -> Text
mkMultiRow labelW colW statsList (lbl, f) =
  let cells = map (padLeft colW . f) statsList
  in padRight labelW lbl <> T.intercalate " " cells

-- ---------------------------------------------------------------------------
-- Padding helpers — all total
-- ---------------------------------------------------------------------------

padLeft :: Int -> Text -> Text
padLeft w t
  | T.length t >= w = t
  | otherwise       = T.replicate (w - T.length t) " " <> t

padRight :: Int -> Text -> Text
padRight w t
  | T.length t >= w = t
  | otherwise       = t <> T.replicate (w - T.length t) " "

padCenter :: Int -> Text -> Text
padCenter w t =
  let pad  = max 0 (w - T.length t)
      lpad = pad `div` 2
      rpad = pad - lpad
  in T.replicate lpad " " <> t <> T.replicate rpad " "

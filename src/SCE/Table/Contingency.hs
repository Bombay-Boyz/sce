{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StrictData         #-}

{-|
Module      : SCE.Table.Contingency
Description : ASCII formatting for contingency tables (Phase 4)
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Renders a 'ContingencyTable' as aligned ASCII text.

Two entry points:

  * 'formatContingencyTable'     — observed counts with row percentages,
                                   column totals, and expected counts
  * 'formatContingencyWithStats' — same table, plus chi-square result footer

Design constraints:
  * No partial functions. All indexing uses safe helpers with explicit fallbacks.
  * 80-column safe output.
  * Pure — no IO.
-}
module SCE.Table.Contingency
  ( formatContingencyTable
  , formatContingencyWithStats
  ) where

import SCE.Statistics.Categorical ( ContingencyTable(..) )
import SCE.Statistics.TestResult
  ( TestResult(..), interpretEffect, interpretEffectSize )

import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Vector as V
import           Text.Printf ( printf )

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Format a contingency table as ASCII.
--
-- Example (2 x 2 table, n = 100):
--
-- > ============================================================
-- > Contingency Table  (n = 100)
-- > ============================================================
-- >              |  Tea       |  Coffee    |  Total
-- > -------------+------------+------------+------------
-- >  Male        |  15 (30%)  |  35 (70%)  |  50
-- >  Female      |  30 (60%)  |  20 (40%)  |  50
-- > -------------+------------+------------+------------
-- >  Total       |  45        |  55        |  100
-- > ============================================================
-- >  Expected counts (under H0 of independence):
-- >  Male        |  22.5      |  27.5      |  50
-- >  Female      |  22.5      |  27.5      |  50
-- > ============================================================
formatContingencyTable :: ContingencyTable -> [Text]
formatContingencyTable ct =
  let rowLabels = ctRowLabels  ct
      colLabels = ctColLabels  ct
      nRows     = length rowLabels
      nCols     = length colLabels
      n         = ctGrandTotal ct

      -- Safe indexed access
      obsAt  i j = safeVV (ctCounts    ct) i j
      rowTot i   = safeV  (ctRowTotals ct) i
      colTot j   = safeV  (ctColTotals ct) j
      expAt  i j
        | n == 0    = 0.0
        | otherwise = fromIntegral (rowTot i) * fromIntegral (colTot j)
                      / fromIntegral n :: Double

      -- Layout widths
      labelW  = 2 + safeMaxLen 6 rowLabels
      colW    = 12   -- wide enough for "9999 (100%)"

      -- Separator lines
      rule    = T.replicate 60 "="
      divider = T.replicate labelW "-"
             <> "+"
             <> T.intercalate "+"
                  (replicate (nCols + 1) (T.replicate (colW + 2) "-"))

      -- Header row
      headerCells = map (padCenter colW) colLabels ++ [padCenter colW "Total"]
      headerRow   = padRight labelW " "
                 <> "| "
                 <> T.intercalate " | " headerCells

      -- Table rows
      obsRows = map (mkObsRow  labelW colW nCols rowLabels obsAt rowTot)
                    [0 .. nRows - 1]
      totRow  = mkTotalsRow labelW colW nCols colTot n
      expRows = map (mkExpRow  labelW colW nCols rowLabels expAt rowTot)
                    [0 .. nRows - 1]

  in [ rule
     , "Contingency Table  (n = " <> T.pack (show n) <> ")"
     , rule
     , headerRow
     , divider
     ]
  ++ obsRows
  ++ [ divider
     , totRow
     , rule
     , " Expected counts (under H0 of independence):"
     ]
  ++ expRows
  ++ [rule]

-- | Format a contingency table with a chi-square result footer.
--
-- Appends: statistic, degrees of freedom, p-value, significance stars,
-- and effect size if available.
formatContingencyWithStats :: ContingencyTable -> TestResult -> [Text]
formatContingencyWithStats ct tr =
  formatContingencyTable ct ++ statsFooter tr

-- ---------------------------------------------------------------------------
-- Row builders
-- ---------------------------------------------------------------------------

-- | Observed-count row: "count (row%)" per cell, then row total.
mkObsRow
  :: Int                  -- ^ label column width
  -> Int                  -- ^ data column width
  -> Int                  -- ^ number of data columns
  -> [Text]               -- ^ row labels
  -> (Int -> Int -> Int)  -- ^ obsAt i j
  -> (Int -> Int)         -- ^ rowTot i
  -> Int                  -- ^ row index i
  -> Text
mkObsRow labelW colW nCols rowLabels obsAt rowTot i =
  let lbl     = safeListIdx "" rowLabels i
      rt      = rowTot i
      cells   = [ padLeft colW (fmtObsCell (obsAt i j) rt)
                | j <- [0 .. nCols - 1] ]
      totCell = padLeft colW (T.pack (show rt))
  in padRight labelW (" " <> lbl)
  <> "| " <> T.intercalate " | " cells
  <> " | " <> totCell

-- | Column totals row.
mkTotalsRow :: Int -> Int -> Int -> (Int -> Int) -> Int -> Text
mkTotalsRow labelW colW nCols colTot n =
  let cells   = [ padLeft colW (T.pack (show (colTot j)))
                | j <- [0 .. nCols - 1] ]
      totCell = padLeft colW (T.pack (show n))
  in padRight labelW " Total"
  <> "| " <> T.intercalate " | " cells
  <> " | " <> totCell

-- | Expected-count row: one decimal place per cell, then row total.
mkExpRow
  :: Int
  -> Int
  -> Int
  -> [Text]
  -> (Int -> Int -> Double)
  -> (Int -> Int)
  -> Int
  -> Text
mkExpRow labelW colW nCols rowLabels expAt rowTot i =
  let lbl     = safeListIdx "" rowLabels i
      rt      = rowTot i
      cells   = [ padLeft colW (T.pack (printf "%.1f" (expAt i j)))
                | j <- [0 .. nCols - 1] ]
      totCell = padLeft colW (T.pack (show rt))
  in padRight labelW (" " <> lbl)
  <> "| " <> T.intercalate " | " cells
  <> " | " <> totCell

-- ---------------------------------------------------------------------------
-- Stats footer
-- ---------------------------------------------------------------------------

statsFooter :: TestResult -> [Text]
statsFooter tr =
  let pv    = trPValue    tr
      stat  = trStatistic tr
      dfStr = case trDegreesOfFreedom tr of
                Just df -> "(" <> T.pack (printf "%.0f" df) <> ")"
                Nothing -> ""
      effStr = case trEffectSize tr of
                 Just es -> "  Effect = "
                         <> T.pack (printf "%.3f" (interpretEffectSize es))
                         <> " (" <> interpretEffect es <> ")"
                 Nothing -> ""
  in [ T.replicate 60 "-"
     , "Chi-square" <> dfStr
       <> " = " <> T.pack (printf "%.3f" stat)
       <> ",  p = " <> fmtP pv
       <> "  " <> sigStars pv
       <> effStr
     , T.replicate 60 "-"
     ]

sigStars :: Double -> Text
sigStars p
  | p < 0.001 = "***"
  | p < 0.01  = "**"
  | p < 0.05  = "*"
  | otherwise  = "ns"

fmtP :: Double -> Text
fmtP p
  | p < 0.001 = "< 0.001"
  | otherwise  = T.pack (printf "%.3f" p)

fmtObsCell :: Int -> Int -> Text
fmtObsCell count rowTotal =
  let pctTxt
        | rowTotal == 0 = " - "
        | otherwise     =
            T.pack (printf "%.0f%%" pctVal)
      pctVal = 100.0 * fromIntegral count
                     / fromIntegral rowTotal :: Double
  in T.pack (show count) <> " (" <> pctTxt <> ")"

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

-- ---------------------------------------------------------------------------
-- Safe access helpers
-- ---------------------------------------------------------------------------

safeV :: V.Vector Int -> Int -> Int
safeV vec i = case vec V.!? i of
  Just x  -> x
  Nothing -> 0

safeVV :: V.Vector (V.Vector Int) -> Int -> Int -> Int
safeVV mat i j = case mat V.!? i of
  Nothing  -> 0
  Just row -> case row V.!? j of
    Just x  -> x
    Nothing -> 0

safeListIdx :: a -> [a] -> Int -> a
safeListIdx def xs i = case drop i xs of
  (x:_) -> x
  []    -> def

-- | Maximum Text.length in a list, with a minimum floor.
safeMaxLen :: Int -> [Text] -> Int
safeMaxLen flr []     = flr
safeMaxLen flr (x:xs) =
  foldl (\acc t -> max acc (T.length t)) (max flr (T.length x)) xs

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Table.Formatter
Description : ASCII table formatting and rendering
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Formats raw and enriched tables as aligned ASCII text.

Output layout:
  1. Raw Data Table   – original data, tabular, box-drawn borders
  2. Analytics Table  – sorted rows + derived columns (Δ, cumulative,
                        % of total) plus subtotals per text group and
                        a grand-total row
-}
module SCE.Table.Formatter
  ( formatTable
  , formatRawTable
  , formatAnalyticsTable
  , formatEnrichedTable
  , formatTableRow
  , TableFormatConfig(..)
  ) where

import SCE.Core.Types
import SCE.Table.Enrichment
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (sortBy, groupBy)
import Data.Ord (comparing, Down(..))
import Data.Function (on)
import Text.Printf (printf)

------------------------------------------------------------
-- Configuration
------------------------------------------------------------

data TableFormatConfig = TableFormatConfig
  { formatWidth        :: Int
  , formatAlignNumeric :: Bool
  , formatShowBorders  :: Bool
  }
  deriving stock (Show, Eq)

------------------------------------------------------------
-- Column width helpers
------------------------------------------------------------

-- | Compute per-column display widths (at least header length, at least 6)
colWidths :: Vector ColumnName -> Vector DataRow -> Vector Int
colWidths cols rows =
  V.map (\col ->
    let headerW = T.length col
        valW    = V.foldl' (\acc row ->
                    case M.lookup col row of
                      Just v  -> max acc (T.length $ formatDataValue v)
                      Nothing -> acc
                  ) 0 rows
    in max 6 (max headerW valW)
  ) cols

-- | Render one row using given widths, right-aligning numeric cells
renderRow :: Vector ColumnName -> Vector Int -> DataRow -> Text
renderRow cols widths row =
  "| " <>
  T.intercalate " | "
    (V.toList $ V.zipWith3 (\col w _ ->
        let raw = case M.lookup col row of
                    Just v  -> formatDataValue v
                    Nothing -> "--"
            isNum = case M.lookup col row of
                      Just (NumericValue _) -> True
                      Just (IntegerValue _) -> True
                      _                    -> False
        in if isNum
             then T.justifyRight w ' ' raw
             else T.justifyLeft  w ' ' raw
      ) cols widths (V.replicate (V.length cols) ())
    ) <>
  " |"

-- | Horizontal rule line
hRule :: Vector Int -> Char -> Text
hRule widths corner =
  T.singleton corner <>
  T.intercalate (T.singleton corner)
    (V.toList $ V.map (\w -> T.replicate (w + 2) "-") widths) <>
  T.singleton corner

------------------------------------------------------------
-- Section 1: Raw Data Table
------------------------------------------------------------

-- | Format the original unmodified data as a clean box table
formatRawTable :: DataTable -> [Text]
formatRawTable table =
  let cols   = tableColumns table
      rows   = tableRows table
      ws     = colWidths cols rows
      top    = hRule ws '+'
      mid    = hRule ws '+'
      bot    = hRule ws '+'
      hdr    = renderHeader cols ws
      dataRs = V.toList $ V.map (renderRow cols ws) rows
      title  = maybe "RAW DATA" id (metaTitle (tableMetadata table))
      nRows  = V.length rows
      nCols  = V.length cols
      note   = "  " <> T.pack (show nRows) <> " rows  ×  "
                    <> T.pack (show nCols) <> " columns"
  in  [ T.replicate 72 "═"
      , "TABLE 1 – RAW DATA:  " <> title
      , T.replicate 72 "═"
      , top
      , hdr
      , mid
      ]
   ++ dataRs
   ++ [ bot
      , note
      ]

-- | Render header row
renderHeader :: Vector ColumnName -> Vector Int -> Text
renderHeader cols widths =
  "| " <>
  T.intercalate " | "
    (V.toList $ V.zipWith (\col w -> T.justifyLeft w ' ' col) cols widths) <>
  " |"

-- | Alias kept for backward compatibility
formatTable :: DataTable -> [Text]
formatTable = formatRawTable

------------------------------------------------------------
-- Section 2: Analytics Table
------------------------------------------------------------

{-|
Builds a unified analytics table from an EnrichedTable.

Layout per numeric column (original columns only; derived columns
added inline):
  Label | Value | Δ (diff) | Cumulative | % of Total

Rows are sorted descending by the first numeric column.
Text-column groups each get a subtotal row.
A grand-total row closes the table.
-}
formatAnalyticsTable :: EnrichedTable -> [Text]
formatAnalyticsTable enriched =
  let origTable  = enrichedOriginal enriched
      origCols   = tableColumns origTable
      enrichCols = enrichedColumns enriched
      enrichRows = V.toList $ enrichedRows enriched

      -- Identify original numeric columns
      numericOrigCols = V.toList $ V.filter (isNumCol origTable) origCols

      -- Identify the first text column (used for grouping / label)
      textCols = V.toList $ V.filter (not . isNumCol origTable) origCols

      -- For each original numeric col, find its derived siblings
      allDisplayCols = concatMap (buildColGroup enrichCols) numericOrigCols

      -- Combine: text cols first, then derived groups
      displayCols = V.fromList $ textCols ++ allDisplayCols

      -- Sort rows descending by first numeric column (if any)
      sortedRows = case numericOrigCols of
          []    -> enrichRows
          (c:_) -> sortBy (comparing (Down . numVal c)) enrichRows

      -- Compute per-column widths across sorted rows + header
      ws = colWidths displayCols (V.fromList sortedRows)

      top    = hRule ws '+'
      divider = hRule ws '+'
      bot    = hRule ws '+'
      hdr    = renderHeader displayCols ws

      -- Totals map
      totalsMap = maybe M.empty id (enrichedTotals enriched)

      -- Group rows by first text column value (for subtotals)
      tableBodyLines =
        case textCols of
          []    -> map (renderRow displayCols ws) sortedRows
          (gc:_) ->
            let groups = groupBy ((==) `on` textVal gc)
                           $ sortBy (comparing (textVal gc)) sortedRows
            in concatMap (renderGroup displayCols ws gc numericOrigCols enrichCols) groups

      -- Grand total row
      grandTotalRow = buildGrandTotalRow displayCols numericOrigCols totalsMap

      title = maybe "ANALYTICS" id (metaTitle (tableMetadata origTable))
      footnote = buildFootnote numericOrigCols

  in  [ T.replicate 72 "═"
      , "TABLE 2 – SORTED & DERIVED DATA:  " <> title
      , T.replicate 72 "═"
      , "Columns: sorted ↓ by value  |  Δ = change from previous row  |"
      , "         Cumul = running total  |  % = share of grand total"
      , T.replicate 72 "─"
      , top
      , hdr
      , divider
      ]
   ++ tableBodyLines
   ++ [ divider
      , renderRow displayCols ws grandTotalRow
      , bot
      ]
   ++ footnote

-- | Build the ordered list of columns for one numeric column's group
buildColGroup :: Vector ColumnName -> ColumnName -> [ColumnName]
buildColGroup allCols base =
  let want = [ base
             , base <> " (Δ)"
             , base <> " (Cumulative)"
             , base <> " (%)"
             ]
  in filter (`V.elem` allCols) want

-- | Render a group of rows (same text-column value) + subtotal line
renderGroup
  :: Vector ColumnName
  -> Vector Int
  -> ColumnName          -- grouping column
  -> [ColumnName]        -- original numeric cols
  -> Vector ColumnName   -- all enriched cols
  -> [DataRow]
  -> [Text]
renderGroup displayCols ws gc numCols allEnrichedCols rows =
  let -- Sort within group by the first numeric col descending
      sortCol    = case numCols of
                     []    -> "__none__"
                     (c:_) -> case buildColGroup allEnrichedCols c of
                                []    -> c
                                (x:_) -> x
      sorted     = sortBy (comparing (Down . numVal sortCol)) rows
      dataLines  = map (renderRow displayCols ws) sorted
      subtotal   = buildSubtotalRow displayCols gc rows numCols
      subtotLine = renderSubtotalRow displayCols ws subtotal
  in dataLines ++ [subtotLine]

-- | Build a subtotal DataRow for a group
buildSubtotalRow
  :: Vector ColumnName
  -> ColumnName
  -> [DataRow]
  -> [ColumnName]
  -> DataRow
buildSubtotalRow displayCols gc rows numCols =
  let labelVal = case rows of
                   (r:_) -> case M.lookup gc r of
                               Just v  -> formatDataValue v <> " subtotal"
                               Nothing -> "Subtotal"
                   []    -> "Subtotal"
      baseMap  = M.singleton gc (TextValue labelVal)
      -- Sum only the plain numeric columns (not derived)
      sumsMap  = foldl (\acc col ->
                    let total = sum $ map (numVal col) rows
                    in M.insert col (NumericValue total) acc
                  ) baseMap numCols
  in sumsMap

-- | Render a subtotal row with visual emphasis
renderSubtotalRow :: Vector ColumnName -> Vector Int -> DataRow -> Text
renderSubtotalRow cols widths row =
  "│ " <>
  T.intercalate " │ "
    (V.toList $ V.zipWith3 (\col w _ ->
        let raw = case M.lookup col row of
                    Just v  -> formatDataValue v
                    Nothing -> "--"
            isNum = case M.lookup col row of
                      Just (NumericValue _) -> True
                      Just (IntegerValue _) -> True
                      _                    -> False
        in if isNum
             then T.justifyRight w ' ' raw
             else T.justifyLeft  w ' ' raw
      ) cols widths (V.replicate (V.length cols) ())
    ) <>
  " │"

-- | Build a grand-total row
buildGrandTotalRow
  :: Vector ColumnName
  -> [ColumnName]
  -> Map ColumnName DataValue
  -> DataRow
buildGrandTotalRow displayCols numCols totalsMap =
  let -- Start with "--" for every column
      base    = V.foldl' (\acc col -> M.insert col (TextValue "--") acc) M.empty displayCols
      -- Fill in totals for numeric cols
      withNum = foldl (\acc col ->
                  case M.lookup col totalsMap of
                    Just v  -> M.insert col v acc
                    Nothing -> acc
                ) base numCols
      -- Label the first non-numeric column "GRAND TOTAL"
      nonNumCols = V.toList $ V.filter (\c -> c `notElem` numCols) displayCols
      withLabel = case nonNumCols of
                    (fc:_) -> M.insert fc (TextValue "GRAND TOTAL") withNum
                    []     -> withNum
  in withLabel

-- | Footnotes explaining derived columns
buildFootnote :: [ColumnName] -> [Text]
buildFootnote [] = []
buildFootnote cols =
  [ ""
  , T.replicate 72 "─"
  , "FOOTNOTES"
  , T.replicate 72 "─"
  ] ++
  concatMap (\col ->
    [ "  " <> col <> " (Δ)          – Absolute change from the preceding row"
    , "  " <> col <> " (Cumulative) – Running total from first to current row"
    , "  " <> col <> " (%)          – This row's value as a % of the grand total"
    , "  " <> col <> " subtotal     – Sum of the group above this line"
    , ""
    ]
  ) cols ++
  [ "  GRAND TOTAL – Sum of all rows for each numeric column"
  , "  ↓ Rows sorted descending by primary numeric column value"
  , T.replicate 72 "─"
  ]

------------------------------------------------------------
-- Backward-compatible formatEnrichedTable (used by Markdown renderer)
------------------------------------------------------------

-- | Kept for callers that still use the old single-section format.
--   Now delegates to the new two-section layout.
formatEnrichedTable :: EnrichedTable -> [Text]
formatEnrichedTable enriched =
     formatRawTable (enrichedOriginal enriched)
  ++ [""]
  ++ formatAnalyticsTable enriched

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

isNumCol :: DataTable -> ColumnName -> Bool
isNumCol tbl col =
  case getNumericColumnFromTable col tbl of
    Right _ -> True
    Left _  -> False

-- | Extract numeric column without requiring DataModel import
getNumericColumnFromTable :: ColumnName -> DataTable -> Either () (Vector Double)
getNumericColumnFromTable colName table =
  let vals = V.mapMaybe (M.lookup colName) (tableRows table)
  in if V.null vals
       then Left ()
       else let nums = V.mapMaybe toNum vals
            in if V.length nums == V.length vals
                 then Right nums
                 else Left ()
  where
    toNum (NumericValue d) = Just d
    toNum (IntegerValue i) = Just (fromIntegral i)
    toNum _                = Nothing

numVal :: ColumnName -> DataRow -> Double
numVal col row =
  case M.lookup col row of
    Just (NumericValue d) -> d
    Just (IntegerValue i) -> fromIntegral i
    _                     -> 0.0

textVal :: ColumnName -> DataRow -> Text
textVal col row =
  case M.lookup col row of
    Just (TextValue t) -> t
    Just v             -> formatDataValue v
    Nothing            -> ""

-- | Format table row for export (CSV-style)
formatTableRow :: DataRow -> Text
formatTableRow row =
  T.intercalate "," $ map formatDataValue $ M.elems row

-- | Format a data value for display
formatDataValue :: DataValue -> Text
formatDataValue (TextValue t)   = t
formatDataValue (NumericValue d) = formatNumber d
formatDataValue (IntegerValue i) = T.pack $ show i
formatDataValue MissingValue     = "--"

-- | Format a number with appropriate precision
formatNumber :: Double -> Text
formatNumber d
  | d >= 1000000 = T.pack $ printf "%.1fM" (d / 1000000 :: Double)
  | d >= 1000    = T.pack $ printf "%.1fK" (d / 1000    :: Double)
  | abs d < 0.01 = T.pack $ printf "%.4f"  d
  | abs d < 1    = T.pack $ printf "%.2f"  d
  | otherwise    = T.pack $ printf "%.0f"  d

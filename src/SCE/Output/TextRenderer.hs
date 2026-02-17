{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.Output.TextRenderer
Description : Plain-text report rendering
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Output layout
─────────────
  1. Raw Data Table    – every original field, tabular, no derived columns
  2. Analytics Table   – sorted rows with Δ, cumulative, % of total,
                         subtotals per label group, grand-total row
  3. Charts Section    – one exhibit per chart, with legend and footnotes
  4. Report Footer     – summary counts
-}
module SCE.Output.TextRenderer
  ( renderToText
  , renderAnalysisReport
  , AnalysisReport(..)
  ) where

import SCE.Core.Types
import SCE.Table.Enrichment
import SCE.Table.Formatter
import SCE.Chart.Rejection

import Data.Text (Text)
import qualified Data.Text as T

------------------------------------------------------------
-- Report Type
------------------------------------------------------------

data AnalysisReport = AnalysisReport
  { reportTable    :: EnrichedTable
  , reportCharts   :: [Either RejectionReport [Text]]
  , reportMetadata :: TableMetadata
  }
  deriving stock (Show)

------------------------------------------------------------
-- Top-Level Rendering
------------------------------------------------------------

renderAnalysisReport :: AnalysisReport -> Text
renderAnalysisReport report =
  T.unlines $
       renderHeader (reportMetadata report)
    ++ [""]
    -- Section 1: Raw data
    ++ renderRawSection (reportTable report)
    ++ [""]
    -- Section 2: Sorted + derived analytics table
    ++ renderAnalyticsSection (reportTable report)
    -- Section 3: Charts — blank separator + content only when charts exist
    ++ renderChartsSectionWithSep (reportCharts report)
    ++ [""]
    -- Footer
    ++ renderFooter report

renderToText :: AnalysisReport -> Text
renderToText = renderAnalysisReport

------------------------------------------------------------
-- Report Header
------------------------------------------------------------

renderHeader :: TableMetadata -> [Text]
renderHeader meta =
  [ T.replicate 72 "═"
  , "  STATISTICAL ANALYSIS REPORT"
  , T.replicate 72 "═"
  , ""
  , "  " <> maybe "Untitled Analysis" id (metaTitle meta)
  , maybe "" (\s -> "  Source: " <> T.pack s) (metaSource meta)
  , ""
  ]

------------------------------------------------------------
-- Section 1 – Raw Data Table
------------------------------------------------------------

renderRawSection :: EnrichedTable -> [Text]
renderRawSection enriched =
  formatRawTable (enrichedOriginal enriched)

------------------------------------------------------------
-- Section 2 – Analytics Table
------------------------------------------------------------

renderAnalyticsSection :: EnrichedTable -> [Text]
renderAnalyticsSection enriched =
  formatAnalyticsTable enriched

------------------------------------------------------------
-- Section 3 – Charts
------------------------------------------------------------

-- | Prepend a blank separator only when there is something to show.
renderChartsSectionWithSep :: [Either RejectionReport [Text]] -> [Text]
renderChartsSectionWithSep [] = []
renderChartsSectionWithSep charts = [""] ++ renderChartsSection charts

-- | Render the charts section.
--   Empty list  → no --chart was given → omit the section entirely (no output).
--   Left entry  → chart was requested but could not be generated → show reason.
--   Right entry → chart generated successfully → show chart, legend, footnotes.
renderChartsSection :: [Either RejectionReport [Text]] -> [Text]
renderChartsSection [] = []   -- nothing requested, nothing shown
renderChartsSection charts =
  concatMap renderChart $ zip [1 :: Int ..] charts
  where
    -- Successfully generated chart
    renderChart (n, Right chartLines) =
      let exhibitTitle = extractChartTitle chartLines
      in   [ T.replicate 72 "═"
           , "EXHIBIT " <> T.pack (show n) <> "  –  " <> exhibitTitle
           , T.replicate 72 "═"
           , ""
           ]
        ++ chartLines
        ++ [""]
        ++ renderChartLegend exhibitTitle chartLines
        ++ renderChartFootnote n exhibitTitle chartLines
        ++ [""]

    -- Requested but not generated: show titled header + full reason
    renderChart (n, Left rejection) =
      let chartTypeName = T.pack (show $ rejectionChartType rejection)
      in   [ T.replicate 72 "═"
           , "EXHIBIT " <> T.pack (show n) <> "  –  " <> chartTypeName <> "  [NOT GENERATED]"
           , T.replicate 72 "═"
           , ""
           , formatRejectionReport rejection
           , ""
           ]

-- | Extract chart title from the lines produced by generateHeader.
--   The header structure is: ["====", "<title>", "====", ""]
--   so the title is the first non-separator, non-empty line.
extractChartTitle :: [Text] -> Text
extractChartTitle ls =
  let contentLines = filter (\l -> not (T.null l)
                                && not (T.isPrefixOf "=" l)) ls
  in case contentLines of
       (t:_) -> t
       []    -> "Chart"

-- | Detect whether a set of chart lines came from a dot plot.
--   Dot plot lines contain a '·' character; bar chart lines contain '='.
isDotPlot :: [Text] -> Bool
isDotPlot ls = any (T.isInfixOf "·") ls

-- | Footnote for a chart exhibit — wording differs for bar vs dot plot.
renderChartFootnote :: Int -> Text -> [Text] -> [Text]
renderChartFootnote n title chartLines
  | isDotPlot chartLines =
      [ "  [¹] Each · marker shows the exact position of the value on a"
      , "       scale spanning the minimum to the maximum in the dataset."
      , "  [²] No baseline truncation is possible — position is the only encoding."
      , "  [³] \"" <> title <> "\""
           <> "  –  Exhibit " <> T.pack (show n)
           <> " auto-generated as a dot plot (bar chart refused: tightly clustered data)."
      ]
  | otherwise =
      [ "  [¹] Bar lengths are proportional to value; each character represents"
      , "       an equal increment of the maximum displayed value."
      , "  [²] Values shown at the right of each bar are the actual data values."
      , "  [³] \"" <> title <> "\""
           <> "  –  Exhibit " <> T.pack (show n)
           <> " generated from the column selected via --value-column."
      ]

-- | Build a titled legend from chart lines: extract label → value pairs
renderChartLegend :: Text -> [Text] -> [Text]
renderChartLegend title chartLines =
  let barLines = filter (\l -> "|" `T.isInfixOf` l
                                && not (T.isPrefixOf "=" l)
                                && not (T.isPrefixOf "Bar" l)
                                && not (T.isPrefixOf "Scale" l)) chartLines
      entries  = map extractLegendEntry barLines
      validEntries = filter (not . T.null . fst) entries
  in if null validEntries
       then []
       else  [ T.replicate 72 "─"
             , "  LEGEND  –  " <> title
             , T.replicate 72 "─"
             , "  " <> T.justifyLeft 20 ' ' "Label" <> "  Value"
             , "  " <> T.replicate 34 "-"
             ]
          ++ map (\(lbl, val) ->
                "  " <> T.justifyLeft 20 ' ' lbl <> "  " <> val
              ) validEntries
          ++ [ T.replicate 72 "─" ]

-- | Pull label and value text from a bar line like: "Label     |===|  $value"
extractLegendEntry :: Text -> (Text, Text)
extractLegendEntry line =
  let (beforePipe, rest) = T.breakOn "|" line
      lbl   = T.strip beforePipe
      afterBars = T.dropWhile (\c -> c == '|' || c == '=' || c == '-' || c == ' ')
                    (T.drop 1 rest)
      val   = T.strip $ T.dropWhile (== '|') $ T.dropWhile (/= '|') afterBars
      -- Fallback: take text after the last '|'
      parts = T.splitOn "|" line
      valAlt = T.strip $ safeLast parts
  in (lbl, if T.null val then valAlt else val)
  where
    -- Safe version of last that returns empty text for empty list
    safeLast [] = ""
    safeLast [x] = x
    safeLast (_:xs) = safeLast xs

------------------------------------------------------------
-- Rejection formatter (local copy)
------------------------------------------------------------

formatRejectionReport :: RejectionReport -> Text
formatRejectionReport rep =
  T.unlines
    [ "  Chart Type:  " <> T.pack (show $ rejectionChartType rep)
    , ""
    , "  Reason:"
    , "    " <> rejectionDetails rep
    , ""
    , "  Rationale:"
    , "    " <> rejectionRationale rep
    , ""
    , "  Recommendations:"
    ]
  <> T.unlines (map ("    • " <>) (rejectionRecommendations rep))

------------------------------------------------------------
-- Report Footer
------------------------------------------------------------

renderFooter :: AnalysisReport -> [Text]
renderFooter report =
  let allCharts   = reportCharts report
      generated   = length $ filter isRight allCharts
      rejected    = length $ filter isLeft  allCharts
  in  [ T.replicate 72 "═"
      , "  REPORT SUMMARY"
      , T.replicate 72 "═"
      , "  Tables Generated:       2  (Table 1 – Raw Data, Table 2 – Analytics)"
      , "  Exhibits Generated:     " <> T.pack (show generated)
      , "  Exhibits Not Generated: " <> T.pack (show rejected)
      , "  Enrichments Applied:    " <> T.pack (show $ length $ enrichedApplied $ reportTable report)
      , "      Δ (difference)  |  Cumulative  |  % of Total  |"
      , "      Subtotals per group  |  Grand Total"
      , ""
      , "  Generated by Statistical Charting Engine v1.0"
      , T.replicate 72 "═"
      ]
  where
    isLeft  (Left _)  = True
    isLeft  _         = False
    isRight (Right _) = True
    isRight _         = False

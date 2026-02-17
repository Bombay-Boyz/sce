{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module SCE.Chart.Rejection
  ( -- * Rejection Handling
    generateRejectionReport
  , RejectionReport(..)
  ) where

import SCE.Core.Types
import Data.Text (Text)
import qualified Data.Text as T

------------------------------------------------------------
-- Rejection Report Type
------------------------------------------------------------

data RejectionReport = RejectionReport
  { rejectionChartType       :: ChartType
  , rejectionReason          :: RejectionReason
  , rejectionDetails         :: Text
  , rejectionRationale       :: Text
  , rejectionRecommendations :: [Text]
  }
  deriving stock (Show, Eq)

------------------------------------------------------------
-- Report Generation
------------------------------------------------------------

generateRejectionReport :: ChartType -> RejectionReason -> RejectionReport
generateRejectionReport ct reason =
  case reason of

    TooManyCategoriesRejection actual maxAllowed ->
      RejectionReport
        { rejectionChartType = ct
        , rejectionReason = reason
        , rejectionDetails = T.pack $
            "The dataset contains " ++ show actual ++
            " categories, exceeding the maximum limit of " ++
            show maxAllowed ++ " categories for " ++
            show ct ++ "."
        , rejectionRationale =
            "Charts with more than "
              <> T.pack (show maxAllowed)
              <> " categories become unreadable and defeat the purpose of "
              <> "visual representation. Statistical exhibits should highlight "
              <> "key patterns, not overwhelm with exhaustive detail."
        , rejectionRecommendations =
            [ "Group related categories to reduce count"
            , "Show top N categories + \"Other\" aggregate"
            , "Use summary statistics in table form"
            , "Split into multiple exhibits by subcategory"
            ]
        }

    InsufficientDataRejection actual minRequired ->
      RejectionReport
        { rejectionChartType = ct
        , rejectionReason = reason
        , rejectionDetails =
            "Statistical visualizations require sufficient data points to "
              <> "reveal patterns and trends. With too few points, the chart "
              <> "provides no advantage over a simple table."
        , rejectionRationale = T.pack $
            "The dataset contains only " ++ show actual ++
            " data points, which is insufficient for meaningful " ++
            show ct ++ " visualization (minimum " ++
            show minRequired ++ " required)."
        , rejectionRecommendations =
            [ "Collect more data points"
            , "Use a table format instead"
            , "Consider a different chart type with lower requirements"
            ]
        }

    InvalidScaleRejection scale ct' ->
      RejectionReport
        { rejectionChartType = ct'
        , rejectionReason = reason
        , rejectionDetails = T.pack $
            "The data has " ++ show scale ++
            " measurement scale, which is incompatible with "
            ++ show ct' ++ "."
        , rejectionRationale =
            "Each chart type has specific measurement scale requirements. "
              <> T.pack (show ct')
              <> " requires at least "
              <> requiredScaleText ct'
              <> " scale data."
        , rejectionRecommendations =
            [ "Use a chart type appropriate for "
                <> T.pack (show scale) <> " data"
            , "Transform data to appropriate scale if statistically valid"
            , "Present data in table format"
            ]
        }

    DataQualityRejection msg ->
      RejectionReport
        { rejectionChartType = ct
        , rejectionReason = reason
        , rejectionDetails = msg
        , rejectionRationale =
            "Data quality issues prevent reliable visualization. Charts "
              <> "should only be generated from clean, validated data."
        , rejectionRecommendations =
            [ "Review and clean the source data"
            , "Check for missing or invalid values"
            , "Verify data types are correct"
            ]
        }

    TightlyClusteredRejection minV maxV spreadRatio ->
      RejectionReport
        { rejectionChartType = ct
        , rejectionReason    = reason
        , rejectionDetails   =
            "Values range from "
              <> fmtNum minV <> " to " <> fmtNum maxV
              <> ", a spread of only "
              <> T.pack (show (round (spreadRatio * 100) :: Int))
              <> "% of the maximum value. "
              <> "Bar lengths would differ by less than "
              <> T.pack (show (round (spreadRatio * 100) :: Int))
              <> "% visually, making the chart misleading."
        , rejectionRationale =
            "Bar charts encode value as physical length. When all bars are "
              <> "85–99% the same length, the eye cannot distinguish meaningful "
              <> "differences from noise. The chart would either require a "
              <> "truncated (non-zero) baseline to look useful — which is the "
              <> "most common form of statistical manipulation — or it would "
              <> "appear as nearly identical bars that convey nothing. "
              <> "The table below already communicates this data accurately."
        , rejectionRecommendations =
            [ "Use the Analytics Table (Table 2) — it shows exact values, "
                <> "differences (Δ), and percentages which are more informative "
                <> "than a bar chart for tightly clustered data"
            , "Consider a dot plot or range chart if visual comparison is needed"
            , "If the relative differences matter, use the % column in Table 2"
            , "If absolute values matter, the raw numbers in Table 1 are clearest"
            ]
        }
  where
    fmtNum :: Double -> Text
    fmtNum v
      | v >= 1000000 = T.pack (show (round (v / 1000000) :: Int)) <> "M"
      | v >= 1000    = T.pack (show (round (v / 1000)    :: Int)) <> "K"
      | otherwise    = T.pack (show (round v             :: Int))

------------------------------------------------------------
-- Formatting
------------------------------------------------------------

requiredScaleText :: ChartType -> Text
requiredScaleText HorizontalBarChart = "Nominal"
requiredScaleText VerticalBarChart   = "Nominal"
requiredScaleText LineChart          = "Ordinal"
requiredScaleText ScatterPlot        = "Interval"
requiredScaleText DotPlot            = "Nominal"
requiredScaleText Histogram          = "Interval"
requiredScaleText BoxPlot            = "Interval"
requiredScaleText ConfidenceInterval = "Interval"

formatRejectionReport :: RejectionReport -> Text
formatRejectionReport report =
  T.unlines $
      [ T.replicate 68 "="
      , "EXHIBIT REJECTED: "
          <> T.pack (show $ rejectionChartType report)
      , T.replicate 68 "="
      , ""
      , "Chart Type: "
          <> T.pack (show $ rejectionChartType report)
      , "Rejection Code: "
          <> rejectionCode (rejectionReason report)
      , ""
      , "Reason:"
      , "  " <> rejectionDetails report
      , ""
      , "Rationale:"
      , "  " <> rejectionRationale report
      , ""
      , "Recommendations:"
      ]
      <> map ("  • " <>) (rejectionRecommendations report)
      <> [ ""
         , "Alternative:"
         , "  TABLE (below) presents all data in readable form."
         , ""
         , T.replicate 68 "="
         ]

------------------------------------------------------------
-- Codes
------------------------------------------------------------

-- | Generate a code for rejection reason (currently unused but kept for future API)
{-# WARNING rejectionCode "rejectionCode is currently unused but kept for API compatibility" #-}
rejectionCode :: RejectionReason -> Text
rejectionCode (TooManyCategoriesRejection _ _)    = "TOO_MANY_CATEGORIES"
rejectionCode (InsufficientDataRejection _ _)     = "INSUFFICIENT_DATA"
rejectionCode (InvalidScaleRejection _ _)         = "INVALID_SCALE"
rejectionCode (DataQualityRejection _)            = "DATA_QUALITY"
rejectionCode (TightlyClusteredRejection _ _ _)   = "TIGHTLY_CLUSTERED"

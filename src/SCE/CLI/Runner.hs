{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module SCE.CLI.Runner
  ( runCLI
  , runAnalysis
  ) where

import SCE.Core.Types
  ( ChartConfig(..)
  , def
  , SCEResult
  , DetailedError
  , formatError
  , formatErrorShort
  , DataTable(..)
  , DataValue(..)
  , OutputFormat(..)
  , ChartType(..)
  , RejectionReason(..)
  , MeasurementScale(..)
  , ColumnName
  )
import SCE.Core.DataModel
import SCE.Table.Enrichment
import SCE.Chart.BarChart
import SCE.Chart.ScatterPlot
import SCE.Chart.DotChart
import SCE.Chart.Histogram
import SCE.Chart.BoxPlot
import SCE.Chart.ConfidenceInterval
import SCE.Chart.LineChart
import SCE.Chart.Inference
import SCE.Chart.Admissibility
import SCE.Chart.Rejection
import SCE.Output.TextRenderer
import SCE.Output.MarkdownRenderer
import SCE.Output.DocxRenderer
import SCE.Validation.Validator
import SCE.CLI.Parser
import SCE.CLI.Interactive

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import System.FilePath (takeBaseName, normalise, takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromMaybe)

import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------

-- | Extract a column as text values
getTextColumn :: ColumnName -> DataTable -> SCEResult (Vector Text)
getTextColumn colName table = do
  col <- getColumn colName table
  return $ V.map dataValueToText col
  where
    dataValueToText :: DataValue -> Text
    dataValueToText (TextValue t) = t
    dataValueToText (NumericValue d) = T.pack $ show d
    dataValueToText (IntegerValue i) = T.pack $ show i
    dataValueToText MissingValue = ""

-- | Infer unit from column name based on common patterns
inferUnitFromColumnName :: Text -> Maybe Text
inferUnitFromColumnName colName =
  let lower = T.toLower colName
  in case () of
       _ | T.isInfixOf "usd" lower || T.isInfixOf "dollar" lower -> Just "USD"
         | T.isInfixOf "eur" lower || T.isInfixOf "euro" lower -> Just "EUR"
         | T.isInfixOf "gbp" lower || T.isInfixOf "pound" lower -> Just "GBP"
         | T.isInfixOf "price" lower || T.isInfixOf "cost" lower || 
           T.isInfixOf "revenue" lower || T.isInfixOf "sales" lower -> Just "$"
         | T.isInfixOf "percent" lower || T.isInfixOf "pct" lower || 
           T.isInfixOf "rate" lower -> Just "%"
         | T.isInfixOf "temp" lower || T.isInfixOf "celsius" lower -> Just "°C"
         | T.isInfixOf "fahrenheit" lower -> Just "°F"
         | T.isInfixOf "meter" lower || T.isInfixOf "metre" lower -> Just "m"
         | T.isInfixOf "kilogram" lower || T.isInfixOf "kg" lower -> Just "kg"
         | T.isInfixOf "hour" lower -> Just "hrs"
         | T.isInfixOf "count" lower || T.isInfixOf "quantity" lower -> Just "units"
         | otherwise -> Nothing



------------------------------------------------------------
-- CLI Entry
------------------------------------------------------------

runCLI :: CLIConfig -> IO ()
runCLI config
  | cliShowHelp config = printUsage >> exitSuccess
  | null (cliInputFile config) = do
      TIO.hPutStrLn stderr "Error: No input file specified"
      printUsage
      exitFailure
  | otherwise = do
      result <- runAnalysis config
      case result of
        Left err -> do
          TIO.hPutStrLn stderr $ formatError err
          exitFailure
        Right output -> do
          case cliOutputFile config of
            Just outFile -> do
              -- Validate and normalize output path
              let normalizedPath = normalise outFile
              -- Create parent directory if it doesn't exist
              let parentDir = takeDirectory normalizedPath
              createDirectoryIfMissing True parentDir
              -- Write file
              TIO.writeFile normalizedPath output
            Nothing      -> TIO.putStrLn output
          exitSuccess

------------------------------------------------------------
-- Analysis Pipeline
------------------------------------------------------------

runAnalysis :: CLIConfig -> IO (SCEResult Text)
runAnalysis config = do
  parseResult <- parseCSVFile (cliInputFile config)
  case parseResult of
    Left err -> return $ Left err
    Right (columns, rows) -> do

      -- Use --title if given, otherwise derive from the filename
      let derivedTitle =
            case cliTitle config of
              Just t  -> Just t
              Nothing -> Just $ T.pack $ takeBaseName (cliInputFile config)

      let tableResult =
            buildDataTable
              derivedTitle
              (Just $ cliInputFile config)
              columns
              rows

      case tableResult of
        Left err -> return $ Left err
        Right table -> do

          case validateDataTable table of
            Left err -> return $ Left err
            Right () -> do

              case enrichTable table of
                Left err -> return $ Left err
                Right enriched -> do

                  charts <- generateCharts config table

                  let report = AnalysisReport
                        { reportTable    = enriched
                        , reportCharts   = charts
                        , reportMetadata = tableMetadata table
                        }

                  let output =
                        case cliOutputFormat config of
                          PlainText -> renderToText report
                          Markdown  -> renderToMarkdown report
                          DocX      -> "DOCX format not yet implemented"

                  return $ Right output

------------------------------------------------------------
-- Chart Generation
------------------------------------------------------------

generateCharts
  :: CLIConfig
  -> DataTable
  -> IO [Either RejectionReport [Text]]
generateCharts config table = do
  -- INTERACTIVE MODE: Always use interactive selection
  selectedChartType <- runInteractiveSelection table
  
  -- Generate the selected chart
  generateChartByType selectedChartType config table

-- | Generate a specific chart type
generateChartByType
  :: ChartType
  -> CLIConfig
  -> DataTable
  -> IO [Either RejectionReport [Text]]
generateChartByType chartType config table = do

      -- ══════════════════════════════════════════════════════════
      -- CONFIDENCE INTERVAL - Time-series with uncertainty
      -- ══════════════════════════════════════════════════════════
      if chartType == ConfidenceInterval
        then do
          -- Try to detect CI columns
          case detectConfidenceIntervalColumns (tableColumns table) of
            Nothing ->
              let rejection = RejectionReport
                    { rejectionChartType = chartType
                    , rejectionReason = DataQualityRejection "Could not detect confidence interval columns"
                    , rejectionDetails =
                        "Confidence interval charts require: " <>
                        "Label column (date/time/period), " <>
                        "Estimate column (value/mean/estimate), " <>
                        "Lower bound column (lower/ci_lower), " <>
                        "Upper bound column (upper/ci_upper)"
                    , rejectionRationale =
                        "The dataset must have columns that represent time periods, " <>
                        "point estimates, and confidence bounds."
                    , rejectionRecommendations =
                        [ "Ensure your CSV has columns like: Year, Estimate, Lower, Upper"
                        , "Or: Date, Value, CI_Lower, CI_Upper"
                        , "Available columns: " <> T.intercalate ", " (V.toList $ tableColumns table)
                        ]
                    }
              in return [Left rejection]
            
            Just (labelCol, estCol, lowerCol, upperCol) -> do
              -- Extract columns
              case (getTextColumn labelCol table, getNumericColumn estCol table,
                    getNumericColumn lowerCol table, getNumericColumn upperCol table) of
                (Right labels, Right estimates, Right lowers, Right uppers) -> do
                  -- Build confidence interval points
                  let points = V.zipWith4 (\lbl est low upp ->
                        ConfidencePoint
                          { cpLabel = lbl
                          , cpEstimate = est
                          , cpLower = low
                          , cpUpper = upp
                          , cpInnerLower = Nothing
                          , cpInnerUpper = Nothing
                          }) labels estimates lowers uppers
                  
                  let ciData = ConfidenceIntervalData
                        { ciPoints = points
                        , ciTitle = cliTitle config
                        , ciYAxisLabel = fromMaybe estCol (cliTitle config)
                        , ciConfig = def { chartType = ConfidenceInterval }
                        , ciConfidenceLevel = 0.95  -- Default 95%
                        }
                  
                  case generateConfidenceInterval ciData of
                    Right chartLines -> return [Right chartLines]
                    Left err ->
                      return [Left $ generateRejectionReport chartType
                                       (DataQualityRejection $ formatErrorShort err)]
                
                _ ->
                  let rejection = RejectionReport
                        { rejectionChartType = chartType
                        , rejectionReason = DataQualityRejection "Could not read confidence interval data"
                        , rejectionDetails =
                            "Failed to read numeric data from detected columns. " <>
                            "Ensure " <> estCol <> ", " <> lowerCol <> ", " <> upperCol <>
                            " contain numeric values."
                        , rejectionRationale =
                            "Confidence intervals require numeric data for estimates and bounds."
                        , rejectionRecommendations =
                            [ "Check that all value columns contain numbers only"
                            , "Remove any text or empty cells from numeric columns"
                            ]
                        }
                  in return [Left rejection]

      -- ══════════════════════════════════════════════════════════
      -- SCATTER PLOT - Handle separately (needs X and Y columns)
      -- ══════════════════════════════════════════════════════════
      else if chartType == ScatterPlot
        then do
          -- Get X column (prompt if not provided)
          xCol <- case cliXColumn config of
            Just col -> return col
            Nothing -> promptForColumn (tableColumns table) "X-axis column for scatter plot"
          
          -- Get Y column (prompt if not provided)
          yCol <- case cliYColumn config of
            Just col -> return col
            Nothing -> promptForColumn (tableColumns table) "Y-axis column for scatter plot"
          
          case (getNumericColumn xCol table, getNumericColumn yCol table) of
            (Right xVals, Right yVals) -> do
              let scatterPoints = V.zipWith 
                    (\x y -> ScatterPoint x y Nothing)
                    xVals yVals
              let chartConfig = def { chartType = ScatterPlot }
              let scatterData = ScatterPlotData
                    { scatterPoints = scatterPoints
                    , scatterXAxisLabel = xCol
                    , scatterYAxisLabel = yCol
                    , scatterTitle = cliTitle config
                    , scatterConfig = chartConfig
                    , scatterXScale = Ratio
                    , scatterYScale = Ratio
                    }
              case generateScatterPlot scatterData of
                Right chartLines -> return [Right chartLines]
                Left err ->
                  return [Left $ generateRejectionReport chartType
                                   (DataQualityRejection $ formatErrorShort err)]
            (Left err, _) ->
              return [Left $ generateRejectionReport chartType
                               (DataQualityRejection $ 
                                 "X-axis column error: " <> formatErrorShort err)]
            (_, Left err) ->
              return [Left $ generateRejectionReport chartType
                               (DataQualityRejection $ 
                                 "Y-axis column error: " <> formatErrorShort err)]

      -- ══════════════════════════════════════════════════════════
      -- HISTOGRAM - Handle separately (needs continuous data)
      -- ══════════════════════════════════════════════════════════
        else if chartType == Histogram
        then do
          -- Get value column (prompt if not provided or doesn't exist)
          valueCol <- case cliValueColumn config of
            configCol -> do
              case getNumericColumn configCol table of
                Right _ -> return configCol
                Left _ -> do
                  TIO.putStrLn ""
                  TIO.putStrLn $ "Column '" <> configCol <> "' not found or not numeric."
                  promptForColumn (tableColumns table) "numeric value column for histogram"

          -- Build histogram title
          let histTitleText =
                case cliTitle config of
                  Just t  -> t
                  Nothing -> "Distribution of " <> valueCol

          case getNumericColumn valueCol table of
            Left colErr ->
              let rejection = RejectionReport
                    { rejectionChartType       = chartType
                    , rejectionReason          = DataQualityRejection (T.pack $ show colErr)
                    , rejectionDetails         =
                        "The value column \"" <> valueCol <> "\" could not be read as numeric data. "
                        <> "Histograms require continuous quantitative data."
                    , rejectionRationale       =
                        "Histograms display the distribution of a single continuous variable. "
                        <> "The data must be numeric (interval or ratio scale)."
                    , rejectionRecommendations =
                        [ "Check that --value-column matches a column name in the CSV header"
                        , "Verify the column contains numeric values (no text, no blanks)"
                        , "Ensure you have continuous data, not categories"
                        , "Available columns: " <> T.intercalate ", " (V.toList $ tableColumns table)
                        ]
                    }
              in return [Left rejection]

            Right values -> do
              let valueList = V.toList values
              let scale = Ratio  -- Assume ratio scale for continuous data
              let admissibility = checkAdmissibility chartType scale (V.length values) valueList

              case admissibility of
                Admissible -> do
                  let histData = HistogramData
                        { histValues = values
                        , histTitle = Just histTitleText
                        , histXAxisLabel = valueCol
                        , histConfig = def { chartType = Histogram }
                        , histScale = scale
                        , histBinningMethod = FreedmanDiaconis  -- Default to robust method
                        }
                  case generateHistogram histData of
                    Right chartLines -> return [Right chartLines]
                    Left err ->
                      return [Left $ generateRejectionReport chartType
                                       (DataQualityRejection $ formatErrorShort err)]

                NotAdmissible reason ->
                  return [Left $ generateRejectionReport chartType reason]

      -- ══════════════════════════════════════════════════════════
      -- BOX PLOT - Handle separately (needs continuous data)
      -- ══════════════════════════════════════════════════════════
        else if chartType == BoxPlot
        then do
          -- Get value column (prompt if not provided or doesn't exist)
          valueCol <- case cliValueColumn config of
            configCol -> do
              case getNumericColumn configCol table of
                Right _ -> return configCol
                Left _ -> do
                  TIO.putStrLn ""
                  TIO.putStrLn $ "Column '" <> configCol <> "' not found or not numeric."
                  promptForColumn (tableColumns table) "numeric value column for box plot"

          -- Build box plot title
          let boxTitleText =
                case cliTitle config of
                  Just t  -> t
                  Nothing -> "Distribution of " <> valueCol

          case getNumericColumn valueCol table of
            Left colErr ->
              let rejection = RejectionReport
                    { rejectionChartType       = chartType
                    , rejectionReason          = DataQualityRejection (T.pack $ show colErr)
                    , rejectionDetails         =
                        "The value column \"" <> valueCol <> "\" could not be read as numeric data. "
                        <> "Box plots require continuous quantitative data."
                    , rejectionRationale       =
                        "Box plots display the five-number summary of a continuous variable. "
                        <> "The data must be numeric (interval or ratio scale)."
                    , rejectionRecommendations =
                        [ "Check that --value-column matches a column name in the CSV header"
                        , "Verify the column contains numeric values (no text, no blanks)"
                        , "Ensure you have continuous data, not categories"
                        , "Available columns: " <> T.intercalate ", " (V.toList $ tableColumns table)
                        ]
                    }
              in return [Left rejection]

            Right values -> do
              let valueList = V.toList values
              let scale = Ratio  -- Assume ratio scale for continuous data
              let admissibility = checkAdmissibility chartType scale (V.length values) valueList

              case admissibility of
                Admissible -> do
                  let boxData = BoxPlotData
                        { boxValues = values
                        , boxTitle = Just boxTitleText
                        , boxAxisLabel = valueCol
                        , boxConfig = def { chartType = BoxPlot }
                        , boxScale = scale
                        }
                  case generateBoxPlot boxData of
                    Right chartLines -> return [Right chartLines]
                    Left err ->
                      return [Left $ generateRejectionReport chartType
                                       (DataQualityRejection $ formatErrorShort err)]

                NotAdmissible reason ->
                  return [Left $ generateRejectionReport chartType reason]

      -- ══════════════════════════════════════════════════════════
      -- LINE CHART - Time-series or ordered data
      -- ══════════════════════════════════════════════════════════
        else if chartType == LineChart
          then do
            -- Get label column (prompt if not provided)
            labelCol <- case cliLabelColumn config of
              Just col -> return col
              Nothing  -> case V.uncons (tableColumns table) of
                Just (firstCol, _) -> return firstCol
                Nothing -> do
                  TIO.putStrLn "Error: No columns found in table"
                  return "Label"
            
            -- Get value column (prompt if not provided or doesn't exist)
            valueCol <- case cliValueColumn config of
              configCol -> do
                case getNumericColumn configCol table of
                  Right _ -> return configCol
                  Left _ -> do
                    TIO.putStrLn ""
                    TIO.putStrLn $ "Column '" <> configCol <> "' not found or not numeric."
                    promptForColumn (tableColumns table) "numeric value column for line chart"
            
            -- Build title
            let chartTitleText =
                  case cliTitle config of
                    Just t  -> t
                    Nothing -> valueCol <> " over " <> labelCol
            
            case (getTextColumn labelCol table, getNumericColumn valueCol table) of
              (Right labels, Right values) -> do
                -- Build line points
                let points = V.zipWith (\lbl val ->
                      LinePoint
                        { pointLabel = lbl
                        , pointValue = val
                        }) labels values
                
                -- Get measurement scale for the value column
                let scale = M.findWithDefault Ratio valueCol (tableScales table)
                
                let lineData = LineChartData
                      { linePoints = points
                      , lineConfig = def { chartType = LineChart }
                      , lineScale = scale
                      , lineUnit = inferUnitFromColumnName valueCol
                      , lineTitle = Just chartTitleText
                      , lineYLabel = Just valueCol
                      , lineXLabel = Just labelCol
                      }
                
                case generateLineChart lineData of
                  Right chartLines -> return [Right chartLines]
                  Left err ->
                    return [Left $ generateRejectionReport chartType
                                     (DataQualityRejection $ formatErrorShort err)]
              
              _ ->
                let rejection = RejectionReport
                      { rejectionChartType = chartType
                      , rejectionReason = DataQualityRejection "Could not read line chart data"
                      , rejectionDetails =
                          "Failed to read data for line chart. Need label column and numeric value column."
                      , rejectionRationale =
                          "Line charts require a label/time column and a numeric value column."
                      , rejectionRecommendations =
                          [ "Use --label-column to specify the X-axis (time/category) column"
                          , "Use --value-column to specify the Y-axis (numeric) column"
                          , "Available columns: " <> T.intercalate ", " (V.toList $ tableColumns table)
                          ]
                      }
                in return [Left rejection]

      -- ══════════════════════════════════════════════════════════
      -- BAR CHARTS AND DOT PLOTS (use value + label columns)
      -- ══════════════════════════════════════════════════════════
        else do
          -- Get label column (prompt if not provided or use first column)
          labelCol <- case cliLabelColumn config of
            Just col -> return col
            Nothing  -> case V.uncons (tableColumns table) of
              Just (firstCol, _) -> return firstCol
              Nothing -> do
                TIO.putStrLn "Error: No columns found in table"
                return "Label"  -- Fallback, will fail gracefully later
          
          -- Get value column (prompt if not provided or doesn't exist)
          valueCol <- case cliValueColumn config of
            configCol -> do
              -- Check if the column exists in the table
              case getNumericColumn configCol table of
                Right _ -> return configCol  -- Column exists and is numeric
                Left _ -> do
                  -- Column doesn't exist or isn't numeric, prompt user
                  TIO.putStrLn ""
                  TIO.putStrLn $ "Column '" <> configCol <> "' not found or not numeric."
                  promptForColumn (tableColumns table) "numeric value column for the chart"

          -- Build a descriptive chart title from report title + value column
          let chartTitleText =
                case cliTitle config of
                  Just t  -> t <> "  –  " <> valueCol
                  Nothing -> valueCol <> " by " <> labelCol

          case (getNumericColumn valueCol table, getColumn labelCol table) of

            -- Value column not found or not numeric
            (Left colErr, _) ->
              let rejection = RejectionReport
                    { rejectionChartType       = chartType
                    , rejectionReason          = DataQualityRejection (T.pack $ show colErr)
                    , rejectionDetails         =
                        "The value column \"" <> valueCol <> "\" could not be read as numeric data. "
                        <> "Chart generation requires a numeric column for bar lengths."
                    , rejectionRationale       =
                        "Charts plot numeric quantities. A column that contains text "
                        <> "or is missing from the dataset cannot be visualised."
                    , rejectionRecommendations =
                        [ "Check that --value-column matches a column name in the CSV header"
                        , "Verify the column contains numeric values (no text, no blanks)"
                        , "Available columns: " <> T.intercalate ", " (V.toList $ tableColumns table)
                        ]
                    }
              in return [Left rejection]

            -- Label column not found
            (_, Left colErr) ->
              let rejection = RejectionReport
                    { rejectionChartType       = chartType
                    , rejectionReason          = DataQualityRejection (T.pack $ show colErr)
                    , rejectionDetails         =
                        "The label column \"" <> labelCol <> "\" was not found in the dataset. "
                        <> "Chart generation requires a column to use as labels."
                    , rejectionRationale       =
                        "Each data point in the chart needs a label drawn from a data column. "
                        <> "Without a valid label column the chart cannot be rendered."
                    , rejectionRecommendations =
                        [ "Use --label-column to specify which column to use for labels"
                        , "Available columns: " <> T.intercalate ", " (V.toList $ tableColumns table)
                        ]
                    }
              in return [Left rejection]

            (Right values, Right labels) -> do

              let items =
                    V.zipWith
                      (\lbl val ->
                        case lbl of
                          TextValue t    -> BarItem t val
                          IntegerValue i -> BarItem (T.pack $ show i) val
                          NumericValue d -> BarItem (T.pack $ show d) val
                          MissingValue   -> BarItem "N/A" val
                      )
                      labels
                      values

              let chartConfig = def { chartType = chartType }
              
              -- Infer unit from column name if it contains common currency/unit terms
              let inferredUnit = inferUnitFromColumnName valueCol
              
              -- Get the actual measurement scale for the value column from the table
              let inferredScale = M.findWithDefault Ratio valueCol (tableScales table)
              
              let chartData =
                    BarChartData
                      { barItems  = items
                      , barConfig = chartConfig
                      , barScale  = inferredScale
                      , barUnit   = inferredUnit
                      , barTitle  = Just chartTitleText
                      }

              let valueList    = V.toList values
              let admissibility =
                    checkAdmissibility chartType inferredScale (V.length items) valueList

              case admissibility of

                -- ── Admissible: generate the requested chart type ────────────
                Admissible ->
                  case chartType of
                    DotPlot ->
                      case generateDotPlot chartData of
                        Right chartLines -> return [Right chartLines]
                        Left err ->
                          return [Left $ generateRejectionReport chartType
                                           (DataQualityRejection $ formatErrorShort err)]

                    _ ->
                      case generateHorizontalBarChart chartData of
                        Right chartLines -> return [Right chartLines]
                        Left err ->
                          return [Left $ generateRejectionReport chartType
                                           (DataQualityRejection $ formatErrorShort err)]

                -- ── Tightly clustered: auto-fallback to dot plot ─────────────
                NotAdmissible reason@(TightlyClusteredRejection _ _ _)
                  | chartType `elem` [HorizontalBarChart, VerticalBarChart] ->

                    let barRejection = generateRejectionReport chartType reason
                        dotConfig    = chartConfig { chartType = DotPlot }
                        dotTitle     = chartTitleText <> "  [Dot Plot]"
                        dotData      = chartData
                                         { barConfig = dotConfig
                                         , barTitle  = Just dotTitle
                                         }
                        dotAdmiss    = checkDotPlotAdmissibility inferredScale (V.length items)
                    in case dotAdmiss of
                         Admissible ->
                           case generateDotPlot dotData of
                             Right dotLines ->
                               return [ Left barRejection   -- exhibit 1: why bar was refused
                                      , Right dotLines      -- exhibit 2: dot plot instead
                                      ]
                             Left err ->
                               return [ Left barRejection
                                      , Left $ generateRejectionReport DotPlot
                                                 (DataQualityRejection $ formatErrorShort err)
                                      ]
                         NotAdmissible dotReason ->
                           return [ Left barRejection
                                  , Left $ generateRejectionReport DotPlot dotReason
                                  ]

                -- ── Any other rejection ──────────────────────────────────────
                NotAdmissible reason ->
                  return [Left $ generateRejectionReport chartType reason]
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.CLI.Interactive
Description : Interactive chart selection interface
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Provides an interactive command-line interface for chart selection with
ranked recommendations, explanations, and comparison features.
-}
module SCE.CLI.Interactive
  ( -- * Interactive Selection
    runInteractiveSelection
  , promptForColumn
  , displayRankings
  , displayComparison
    -- * User Input
  , getUserSelection
  , getUserConfirmation
  ) where

import SCE.Core.Types
import SCE.Core.DataModel
import SCE.Chart.Inference
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Char (toLower, toUpper)
import Data.List (intercalate, (!?))

------------------------------------------------------------
-- Interactive Selection
------------------------------------------------------------

-- | Run the interactive chart selection process
runInteractiveSelection :: DataTable -> IO ChartType
runInteractiveSelection table = do
  let recommendations = evaluateAllCharts table
  
  putStrLn ""
  putStrLn $ T.unpack $ T.replicate 70 "="
  putStrLn "RECOMMENDED VISUALIZATIONS"
  putStrLn $ T.unpack $ T.replicate 70 "="
  putStrLn ""
  
  displayRankings recommendations
  
  putStrLn ""
  choice <- getUserSelection (allEvaluations recommendations)
  
  case choice of
    ChartSelection ct -> return ct
    CompareCharts indices -> do
      displayComparisonFor recommendations indices
      runInteractiveSelection table  -- Loop back to selection
    ShowAll -> do
      displayAllDetails recommendations
      runInteractiveSelection table
    Help -> do
      displayHelp
      runInteractiveSelection table

data UserChoice
  = ChartSelection ChartType
  | CompareCharts [Int]
  | ShowAll
  | Help
  deriving (Show, Eq)

-- | Display ranked recommendations
displayRankings :: ChartRecommendations -> IO ()
displayRankings recs = do
  let primary = primaryRecommendation recs
  let secondary = secondaryRecommendations recs
  let viable = viableAlternatives recs
  
  -- Display primary recommendation
  displayRecommendation 1 primary True
  
  -- Display secondary recommendations
  mapM_ (\(idx, rec) -> displayRecommendation idx rec False) $
    zip [2..] secondary
  
  -- Display viable alternatives
  when (not $ null viable) $ do
    putStrLn ""
    putStrLn "OTHER OPTIONS:"
    mapM_ (\(idx, rec) -> displayRecommendationCompact idx rec) $
      zip [length secondary + 2..] viable
  
  where
    when True action = action
    when False _ = return ()

displayRecommendation :: Int -> ChartRecommendation -> Bool -> IO ()
displayRecommendation idx rec isPrimary = do
  let stars = confidenceToStars (recConfidence rec)
  let pct = floor (recConfidence rec * 100) :: Int
  let chartName = chartTypeName (recChartType rec)
  
  if isPrimary
    then do
      putStrLn $ printf "[%d] %s %s (%d%%) ★ RECOMMENDED" idx stars chartName pct
      putStrLn $ "    " ++ T.unpack (recRationale rec)
    else do
      putStrLn ""
      putStrLn $ printf "[%d] %s %s (%d%%)" idx stars chartName pct
      putStrLn $ "    " ++ T.unpack (recRationale rec)
  
  putStrLn ""

displayRecommendationCompact :: Int -> ChartRecommendation -> IO ()
displayRecommendationCompact idx rec = do
  let stars = confidenceToStars (recConfidence rec)
  let pct = floor (recConfidence rec * 100) :: Int
  let chartName = chartTypeName (recChartType rec)
  putStrLn $ printf "[%d] %s %s (%d%%)" idx stars chartName pct

confidenceToStars :: Double -> String
confidenceToStars conf
  | conf >= 0.90 = "★★★★★"
  | conf >= 0.80 = "★★★★☆"
  | conf >= 0.70 = "★★★☆☆"
  | conf >= 0.60 = "★★☆☆☆"
  | conf >= 0.50 = "★☆☆☆☆"
  | otherwise    = "☆☆☆☆☆"

chartTypeName :: ChartType -> String
chartTypeName HorizontalBarChart = "Horizontal Bar Chart"
chartTypeName VerticalBarChart = "Vertical Bar Chart"
chartTypeName LineChart = "Line Chart"
chartTypeName ScatterPlot = "Scatter Plot"
chartTypeName DotPlot = "Dot Plot"
chartTypeName Histogram = "Histogram"
chartTypeName BoxPlot = "Box Plot"
chartTypeName ConfidenceInterval = "Confidence Interval Chart"

-- | Get user's selection
getUserSelection :: [ChartRecommendation] -> IO UserChoice
getUserSelection recs = do
  putStrLn ""
  putStr $ "Select [1-" ++ show (length recs) ++ ", A=all, C=compare, H=help]: "
  hFlush stdout
  input <- getLine
  
  let inputUpper = map toUpper input
  
  case inputUpper of
    "A" -> return ShowAll
    "H" -> return Help
    "C" -> do
      putStr "Compare which? (e.g., \"1,2\" or \"1 3\"): "
      hFlush stdout
      compareInput <- getLine
      let indices = parseIndices compareInput
      if null indices || length indices < 2
        then do
          putStrLn "Please enter at least two chart numbers to compare."
          getUserSelection recs
        else return $ CompareCharts indices
    _ -> case readMaybe input :: Maybe Int of
      Just n | n >= 1 && n <= length recs -> do
        -- Safe indexing: n-1 is the 0-based index
        case recs !? (n - 1) of
          Just selectedRec -> return $ ChartSelection (recChartType selectedRec)
          Nothing -> do
            -- Logically unreachable due to bounds check, but handled safely
            putStrLn "Internal error: invalid selection. Please try again."
            getUserSelection recs
      _ -> do
        putStrLn $ "Invalid selection. Please enter a number between 1 and " ++ show (length recs)
        getUserSelection recs

parseIndices :: String -> [Int]
parseIndices input =
  let cleaned = map (\c -> if c == ',' then ' ' else c) input
      parts = words cleaned
      indices = [n | part <- parts, Just n <- [readMaybe part :: Maybe Int]]
  in indices

-- | Display all details
displayAllDetails :: ChartRecommendations -> IO ()
displayAllDetails recs = do
  putStrLn ""
  putStrLn $ T.unpack $ T.replicate 70 "="
  putStrLn "ALL CHART OPTIONS (Detailed View)"
  putStrLn $ T.unpack $ T.replicate 70 "="
  putStrLn ""
  
  mapM_ displayDetailedRecommendation $ zip [1..] (allEvaluations recs)
  
  putStrLn ""
  putStrLn "Press Enter to return to selection..."
  _ <- getLine
  return ()

displayDetailedRecommendation :: (Int, ChartRecommendation) -> IO ()
displayDetailedRecommendation (idx, rec) = do
  let stars = confidenceToStars (recConfidence rec)
  let pct = floor (recConfidence rec * 100) :: Int
  let chartName = chartTypeName (recChartType rec)
  
  putStrLn $ printf "[%d] %s %s (%d%%)" idx stars chartName pct
  putStrLn ""
  putStrLn $ "    Rationale: " ++ T.unpack (recRationale rec)
  putStrLn ""
  
  let breakdown = recScoreBreakdown rec
  putStrLn "    Score Breakdown:"
  putStrLn $ printf "      Data Structure:    %.0f%%" (dataStructureScore breakdown * 100)
  putStrLn $ printf "      Measurement Scale: %.0f%%" (measurementScaleScore breakdown * 100)
  putStrLn $ printf "      Sample Size:       %.0f%%" (sampleSizeScore breakdown * 100)
  putStrLn $ printf "      Purpose Fit:       %.0f%%" (purposeFitScore breakdown * 100)
  putStrLn $ printf "      Clarity:           %.0f%%" (clarityScore breakdown * 100)
  putStrLn $ printf "      Statistical Rigor: %.0f%%" (rigorScore breakdown * 100)
  putStrLn ""
  
  let pc = recProsAndCons rec
  putStrLn "    Strengths:"
  mapM_ (\p -> putStrLn $ "      ✓ " ++ T.unpack p) (pros pc)
  putStrLn ""
  putStrLn "    Limitations:"
  mapM_ (\c -> putStrLn $ "      ✗ " ++ T.unpack c) (cons pc)
  putStrLn ""
  putStrLn $ T.unpack $ T.replicate 70 "-"
  putStrLn ""

-- | Display comparison between charts
displayComparison :: ChartRecommendation -> ChartRecommendation -> IO ()
displayComparison rec1 rec2 = do
  putStrLn ""
  putStrLn $ T.unpack $ T.replicate 70 "="
  putStrLn $ "COMPARISON: " ++ chartTypeName (recChartType rec1) ++ " vs " ++ chartTypeName (recChartType rec2)
  putStrLn $ T.unpack $ T.replicate 70 "="
  putStrLn ""
  
  -- Confidence comparison
  let conf1 = recConfidence rec1
  let conf2 = recConfidence rec2
  putStrLn $ printf "Confidence Scores: %s (%.0f%%) vs %s (%.0f%%)" 
    (chartTypeName $ recChartType rec1) (conf1 * 100)
    (chartTypeName $ recChartType rec2) (conf2 * 100)
  putStrLn ""
  
  if conf1 > conf2
    then putStrLn $ "→ " ++ chartTypeName (recChartType rec1) ++ " is recommended (higher confidence)"
    else if conf2 > conf1
         then putStrLn $ "→ " ++ chartTypeName (recChartType rec2) ++ " is recommended (higher confidence)"
         else putStrLn "→ Both charts have equal confidence scores"
  putStrLn ""
  
  -- Score breakdown comparison
  putStrLn "Score Breakdown Comparison:"
  putStrLn ""
  
  let b1 = recScoreBreakdown rec1
  let b2 = recScoreBreakdown rec2
  
  displayScoreComparison "Data Structure" (dataStructureScore b1) (dataStructureScore b2)
  displayScoreComparison "Measurement Scale" (measurementScaleScore b1) (measurementScaleScore b2)
  displayScoreComparison "Sample Size" (sampleSizeScore b1) (sampleSizeScore b2)
  displayScoreComparison "Purpose Fit" (purposeFitScore b1) (purposeFitScore b2)
  displayScoreComparison "Clarity" (clarityScore b1) (clarityScore b2)
  displayScoreComparison "Statistical Rigor" (rigorScore b1) (rigorScore b2)
  putStrLn ""
  
  -- Pros comparison
  putStrLn $ chartTypeName (recChartType rec1) ++ " Strengths:"
  mapM_ (\p -> putStrLn $ "  ✓ " ++ T.unpack p) (pros $ recProsAndCons rec1)
  putStrLn ""
  
  putStrLn $ chartTypeName (recChartType rec2) ++ " Strengths:"
  mapM_ (\p -> putStrLn $ "  ✓ " ++ T.unpack p) (pros $ recProsAndCons rec2)
  putStrLn ""
  
  -- Recommendation
  putStrLn "Which is better for YOUR data?"
  if conf1 > conf2 + 0.10
    then putStrLn $ "→ " ++ chartTypeName (recChartType rec1) ++ " is strongly recommended"
    else if conf2 > conf1 + 0.10
         then putStrLn $ "→ " ++ chartTypeName (recChartType rec2) ++ " is strongly recommended"
         else putStrLn "→ Both are viable options. Choose based on your communication goals."
  putStrLn ""

displayScoreComparison :: String -> Double -> Double -> IO ()
displayScoreComparison label score1 score2 = do
  let pct1 = floor (score1 * 100) :: Int
  let pct2 = floor (score2 * 100) :: Int
  let winner = if score1 > score2 then " ←" else if score2 > score1 then " →" else "" :: String
  putStrLn $ printf "  %-20s: %3d%% vs %3d%%%s" label pct1 pct2 winner

displayComparisonFor :: ChartRecommendations -> [Int] -> IO ()
displayComparisonFor recs indices = do
  let allRecs = allEvaluations recs
  let validIndices = filter (\i -> i >= 1 && i <= length allRecs) indices
  
  if length validIndices < 2
    then putStrLn "Invalid chart numbers for comparison."
    else do
      -- Use safe indexing for the validIndices list
      case (validIndices !? 0, validIndices !? 1) of
        (Just idx1, Just idx2) -> 
          case (allRecs !? (idx1 - 1), allRecs !? (idx2 - 1)) of
            (Just rec1, Just rec2) -> do
              displayComparison rec1 rec2
              putStrLn ""
              putStrLn "Press Enter to return to selection..."
              _ <- getLine
              return ()
            _ -> putStrLn "Invalid chart numbers for comparison."
        _ -> putStrLn "Invalid chart numbers for comparison."

-- | Display help
displayHelp :: IO ()
displayHelp = do
  putStrLn ""
  putStrLn $ T.unpack $ T.replicate 70 "="
  putStrLn "HELP - How to Use Interactive Selection"
  putStrLn $ T.unpack $ T.replicate 70 "="
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  1-n      Select a chart by number"
  putStrLn "  A        Show all chart options with full details"
  putStrLn "  C        Compare two or more charts side-by-side"
  putStrLn "  H        Show this help message"
  putStrLn ""
  putStrLn "Understanding Rankings:"
  putStrLn "  ★★★★★    Excellent fit (90-100% confidence)"
  putStrLn "  ★★★★☆    Very good fit (80-89% confidence)"
  putStrLn "  ★★★☆☆    Good fit (70-79% confidence)"
  putStrLn "  ★★☆☆☆    Fair fit (60-69% confidence)"
  putStrLn "  ★☆☆☆☆    Marginal fit (50-59% confidence)"
  putStrLn ""
  putStrLn "The ranking system evaluates six factors:"
  putStrLn "  1. Data Structure (30%) - How well your data matches the chart's needs"
  putStrLn "  2. Measurement Scale (25%) - Whether the chart supports your data types"
  putStrLn "  3. Sample Size (15%) - If you have too few or too many data points"
  putStrLn "  4. Purpose Fit (15%) - How well it serves your analytical goals"
  putStrLn "  5. Clarity (10%) - How easy it is for audiences to understand"
  putStrLn "  6. Statistical Rigor (5%) - How sound the statistical foundation is"
  putStrLn ""
  putStrLn "Press Enter to return to selection..."
  _ <- getLine
  return ()

------------------------------------------------------------
-- Column Selection
------------------------------------------------------------

-- | Prompt user to select a column from available columns
promptForColumn :: Vector Text -> Text -> IO Text
promptForColumn availableColumns purpose = do
  putStrLn ""
  putStrLn $ "The selected chart requires: " ++ T.unpack purpose
  putStrLn "Available columns:"
  V.imapM_ (\idx col -> putStrLn $ "  " ++ show (idx + 1) ++ ". " ++ T.unpack col) availableColumns
  putStrLn ""
  putStr "Enter column number or name: "
  hFlush stdout
  input <- TIO.getLine
  
  -- Try parsing as number first
  case readMaybe (T.unpack input) :: Maybe Int of
    Just n | n >= 1 && n <= V.length availableColumns -> 
      return $ availableColumns V.! (n - 1)
    _ -> 
      -- Try matching by name
      if input `V.elem` availableColumns
        then return input
        else do
          putStrLn "Invalid column. Please try again."
          promptForColumn availableColumns purpose

-- | Get yes/no confirmation
getUserConfirmation :: Text -> IO Bool
getUserConfirmation prompt = do
  putStr $ T.unpack prompt ++ " [y/n]: "
  hFlush stdout
  input <- getLine
  return $ map toLower input `elem` ["y", "yes"]

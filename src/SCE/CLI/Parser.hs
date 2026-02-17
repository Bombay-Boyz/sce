{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : SCE.CLI.Parser
Description : Command-line argument parsing
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Parses and validates command-line arguments.
-}
module SCE.CLI.Parser
  ( -- * CLI Configuration
    CLIConfig(..)
  , parseArgs
  , defaultCLIConfig
  , printUsage
  ) where

import SCE.Core.Types
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)

-- | CLI configuration options
data CLIConfig = CLIConfig
  { cliInputFile     :: FilePath
  , cliOutputFile    :: Maybe FilePath
  , cliOutputFormat  :: OutputFormat
  , cliValueColumn   :: Text
  , cliLabelColumn   :: Maybe Text
  , cliXColumn       :: Maybe Text
  , cliYColumn       :: Maybe Text
  , cliSeriesColumns :: [Text]  -- For multi-series line charts
  , cliTitle         :: Maybe Text
  , cliShowHelp      :: Bool
  }
  deriving stock (Show, Eq)

-- | Default CLI configuration
defaultCLIConfig :: CLIConfig
defaultCLIConfig = CLIConfig
  { cliInputFile = ""
  , cliOutputFile = Nothing
  , cliOutputFormat = PlainText
  , cliValueColumn = "Value"
  , cliLabelColumn = Nothing
  , cliXColumn = Nothing
  , cliYColumn = Nothing
  , cliSeriesColumns = []
  , cliTitle = Nothing
  , cliShowHelp = False
  }

-- | Parse command-line arguments
parseArgs :: IO CLIConfig
parseArgs = do
  args <- getArgs
  return $ parseArgsList args defaultCLIConfig

-- | Parse argument list
parseArgsList :: [String] -> CLIConfig -> CLIConfig
parseArgsList [] config = config
parseArgsList ("--help":_) config = config { cliShowHelp = True }
parseArgsList ("-h":_) config = config { cliShowHelp = True }
parseArgsList ("--input":file:rest) config = 
  parseArgsList rest $ config { cliInputFile = file }
parseArgsList ("-i":file:rest) config = 
  parseArgsList rest $ config { cliInputFile = file }
parseArgsList ("--output":file:rest) config = 
  parseArgsList rest $ config { cliOutputFile = Just file }
parseArgsList ("-o":file:rest) config = 
  parseArgsList rest $ config { cliOutputFile = Just file }
parseArgsList ("--format":fmt:rest) config = 
  parseArgsList rest $ config { cliOutputFormat = parseFormat fmt }
parseArgsList ("--value-column":col:rest) config = 
  parseArgsList rest $ config { cliValueColumn = T.pack col }
parseArgsList ("--label-column":col:rest) config = 
  parseArgsList rest $ config { cliLabelColumn = Just $ T.pack col }
parseArgsList ("--x-column":col:rest) config = 
  parseArgsList rest $ config { cliXColumn = Just $ T.pack col }
parseArgsList ("--y-column":col:rest) config = 
  parseArgsList rest $ config { cliYColumn = Just $ T.pack col }
parseArgsList ("--series":cols:rest) config = 
  parseArgsList rest $ config { cliSeriesColumns = map T.pack $ words $ map (\c -> if c == ',' then ' ' else c) cols }
parseArgsList ("--title":title:rest) config = 
  parseArgsList rest $ config { cliTitle = Just $ T.pack title }
parseArgsList (arg:rest) config
  | null (cliInputFile config) = parseArgsList rest $ config { cliInputFile = arg }
  | otherwise = parseArgsList rest config

-- | Parse output format
parseFormat :: String -> OutputFormat
parseFormat "text" = PlainText
parseFormat "markdown" = Markdown
parseFormat "md" = Markdown
parseFormat "docx" = DocX
parseFormat _ = PlainText

printUsage :: IO ()
printUsage = putStrLn $ unlines
  [ "Statistical Charting Engine (SCE) v2.0 - Interactive Mode"
  , ""
  , "Usage: sce [OPTIONS] <input.csv>"
  , ""
  , "Options:"
  , "  -i, --input FILE         Input CSV file (required)"
  , "  -o, --output FILE        Output file (default: stdout)"
  , "  --format FORMAT          Output format: text, markdown, docx (default: text)"
  , "  --value-column COL       Column name for values (default: Value)"
  , "  --label-column COL       Column name for labels"
  , "  --x-column COL           X-axis column for scatter plots"
  , "  --y-column COL           Y-axis column for scatter plots"
  , "  --series COLS            Comma-separated series columns for multi-series line charts"
  , "  --title TITLE            Title for the report"
  , "  -h, --help               Show this help message"
  , ""
  , "Interactive Mode:"
  , "  SCE now runs in interactive mode by default. After loading your data,"
  , "  the engine analyzes it and presents ranked chart recommendations."
  , "  You can then select the best visualization for your needs."
  , ""
  , "Column Specifications:"
  , "  - If not provided via flags, missing columns will be requested interactively"
  , "  - The system shows available columns and guides you through selection"
  , ""
  , "Examples:"
  , "  sce data.csv                                    # Interactive selection"
  , "  sce --input data.csv --output report.txt       # Save to file"
  , "  sce --input data.csv --format markdown         # Markdown output"
  , "  sce data.csv --value-column Revenue            # Pre-specify value column"
  , "  sce data.csv --x-column Height --y-column Weight  # Pre-specify scatter axes"
  , "  sce data.csv --series Revenue,Expenses,Profit  # Multi-series line chart columns"
  ]

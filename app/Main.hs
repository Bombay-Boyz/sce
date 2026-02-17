{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : SCE command-line application entry point
Copyright   : (c) SCE Team, 2026
License     : BSD-3-Clause

Main entry point for the Statistical Charting Engine CLI.
-}
module Main (main) where

import SCE.CLI.Parser
import SCE.CLI.Runner
import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  -- Set UTF-8 encoding for output
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  
  -- Parse command-line arguments
  config <- parseArgs
  
  -- Run the application
  runCLI config

# SCE — Statistical Charting Engine

A command-line statistical analysis and charting tool written in Haskell.
SCE reads CSV files, infers measurement scales, computes descriptive statistics,
and renders ASCII charts and tables directly in the terminal.

---

## Table of Contents

- [Quick Start](#quick-start)
- [Building](#building)
- [Running Tests](#running-tests)
- [Usage](#usage)
- [Architecture](#architecture)
  - [Phase 0 — Error System](#phase-0--error-system)
  - [Phase 1 — Ingestion Pipeline](#phase-1--ingestion-pipeline)
- [Configuration](#configuration)
- [Development Status](#development-status)

---

## Quick Start

```bash
# Build the project
cabal update
echo "tests: True" >> cabal.project.local
cabal build

# Run on a CSV file
cabal run sce -- --input data.csv

# Run tests
cabal test
```

---

## Building

**Requirements**: GHC 9.4+ and Cabal 3.0+

```bash
cabal build          # library + executable
cabal build sce      # executable only
cabal test           # build and run all test suites
cabal run sce -- --help
```

If the test suite solver fails, enable tests explicitly:

```bash
echo "tests: True" >> cabal.project.local
cabal test
```

You also need `hspec-discover` on your PATH:

```bash
cabal install hspec-discover
export PATH="$HOME/.cabal/bin:$PATH"
```

---

## Running Tests

```bash
cabal test                              # all suites
cabal test --test-show-details=always  # verbose output

# Re-run only a specific suite section
cabal test --test-option='--match "/Phase1.Ingestion/"'
```

The test suite is split by phase:

| File | Coverage |
|---|---|
| `test/Phase0/ErrorSpec.hs` | 63 tests — error construction, formatting, QuickCheck |
| `test/Phase1/IngestionSpec.hs` | 50+ tests — parser, schema, missingness, coercion, QuickCheck |

---

## Usage

```
sce --input <file.csv> [options]

Options:
  --input FILE         CSV input file (required)
  --chart TYPE         Chart type: bar, line, scatter, histogram, boxplot,
                       dotchart, confidence
  --column COL         Primary data column
  --x-column COL       X-axis column (scatter plots)
  --y-column COL       Y-axis column (scatter plots)
  --output FILE        Write output to file instead of stdout
  --format FORMAT      Output format: text (default), markdown
  --title TITLE        Chart title
  --interactive        Launch interactive mode
  --help               Show this help
```

### Examples

```bash
# Histogram of a numeric column
cabal run sce -- --input sales.csv --chart histogram --column revenue

# Scatter plot
cabal run sce -- --input data.csv --chart scatter --x-column age --y-column income

# Interactive mode
cabal run sce -- --input data.csv --interactive
```

---

## Architecture

SCE is structured as a phased improvement project. Each phase is independent
and builds on the previous one.

```
src/SCE/
├── Core/
│   ├── Error.hs          Phase 0 — unified error type
│   ├── Types.hs          Core type definitions + re-exports
│   ├── DataModel.hs      Legacy CSV parsing (retained for CLI compatibility)
│   ├── MeasurementScale.hs
│   ├── Statistics.hs
│   └── Logger.hs
├── Ingestion/            Phase 1 — three-stage ingestion pipeline
│   ├── Parser.hs         Stage 1: bytes → RawFrame
│   ├── Schema.hs         Stage 2: RawFrame → CandidateSchema → CommittedSchema
│   ├── MissingData.hs    Missing data analysis and complete-case filtering
│   └── Coercion.hs       Stage 3: CommittedSchema + RawFrame → ValidatedFrame
├── Validation/           Validators and invariant checks
├── Chart/                Chart renderers (bar, line, scatter, etc.)
├── Table/                Table formatters
├── Output/               Output renderers (text, markdown, docx)
└── CLI/                  Command-line interface
```

### Phase 0 — Error System

All functions that can fail return `SCEResult a = Either DetailedError a`.

```haskell
data DetailedError = DetailedError
  { errorCode        :: ErrorCode      -- E1001..E5003
  , errorMessage     :: Text
  , errorContext     :: ErrorContext   -- file/data/computation context
  , errorSuggestions :: [Text]
  , errorSeverity    :: Severity       -- Info | Warning | Error | Critical
  , errorCategory    :: StatEngineError
  }
```

Error codes by range:

| Range | Category |
|---|---|
| E1001–E1005 | Ingestion errors |
| E2001–E2008 | Validation errors |
| E3001–E3004 | Statistical errors |
| E4001–E4004 | Inference errors |
| E5001–E5003 | Rendering errors |

Format errors for display:

```haskell
formatError      :: DetailedError -> Text  -- multi-line with border
formatErrorShort :: DetailedError -> Text  -- single line for logs
```



## License

BSD-3-Clause. See `LICENSE`.

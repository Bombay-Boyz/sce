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

### Phase 1 — Ingestion Pipeline

A three-stage pipeline replaces the monolithic `DataModel.parseCSVFile`.

#### Stage 1: Parser

```haskell
parseCSVBytes :: ParseConfig -> ByteString -> Either DetailedError RawFrame
parseCSVFile  :: ParseConfig -> FilePath   -> IO (Either DetailedError RawFrame)
detectDelimiter :: ByteString -> Either DetailedError Char
```

`RawFrame` contains only raw `Text` values — no type inference yet.
`detectDelimiter` tries comma, tab, semicolon, pipe by scoring consistency
across the first 5 lines.

#### Stage 2: Schema

```haskell
inferSchema   :: RawFrame          -> Either DetailedError CandidateSchema
confirmSchema :: CandidateSchema
              -> SchemaConfirmation -> Either DetailedError CommittedSchema
```

`inferSchema` analyses each column and assigns a `CandidateScale`:

| Scale | Criteria |
|---|---|
| `LikelyCount` | All non-negative integers |
| `LikelyRatio` | All numeric, no negatives |
| `LikelyInterval` | Numeric with any negative value |
| `LikelyNominal` | Low-cardinality text |
| `LikelyOrdinal` | Text with detectable rank order |
| `Ambiguous` | Cannot determine |

**Key invariant**: `LikelyRatio` is never assigned to a column that contains
any negative value.

`confirmSchema` validates user-supplied `MeasurementScale` choices against the
data (e.g. returns `Left E2003` if user claims Ratio but negatives exist).

#### Stage 3: Coercion

```haskell
coerceFrame :: CommittedSchema
            -> MissingnessReport
            -> RawFrame
            -> Either DetailedError ValidatedFrame
```

Converts raw text to typed `Vector`s. Unparseable values become missing
(recorded as `CoercionWarning`) rather than causing failure. Returns
`Left E2002` only if zero usable rows remain.

```haskell
data TypedColumn
  = NumericCol   Text MeasurementScale (Vector Double)
  | IntegerCol   Text MeasurementScale (Vector Int)
  | CategoricCol Text MeasurementScale (Vector Text)
```

#### Missing Data

```haskell
analyseMissingness  :: RawFrame -> MissingnessReport
completeCaseFilter  :: [Text] -> RawFrame -> Either DetailedError RawFrame
```

`analyseMissingness` classifies each column as `CompleteData`, `MCAR`, `MAR`,
or `MNAR` using heuristics. `completeCaseFilter` returns `Left E2002` if
filtering would leave zero rows.

---

## Configuration

No configuration file is required for basic use. Advanced use:

```haskell
defaultParseConfig :: ParseConfig
defaultParseConfig = ParseConfig
  { pcDelimiter   = Nothing  -- auto-detect
  , pcHasHeader   = True
  , pcQuoteChar   = '"'
  , pcCommentChar = Nothing
  , pcMaxRows     = Nothing
  }
```

---

## Development Status

| Phase | Status | Description |
|---|---|---|
| Phase 0 | ✅ Complete | Unified `DetailedError` / `SCEResult` system |
| Phase 1 | ✅ Complete | Three-stage ingestion pipeline |
| Phase 2 | 🔲 Planned | Data validation layer |
| Phase 3 | 🔲 Planned | Statistical computation improvements |
| Phase 4 | 🔲 Planned | Inference and hypothesis testing |
| Phase 5 | 🔲 Planned | Rendering improvements |
| Phase 6 | 🔲 Planned | LiquidHaskell verification |

---

## License

BSD-3-Clause. See `LICENSE`.

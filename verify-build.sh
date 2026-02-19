#!/bin/bash
# Build Verification Script for SCE v2.0.0

set -e

echo "==================================="
echo "SCE v2.0.0 Build Verification"
echo "==================================="
echo ""

# Check if cabal is available
if ! command -v cabal &> /dev/null; then
    echo "ERROR: cabal not found. Please install GHC and cabal-install."
    echo ""
    echo "On Ubuntu/Debian:"
    echo "  sudo apt-get install ghc cabal-install"
    echo ""
    echo "On macOS:"
    echo "  brew install ghc cabal-install"
    echo ""
    echo "Or use GHCup (recommended):"
    echo "  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
    exit 1
fi

echo "✓ cabal found: $(cabal --version | head -1)"
echo ""

# Update package index
echo "Updating cabal package index..."
cabal update

# Build the project
echo ""
echo "Building SCE v2.0.0..."
echo "This may take a few minutes on first build..."
echo ""
cabal build

if [ $? -eq 0 ]; then
    echo ""
    echo "✓ BUILD SUCCESSFUL"
    echo ""
else
    echo ""
    echo "✗ BUILD FAILED"
    echo "Please check the error messages above."
    exit 1
fi

# Run tests
echo "Running test suite..."
echo ""
cabal test

if [ $? -eq 0 ]; then
    echo ""
    echo "✓ ALL TESTS PASSED"
    echo ""
else
    echo ""
    echo "✗ SOME TESTS FAILED"
    echo "Please check the test output above."
    exit 1
fi

# Verify no unsafe code
echo "Verifying no unsafe error calls..."
ERROR_CALLS=$(grep -rn "error \"" src/ 2>/dev/null | wc -l)
if [ "$ERROR_CALLS" -eq 0 ]; then
    echo "✓ No unsafe error calls found"
else
    echo "✗ Found $ERROR_CALLS error calls (should be 0)"
    grep -rn "error \"" src/
    exit 1
fi

echo ""
echo "==================================="
echo "✓ ALL VERIFICATIONS PASSED"
echo "==================================="
echo ""
echo "Your SCE v2.0.0 is ready to use!"
echo ""
echo "To run the application:"
echo "  cabal run sce -- --input your-data.csv"
echo ""
echo "To install system-wide:"
echo "  cabal install"
echo ""

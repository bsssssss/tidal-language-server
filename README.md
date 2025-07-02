# tidal_ls

A language server for Tidal Cycles

# Build and Installation Guide

## Prerequisites

1. **Haskell Stack or Cabal**: You need either Stack or Cabal to build this Haskell project
2. **GHC**: Glasgow Haskell Compiler (recommended version 9.4+)
3. **Tidal Cycles Source Code**: You must have a local copy of the Tidal Cycles source code

## Required Setup

### 1. Get Tidal Cycles Source Code

Clone the Tidal Cycles repository:

```bash
git clone https://github.com/tidalcycles/Tidal.git
```

### 2. Set TIDAL_PATH Environment Variable

The language server requires the `TIDAL_PATH` environment variable to point to your Tidal Cycles source directory.

**On Linux/macOS:**
```bash
export TIDAL_PATH=/path/to/your/Tidal
```

## Building

### Using Cabal (Recommended)

```bash
# Install dependencies and build
cabal build

# Install the executable
cabal install
```

## Installation

After building, you should have info about where it symlinked.
For me it's `~/.local/bin/tidal-ls`

Make sure the installation directory is in your `PATH`.

## Verification

To verify the installation:

```bash
# Check if tidal-ls is available
which tidal-ls

# Verify TIDAL_PATH is set correctly
echo $TIDAL_PATH
```

The language server should start without errors when called by your editor's LSP client.

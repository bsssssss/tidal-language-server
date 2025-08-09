# Tidal language server

This is a **very experimental** project that aims to provide useful editor features
when working with tidal files, such as context aware auto-completion, hover
documentation and linting.

For now it only handle crude auto-completion and hover documentation. It does so
by parsing Tidal's source code and extracting haddock comments about functions.

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

### 2. Set TIDAL_SRC_PATH Environment Variable

The language server requires the `TIDAL_SRC_PATH` environment variable to point to your Tidal Cycles source directory.

```bash
export TIDAL_SRC_PATH=/path/to/your/Tidal
```

## Building

### Using Cabal

```bash
# Refresh package index
cabal update

# Install dependencies and build
cabal build
```

## Installation

```bash
# Install the executable
cabal install
```

This should tell you where the executable was symlinked.
On my system (osx Sequoia 15.5) it's `~/.local/bin/tidal-language-server`.
Make sure the installation directory is in your `PATH`.

## Verification

To verify the installation:

```bash
# Check if tidal-language-server is available
which tidal-language-server

# Verify TIDAL_PATH is set correctly
echo $TIDAL_SRC_PATH
```

## Use in Neovim

First of all, your setup should be able to recognize tidal filetype. You can do
this by installing [this plugin](https://github.com/tidalcycles/vim-tidal)

Here is an auto-command to start the server when a tidal file is opened.

```lua
-- Tidal language server
vim.api.nvim_create_autocmd("FileType", {
	pattern = "tidal",
	callback = function()
		-- local logfile = "/tmp/tidal_ls.log"
		local client_id = vim.lsp.start({
			name = "tidal_ls",
			cmd = { vim.fn.expand("~/.local/bin/tidal-language-server") },
			handlers = {
				["window/showMessage"] = function(_, result, ctx)
					vim.notify("LSP Message: " .. result.message, vim.log.levels.INFO, { title = "Tidal LSP" })
				end,
				["window/logMessage"] = function(_, result, ctx)
					vim.notify("LSP Log: " .. result.message, vim.log.levels.DEBUG, { title = "Tidal LSP Log" })
				end,
			},
		})
		if not client_id then
			vim.notify("Failed to start LSP client", vim.log.levels.ERROR)
		else
			vim.notify("LSP client started", vim.log.levels.INFO)
		end
	end,
})
```

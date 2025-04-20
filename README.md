# An LSP implementation for [tree-sitter](https://tree-sitter.github.io/tree-sitter/) query files

<!-- vim: set spell: -->

## Configuration

Configuration can be done via server initialization or via a configuration file
named `tsqueryrc.json` located at the project workspace root. Below is an
example file:

```json
{
  "parser_install_directories": ["a/list/of", "parser/installation/paths"],
  "parser_aliases": {
    "ecma": "javascript"
  },
  "language_retrieval_patterns": [
    "languages/src/([^/]+)/[^/]+\\.scm$"
  ]
}
```

### Configuration options

#### `parser_install_directories`

A list of strings representing directories to search for parsers, of the form
`<lang>.(so|dll|dylib)` or `tree-sitter-<lang>.wasm`.

#### `parser_aliases`

A map of parser aliases. E.g., to point `queries/ecma/*.scm` files to the
`javascript` parser:

```json
{
  "parser_aliases": {
    "ecma": "javascript"
  }
}
```

#### `language_retrieval_patterns`

A list of patterns to aid the LSP in finding a language, given a file path.
Patterns must have one capture group which represents the language name. Ordered
from highest to lowest precedence. E.g., for `zed` support:

```json
{
  "language_retrieval_patterns": [
    "languages/src/([^/]+)/[^/]+\\.scm$"
  ]
}
```

**NOTE:** The following fallbacks are _always_ provided:

- `tree-sitter-([^/]+)/queries/[^/]+\.scm$`
- `queries/([^/]+)/[^/]+\.scm$`

### Example setup (for Neovim):

```lua
-- Disable the (slow) builtin query linter
vim.g.query_lint_on = {}

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'query',
  callback = function(ev)
    if vim.bo[ev.buf].buftype == 'nofile' then
      return
    end
    vim.lsp.start {
      name = 'ts_query_ls',
      cmd = { '/path/to/ts_query_ls/target/release/ts_query_ls' },
      root_dir = vim.fs.root(0, { 'tsqueryrc.json', 'queries' }),
      -- OPTIONAL: Override the query omnifunc
      on_attach = function(_, buf)
        vim.bo[buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
      end,
      init_options = {
        parser_install_directories = {
          -- If using nvim-treesitter with lazy.nvim
          vim.fs.joinpath(
            vim.fn.stdpath('data'),
            '/lazy/nvim-treesitter/parser/'
          ),
        },
        parser_aliases = {
          ecma = 'javascript',
        },
        language_retrieval_patterns = {
          'languages/src/([^/]+)/[^/]+\\.scm$',
        },
      },
    }
  end,
})
```

## Standalone tool

### Formatter

The language server can be used as a standalone formatter by passing the
`format` argument, e.g. `ts_query_ls format ./queries --mode write`. The command
can accept multiple directories to format, and must be passed a "mode" of either
`write` or `check`. The mode determines whether the files will be overwritten or
just checked for proper formatting.

```sh
# use this command for the full documentation
ts_query_ls format --help
```

### Linter

The formatter can also be used as standalone linter by passing the `check`
argument, e.g:

```sh
ts_query_ls check ./queries --config \
'{"parser_install_directories": ["/home/jdoe/Documents/parsers/"]}'
```

The command expects a list of directories to search for queries, as well as a
flag to pass JSON configuration to the server (needed to detect parser
locations).

```sh
# use this command for the full documentation
ts_query_ls check --help
```

## Checklist

- [x] References for captures
- [x] Renaming captures
- [x] Completions for capture names in a pattern (for predicates)
- [x] Completions for node names
- [x] Fix utility functions, making them robust when it comes to UTF-16 code
      points
- [x] Go to definition for captures
- [x] Recognition/completion of supertypes (requires `tree-sitter 0.25`)
- [x] Completions and diagnostics for a supertype's subtypes
  - Requires <https://github.com/tree-sitter/tree-sitter/pull/3938>
- [x] Completions field names
- [x] Diagnostics for unrecognized nodes
- [x] Diagnostics for referencing undefined capture groups in predicates
- [x] Diagnostics for incorrect syntax
- [ ] Diagnostics for impossible patterns
  - Currently not possible without a full (sometimes expensive) run of the query
    file. This should either be implemented as a user command, or core methods
    should be exposed to gather pattern information more efficiently
- [x] Recognize parsers built for `WASM`
- [x] Document formatting compatible with the `nvim-treesitter` formatter
- [x] Code cleanup
- [x] Add tests for all* functionality

> *All handlers are tested, but core functionality like language loading will be
> more complicated, and does not yet have test coverage.

### Packaging

- [ ] [`homebrew`](https://github.com/Homebrew/homebrew-core)
      ([it's complicated](https://github.com/Homebrew/homebrew-core/pull/197587))
- [x] [`nixpkgs`](https://github.com/NixOS/nixpkgs)
- [x] [`mason.nvim`](https://github.com/mason-org/mason-registry)
- [x] [`AUR`](https://aur.archlinux.org/)

And others?

## References

Many thanks to @lucario387, and the
[asm-lsp](https://github.com/bergercookie/asm-lsp),
[`jinja-lsp`](https://github.com/uros-5/jinja-lsp),
[`beancount`-language-server](https://github.com/polarmutex/beancount-language-server),
and [helix-editor](https://github.com/helix-editor/helix) projects for the
amazing code that I took inspiration from!

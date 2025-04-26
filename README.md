# An LSP implementation for [tree-sitter](https://tree-sitter.github.io/tree-sitter/) query files

<!-- vim: set spell: -->

## Configuration

Configuration can be done via server initialization or via a configuration file
named `.tsqueryrc.json` located in the project workspace directory, or in any of
its ancestor directories. Below is an example file:

```json
{
  "$schema": "https://raw.githubusercontent.com/ribru17/ts_query_ls/refs/heads/master/schemas/config.json",
  "parser_install_directories": ["${HOME}/my/parser", "/installation/paths"],
  "parser_aliases": {
    "ecma": "javascript"
  },
  "language_retrieval_patterns": [
    "languages/src/([^/]+)/[^/]+\\.scm$"
  ],
  "valid_captures": {
    "highlights": {
      "variable": "Simple identifiers",
      "variable.parameter": "Parameters of a function"
    }
  },
  "valid_predicates": {
    "eq": {
      "parameters": [
        {
          "type": "capture",
          "arity": "required"
        },
        {
          "type": "any",
          "arity": "required"
        }
      ],
      "description": "Checks for equality between two nodes, or a node and a string.",
      "any": true
    }
  }
}
```

### Configuration options

#### `parser_install_directories`

A list of strings representing directories to search for parsers, of the form
`<lang>.(so|dll|dylib)` or `tree-sitter-<lang>.wasm`.

Supports environment variable expansion of the form `${VAR}`.

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

#### `valid_captures`

A map from query file name to valid captures. Valid captures are represented as
a map from capture name (sans `@`) to a short (markdown format) description.
Note that captures prefixed with an underscore are always permissible.

```json
{
  "valid_captures": {
    "highlights": {
      "variable": "Simple identifiers",
      "variable.parameter": "Parameters of a function"
    }
  }
}
```

### `valid_predicates`

A map of predicate names (sans `#` and `?`) to parameter specifications.

Parameters can be one or both of two types (a capture or a string), and can be
required, optional, or "variadic" (there can be zero-to-many of them). Optional
parameters cannot be followed by required parameters, and a variadic parameter
may only appear once, as the last parameter.

```json
{
  "valid_predicates": {
    "any-of": {
      "parameters": [
        {
          "type": "capture",
          "arity": "required"
        },
        {
          "type": "string",
          "arity": "required"
        },
        {
          "type": "string",
          "arity": "variadic"
        }
      ],
      "description": "Checks for equality between multiple strings"
    }
  }
}
```

Predicates are special because they can also accept two other properties: `not`
(`boolean`, default `true`), and `any` (`boolean`, default `false`). `not` means
that the predicate supports a `not-` prefixed version of itself, which acts as
its negation, and `any` means that is supports an `any-` prefixed version of
itself, which holds true if any of the nodes in a quantified capture hold true.
If both properties are `true`, then there will be a predicate of the form
`#not-any-foo?`.

### `valid_directives`

Same as `valid_predicates`, but for directives (e.g. `#foo!`).

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
      root_dir = vim.fs.root(0, { '.tsqueryrc.json', 'queries' }),
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
`format` argument, e.g. `ts_query_ls format ./queries`. The command can accept
multiple directories to format. It can also run in "check" mode by passing the
`--check` (`-c`) flag, which will only validate formatting without writing to
the files.

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

Note that unlike the live server diagnostics, the `check` command runs a full
query scan to check for things like impossible query patterns.

The command can accept a list of directories to search for queries, as well as a
flag to pass JSON configuration to the server (needed to detect parser
locations). If no configuration flag is passed, the command will attempt to read
it from the `.tsqueryrc.json` configuration file in the current directory. If no
directories are specified to be checked, then the command will search for all
queries in the current directory.

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

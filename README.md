# An LSP implementation for [tree-sitter](https://tree-sitter.github.io/tree-sitter/) query files

<!-- vim: set spell: -->

## Configuration

```jsonc
{
  "settings": {
    // Where to look for parsers, of the form <lang>.(so|dll|dylib)
    // or tree-sitter-<lang>.wasm
    "parser_install_directories": ["a/list/of", "parser/installation/paths"],
    // A list of parser aliases (e.g. point queries/ecma/*.scm files to the
    // javascript parser)
    "parser_aliases": {
      "ecma": "javascript"
    },
    // A list of patterns to aid the LSP in finding a language, given a file
    // path. Patterns must have one capture group which represents the language
    // name. Ordered from highest to lowest precedence.
    "language_retrieval_patterns": [
      // E.g. zed support
      "languages/src/([^/]+)/[^/]+\\.scm$"
      // The following fallbacks are *always* provided:
      //
      // tree-sitter-([^/]+)/queries/[^/]+\.scm$
      // queries/([^/]+)/[^/]+\.scm$
    ]
  }
}
```

Example setup (for Neovim):

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
      root_dir = vim.fs.root(0, { 'queries' }),
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
      ([in progress](https://github.com/Homebrew/homebrew-core/pull/197587),
      requires repo to reach 75 GitHub stars)
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

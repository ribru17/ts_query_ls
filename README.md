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
    }
  }
}
```

Example setup (for Neovim):

```lua
vim.lsp.start {
  name = 'ts_query_ls',
  path = {'path/to/cmd'},
  settings = {
    parser_install_directories = {
      -- If using nvim-treesitter with lazy.nvim
      '/home/MYUSERNAME/.local/share/nvim/lazy/nvim-treesitter/parser/'
    }
    parser_aliases = {
      ecma = 'javascript'
    }
  }
}
```

## Checklist

- [x] References for captures
- [x] Renaming captures
- [x] Completions for capture names in a pattern (for predicates)
- [x] Completions for node names
- [x] Fix utility functions, making them robust when it comes to UTF-16 code
      points
- [ ] Go to definition for captures
- [ ] Completions field names
- [ ] Diagnostics for unrecognized nodes
- [ ] Diagnostics for referencing undefined capture groups in predicates
- [ ] Diagnostics for incorrect syntax
- [ ] Diagnostics for impossible patterns
- [x] Recognize parsers built for `WASM`
- [ ] Document formatting compatible with the `nvim-treesitter` formatter

## References

Many thanks to the [`jinja-lsp`](https://github.com/uros-5/jinja-lsp),
[`beancount`-language-server](https://github.com/polarmutex/beancount-language-server),
and [helix-editor](https://github.com/helix-editor/helix) projects for the
amazing code that I took inspiration from!

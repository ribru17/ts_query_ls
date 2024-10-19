# An LSP implementation for [tree-sitter](https://tree-sitter.github.io/tree-sitter/) query files

<!-- vim: set spell: -->

## Checklist

- [x] References for captures
- [x] Renaming captures
- [x] Completions for capture names in a pattern (for predicates)
- [x] Completions for node names
- [ ] Go to definition for captures
- [ ] Completions field names
- [ ] Diagnostics for unrecognized nodes
- [ ] Diagnostics for referencing undefined capture groups in predicates
- [ ] Diagnostics for incorrect syntax
- [ ] Diagnostics for impossible patterns
- [ ] Recognize parsers built for `WASM`
- [ ] Fix utility functions, making them robust when it comes to UTF-16 code
      points

## References

Much thanks to the
[`beancount`-language-server](https://github.com/polarmutex/beancount-language-server)
and [helix-editor](https://github.com/helix-editor/helix) projects for the
amazing code that I borrowed!

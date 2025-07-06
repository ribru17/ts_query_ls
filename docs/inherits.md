## Inheriting queries

```query
; inherits: foo,bar
```

Queries can inherit other queries if they have an `; inherits:` comment as the
first line of the query file. The language server will then act as though the
text of the inherited query files was placed at the top of the document, and
will provide diagnostics for the text in those queries as well (calculated with
the language information of the parent query). Queries will always inherit
others of the same type (e.g. a `highlights.scm` will only import other
`highlights.scm`, never an `injections.scm`).

Note that the syntax is very sensitive; there must be _exactly one_ space after
the `inherits:` keyword, and there must be no spaces in-between module names.

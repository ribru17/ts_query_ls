## `; format-ignore`

The formatter will ignore nodes that are preceeded by a comment starting with
`format-ignore`.

```query
((call_expression
  function: (identifier) @function.builtin)
  ; format-ignore
  (#any-of? @function.builtin
    "printf"   "printf_s"
    "vprintf"  "vprintf_s"
    "scanf"    "scanf_s"
    "vscanf"   "vscanf_s"
    "wprintf"  "wprintf_s"
    "vwprintf" "vwprintf_s"
    "wscanf"   "wscanf_s"
    "vwscanf"  "vwscanf_s"
    "cscanf"   "_cscanf"
    "printw"
    "scanw"))
```

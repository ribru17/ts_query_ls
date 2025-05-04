## The Wildcard Node

A wildcard node is represented with an underscore (`_`), it matches any node.
This is similar to `.` in regular expressions. There are two types, `(_)` will
match any named node, and `_` will match any named or anonymous node.

For example, this pattern would match any node inside a call:

```query
(call (_) @call.inner)
```

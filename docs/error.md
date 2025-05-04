## The `ERROR` Node

When the parser encounters text it does not recognize, it represents this node
as `(ERROR)` in the syntax tree. These error nodes can be queried just like
normal nodes:

```query
(ERROR) @error-node
```

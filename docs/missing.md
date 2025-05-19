## The `MISSING` Node

If the parser is able to recover from erroneous text by inserting a missing
token and then reducing, it will insert that missing node in the final tree so
long as that tree has the lowest error cost. These missing nodes appear as
seemingly normal nodes in the tree, but they are zero tokens wide, and are
internally represented as a property of the actual terminal node that was
inserted, instead of being its own kind of node, like the `ERROR` node. These
special missing nodes can be queried using `(MISSING)`:

```query
(MISSING) @missing-node
```

This is useful when attempting to detect all syntax errors in a given parse
tree, since these missing node are not captured by `(ERROR)` queries. Specific
missing node types can also be queried:

```query
(MISSING identifier) @missing-identifier
(MISSING ";") @missing-semicolon
```

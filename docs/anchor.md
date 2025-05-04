## Anchors

The anchor operator, `.`, is used to constrain the ways in which child patterns
are matched. It has different behaviors depending on where it's placed inside a
query.

When `.` is placed before the _first_ child within a parent pattern, the child
will only match when it is the first named node in the parent. For example, the
below pattern matches a given `array` node at most once, assigning the
`@the-element` capture to the first `identifier` node in the parent `array`:

```query
(array . (identifier) @the-element)
```

Without this anchor, the pattern would match once for every identifier in the
array, with `@the-element` bound to each matched identifier.

Similarly, an anchor placed after a pattern's _last_ child will cause that child
pattern to only match nodes that are the last named child of their parent. The
below pattern matches only nodes that are the last named child within a `block`.

```query
(block (_) @last-expression .)
```

Finally, an anchor _between_ two child patterns will cause the patterns to only
match nodes that are immediate siblings. The pattern below, given a long dotted
name like `a.b.c.d`, will only match pairs of consecutive identifiers: `a, b`,
`b, c`, and `c, d`.

```query
(dotted_name
  (identifier) @prev-id
  .
  (identifier) @next-id)
```

Without the anchor, non-consecutive pairs like `a, c` and `b, d` would also be
matched.

The restrictions placed on a pattern by an anchor operator ignore anonymous
nodes.

"MISSING" @missing

"_" @wildcard

(capture) @capture

(named_node
  "." @anchor)

(grouping
  "." @anchor)

[
  "?"
  "+"
  "*"
] @quantifier

[
  "["
  "]"
] @alternation

(negated_field
  "!" @negation)

(definition/named_node) @capture

(named_node
  (identifier) @identifier.node)

(named_node
  name: (identifier) @error
  (#eq? @error "ERROR"))

(missing_node
  name: (identifier) @identifier.node)

(missing_node
  name: (identifier) @error
  (#eq? @error "ERROR"))

(predicate
  name: _ @predicate
  name: (identifier) @predicate
  (predicate_type) @predicate)

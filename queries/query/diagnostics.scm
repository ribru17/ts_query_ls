(ERROR) @error

(MISSING) @missing

(anonymous_node
  (string
    (string_content) @node.anon))

(program
  (definition) @pattern)

(named_node
  .
  name: (identifier) @node.named)

(named_node
  .
  supertype: (identifier) @supertype)

(missing_node
  name: (identifier) @node.named)

(missing_node
  name: (string
    (string_content) @node.anon))

(field_definition
  name: (identifier) @field)

(negated_field
  (identifier) @field)

(capture) @capture

(predicate
  name: (identifier) @predicate
  (predicate_type) @_type
  (#eq? @_type "?"))

(predicate
  name: (identifier) @directive
  (predicate_type) @_type
  (#eq? @_type "!"))

(escape_sequence) @escape

(parameters
  (capture) @capture.reference)

(named_node
  (capture) @capture.definition)

(list
  (capture) @capture.definition)

(anonymous_node
  (capture) @capture.definition)

(grouping
  (capture) @capture.definition)

(missing_node
  (capture) @capture.definition)

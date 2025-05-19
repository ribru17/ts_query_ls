## Negated Fields

You can also constrain a pattern so that it only matches nodes that _lack_ a
certain field. To do this, add a field name prefixed by a `!` within the parent
pattern. For example, this pattern would match a class declaration with no type
parameters:

```query
(class_declaration
  name: (identifier) @class_name
  !type_parameters)
```

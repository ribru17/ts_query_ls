## Alternations

An alternation is written as a pair of square brackets (`[]`) containing a list
of alternative patterns. This is similar to _character classes_ from regular
expressions (`[abc]` matches either a, b, or c).

For example, this pattern would match a call to either a variable or an object
property. In the case of a variable, capture it as `@function`, and in the case
of a property, capture it as `@method`:

```query
(call_expression
  function: [
    (identifier) @function
    (member_expression
      property: (property_identifier) @method)
  ])
```

This pattern would match a set of possible keyword tokens, capturing them as
`@keyword`:

```query
[
  "break"
  "delete"
  "else"
  "for"
  "function"
  "if"
  "return"
  "try"
  "while"
] @keyword
```

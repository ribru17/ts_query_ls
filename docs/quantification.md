## Quantification Operators

You can match a repeating sequence of sibling nodes using the postfix `+` and
`*` _repetition_ operators, which work analogously to the `+` and `*` operators
[in regular expressions][regex]. The `+` operator matches _one or more_
repetitions of a pattern, and the `*` operator matches _zero or more_.

For example, this pattern would match a sequence of one or more comments:

```query
(comment)+
```

This pattern would match a class declaration, capturing all of the decorators if
any were present:

```query
(class_declaration
  (decorator)* @the-decorator
  name: (identifier) @the-name)
```

You can also mark a node as optional using the `?` operator. For example, this
pattern would match all function calls, capturing a string argument if one was
present:

```query
(call_expression
  function: (identifier) @the-function
  arguments: (arguments (string)? @the-string-arg))
```

[regex]: https://en.wikipedia.org/wiki/Regular_expression#Basic_concepts

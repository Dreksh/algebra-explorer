Grammar-parser
==============

This is so that we don't need to manually calculate the transition
tables when we change the grammar.

Running the Program
-------------------

I've used some c++17 (or newer) stuff in there :P

Running it via:
```
g++ main.cpp -o parser -std=c++17
./parser grammar.txt
```

Grammar
-------
Grammar is written in grammar.txt

The format is:
```
<Rule> -> <Token>...
<Rule> -> <Token>...
```

- Blank lines are not supported
- Multi-line definitions are not supported. They need to all be in a single line
- `End` is a special token, and so no Rules are not allowed to be called `End`
- Rules cannot be names as numbers (not enforced, but things will go wrong)

### Elm-side changes

For each rule, the Elm-counter part will need to implement:
1. `type Parser<Rule>`

This allows the storage of state for the rule. It could be a typealias too.

2. `parserRule<Line Number> -> ... -> Result String Parser<Rule>`

This is for collapsing the steps in a rule into the `ParserRule` type.
`String` is our error output. It should be a human-friendly message.

3. `parserError<Line Number> -> Int -> ParserToken -> String`

This is for returning useful error messages when parsing fails.
- `Int` represents the token position (1-indexed)
- `ParserToken` is the received token which the error was made
- `String` is our error output. It should be a human-friendly message.

Functions provided in the elm code include:
- parserTokenName: ParserToken -> String (to get the name as defined in grammar.txt)
- parserGrammarString: String (content in grammar.txt)
- parserParse: String -> Result () Parser<First Rule's Token Name>

In general, all auto-generated code contain `Parser` or `parser` as a prefix
for functions and types, creating a poor-man's namespace.

Token Syntax
------------

There are 2 types of Tokens:
1. Rule - Their names cannot begin with `"` and can contain no spaces
1. String - Their names are wrapped in `"`, and follow a specific syntax

Syntax for String tokens within the quote (similar to, but not regex):

- Plain character (required to be present in the string sequence)
- `[]` to group characters, with `-` representing a range
       (essentially like a either one for that position)
- `+` one or more of the previous plain character or group
- `*` zero or more of the previous plain character or group
- `?` zero or one for the previous plain charactor or group
- `\` escape character, allowing symbols in `[]+*\"` to be specified
- `\o` other unidentified symbols in the whole document (wildcard)

Some missing regex features that won't be implemented:
- `|` or-ing multiple character sequences
- `()` treating a multiple character sequence as a block

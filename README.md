# 🚀 Loadgen DSL

The assert DSL for Loadgen.

## 🌲 Grammer

```ebnf
grammer    ::= brief | full
brief      ::= status? object EOF
full       ::= pair+ EOF
status     ::= number
expr       ::= expr1 (infixop expr1)*
expr1      ::= literal
             | array
             | object
             | funcall
             | prefixop expr1
             | '(' expr ')'
object     ::= '{' fields '}'
fields     ::= (pair (',' pair)* ','?)?
pair       ::= (key | string) ':' expr
key        ::= name ('.' name)+
array      ::= '[' params ']'
funcall    ::= name '(' params ')'
params     ::= (expr (',' expr)* ','?)?
literal    ::= null
             | boolean
             | number
             | regex
             | string
ignore     ::= whitespace
              | comment
              /* ws: definition */

<?TOKENS?>

comment    ::= '//' char*
name       ::= ident - keyword
keyword    ::= 'null'
             | 'true'
             | 'false'
             | 'not'
             | 'and'
             | 'or'
ident      ::= letter ('-' | '_' | letter | digit)*
letter     ::= [a-zA-Z]
prefixop   ::= '-'
             | '>'
             | '<'
             | '>='
             | '<='
             | '=='
             | 'not'
infixop    ::= 'and' | 'or'
null       ::= 'null'
boolean    ::= 'true' | 'false'
number     ::= digit+
               (. digit+)?
               (('e' | 'E') ('+' | '-')? digit+)?
digit      ::= [0-9]
nonzero    ::= digit - '0'
regex      ::= '/' ('\/' | char - '/')+ '/'
string     ::= '"' ('\"' | char - '"')* '"'
             | "'" ("\'" | char - "'")* "'"
char       ::= #x9
             | [#x20-#xD7FF]
             | [#xE000-#xFFFD]
             | [#x10000-#x10FFFF]
whitespace ::= [#x9#xA#xD#x20]+
EOF        ::= $
```

### References:

- [W3C EBNF Notation](https://www.w3.org/TR/2008/REC-xml-20081126/#sec-notation)
- [REx Parser Generator](https://bottlecaps.de/rex/)

## ⚖️ License

Licensed under either of

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or
  <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or
  <http://opensource.org/licenses/MIT>)

at your option.

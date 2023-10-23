# 🚀 Loadgen DSL

The assertion DSL for Loadgen.

## ✍️ Example

`loadgen-dsl` compiles a little DSL to the yaml configuration of Loadgen.

```sh
# Build WASM module and run it from Golang
just run examples/example.dsl
```

Output:

```txt
and:
- equals:
    _ctx.response.body_json.name: lchynn
- range:
    _ctx.response.body_json.age:
      gte: -22.0
- equals:
    _ctx.response.body_json.com: infini
- or:
  - regexp:
      _ctx.response.body_json.team: dev.*
  - regexp:
      _ctx.response.body_json.team: doc.*
- and:
  - equals:
      _ctx.response.body_json.todo.pizza: inprogress
- equals:
    _ctx.response.body_json.todo.dsl: inprogress
- and:
  - equals:
      _ctx.response.body_json.likes.0: anime
  - equals:
      _ctx.response.body_json.likes.1: sports
```

## 🌲 Grammer

```ebnf
grammer    ::= brief | full
brief      ::= status? object EOF
full       ::= pair+ EOF
status     ::= integer
expr       ::= expr1 (infixop expr1)*
expr1      ::= literal
             | array
             | object
             | funcall
             | prefixop expr1
             | '(' expr ')'
object     ::= '{' fields '}'
fields     ::= (pair (',' pair)* ','?)?
pair       ::= path ':' expr
path       ::= key ('.' key)*
key        ::= name | string | integer
array      ::= '[' params ']'
funcall    ::= name '(' params ')'
params     ::= (expr (',' expr)* ','?)?
literal    ::= null
             | boolean
             | integer
             | float
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
ident      ::= id_start (id_start | '-' | digit)*
id_start   ::= [_a-zA-Z]
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
integer    ::= digit+
exponent   ::= ('e' | 'E') ('+' | '-')? integer
float      ::= integer exponent
             | integer '.' integer exponent?
digit      ::= [0-9]
regex      ::= '/' ('\/' | char - '/')+ '/'
string     ::= '"' (escape | char - '"')* '"'
             | "'" (escape | char - "'")* "'"
escape     ::= '\b'
             | '\f'
             | '\n'
             | '\r'
             | '\t'
             | "\'"
             | '\"'
             | '\\'
             | '\/'
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

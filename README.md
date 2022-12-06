# Fundamentalist Functional Programming

An interpreter Alonzo Church's untyped [λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus), written in Haskell.

```
~> Mul 2 (Fac 6)
1440
~> Take 8 (From 1)
[1,2,3,4,5,6,7,8]
~> Map (Exp 2) (Take 8 (From 1))
[2,4,8,16,32,64,128,256]
~> Reverse (Take 5 (Filter Odd Primes))
[13,11,7,5,3]
```

## λ-Term Syntax

Valid λ-terms can be one of the following three things:

* Variables are non-empty strings of lower-case Latin letters: `a`, `b`, `c`, `x`, `y`, `z`, `aa`, `ab`, `ac`, etc.
* λ-abstractions begin with either `λ` or `\`, then consist of a valid variable name and a subterm separated by a dot: `λx.x`, `\y.y`.
* Applications consist of two subterms separated by a mandatory space: `(λx.x x) (λy.y)`.

The standard associativity and precedence rules are observed, that is, applications associate to the left and have a higher precedence than λ-abstractions.
Parentheses may be used to delimit λ-terms.

For convenience, the interpreter maintains an environment of named λ-terms which may be referred to by an identifier, and also accepts numerals, pairs, and lists, which are desugared to an appropriate [Church encoding](https://en.wikipedia.org/wiki/Church_encoding).

* Identifiers are non-empty strings of Latin letters which begin with an upper-case letter. They refer to terms which have previously been entered into the environment using a `~let` command.
* Numerals are non-empty strings of decimal digits, and may represent any natural number.
* Pairs begin `<`, then consist of exactly two subterms separated by a comma, and end `>`.
* Lists begin `[`, then consist of any number of subterms separated by commas, and end `]`.

See the [prelude](lib/prelude) library for many example λ-terms you might recognise.
Some other example scripts are also provided in [lib](lib), such as a solution to [Advent of Code 2022 puzzle 06](https://adventofcode.com/2022/day/6).

## Interpreter Commands

Bare inputs, that is those not beginning with a tilde, will be interpreted as a λ-term, evaluated according to the current evaluation strategy, and printed according to the current printing strategy.
Otherwise, inputs have the following effect:

* `~let i := t`: Add λ-term `t` to the environment where it may then be referenced using identifier `i`.
* `~reductions t`: Evaluate λ-term `t`, printing all intermediate reductions.
* `~script f`: Run a script of interpreter commands read from file path `f`. Commands are executed line-by-line until the end of the file is reached or a parse error occurs.
* `~prelude`: Short for `~script ./lib/prelude`.
* `~eval s`: Switch to evaluation strategy `s`. Available evaluation strategies are `norm`: normal-order reduction, `appl`: applicative-order reduction, and `off`: do not perform any reduction, for example, to inspect a λ-term from the environment.
* `~pprint`: Toggle pretty-printing.
* `~help`: Print a help message.
* `~exit`: Exit the interpreter.
* `~~ comment`: Lines beginning with two tildes will be treated as comments and ignored.

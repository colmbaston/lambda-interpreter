# Fundamentalist Functional Programming

An interpreter for the untyped [λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus), written in Haskell.

## λ-Term Syntax

Valid λ-terms can be one of the following three things:

* Variables are non-empty strings of lower-case Latin letters: `a`, `b`, `c`, `x`, `y`, `z`, `aa`, `ab`, `ac`, etc.
* λ-abstractions begin with either `λ` or `\`, then consist of a valid variable name and a subterm separated by a dot: `λx.x`, `\y.y`.
* Applications consist of two subterms separated by a mandatory space: `(λx.x x) (λy.y)`.

The standard associativity and precedence rules are observed, that is, applications associate to the left and have a higher precedence than λ-abstractions.
Parentheses may be used to delimit λ-terms.

For convenience, the interpreter maintains an environment of named λ-terms which may be referred to by an identifier, and also accepts numerals, pairs, and lists, which are desugared to an appropriate [Church encoding](https://en.wikipedia.org/wiki/Church_encoding).

* Identifiers are non-empty strings of Latin letters which begin with an upper-case letter. They refer to terms which have previously been entered into the environment using a `~let` command.
* Numerals are non-empty strings of decimal digits.
* Pairs begin `<`, then consist of exactly two subterms separated by a comma, and end `>`.
* Lists begin `[`, then consist of any number of subterms separated by commas, and end `]`.

See the [prelude](lib/prelude) for many example λ-terms.

## Interpreter Commands

Bare inputs, that is those not beginning with a tilde, will be interpreted as a λ-term, evaluated according to the current evaluation strategy, and printed according to the current printing strategy.
Otherwise, inputs have the following effect:

* `~let I := T`: Add λ-term `T` to the environment where it may then be referenced using identifier `I`.
* `~reductions T`: Evaluate λ-term `T`, printing intermediate reductions on the way to the normal form.
* `~time T`: Evaluate λ-term `T`, timing the evaluation.
* `~count T`: Evaluate λ-term `T`, counting how many reductions normalisation takes.
* `~script F`: Run a script of interpreter commands read from file path `F`. Commands are executed line-by-line until the end of the file is reached or a parse error occurs.
* `~prelude`: Short for `~script ./lib/prelude`.
* `~eval E`: Switch to evaluation strategy `E`. Available evaluation strategies are `norm`: normal-order reduction, `appl`: applicative-order reduction, and `off`: do not perform any reduction, for example, to inspect a λ-term from the environment.
* `~pprint`: Toggle pretty-printing.
* `~help`: Print a help message.
* `~exit`: Exit the interpreter.
* `~~ comment`: Lines beginning with two tildes will be treated as comments and ignored.

## Examples

```
~> 2 2 2 2
65536
```

```
~> Swap <<1,2>,3>
<3,<1,2>>
```

```
~> Reverse (Take 5 (Filter Odd Primes))
[13,11,7,5,3]
```

# Fundamentalist Functional Programming

An interpreter for the untyped [λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus), written in Haskell.

```
~> 2 2 2 2
65536
~> ~prelude
attempting to run script ./lib/prelude
script executed successfully
~> Fac 6
720
~> Take 5 Primes
[2,3,5,7,11]
~> ~pprint
pretty-printing off
~> Take 5 Primes
λp.p (λf.λx.f (f x)) (λp.p (λf.λx.f (f (f x))) (λp.p (λf.λx.f (f (f (f (f x))))) (λp.p (λf.λx.f (f (f (f (f (f (f x))))))) (λp.p (λf.λx.f (f (f (f (f (f (f (f (f (f (f x))))))))))) (λx.λt.λf.t)))))
~>
```

## λ-Term Syntax

Valid λ-terms are one of the following three things:

* Variables are non-empty strings of lower-case Latin letters: `a`, `b`, `c`, `x`, `y`, `z`, `aa`, `ab`, etc.
* λ-abstractions begin with either `λ` or `\`, then consist of a valid variable name and a subterm separated by a dot: `λx.x`, `\x.x`.
* Applications consist of two subterms separated by a mandatory space: `(λx.x x) (λx.x)`.

The standard associativity and precedence rules are observed, that is, applications associate to the left and have a higher precedence than λ-abstractions.
Parentheses may be used to delimit λ-terms.

Non-empty strings of decimal digits will be parsed according to the [Church encoding](https://en.wikipedia.org/wiki/Church_encoding) for that natural number; for example, `3` is syntactic sugar for `λf.λx.f (f (f x))`.
Additionally, the interpreter maintains an environment of named λ-terms which may be referred to by an identifier:

* Identifiers are non-empty strings of Latin letters and decimal digits which begin with an upper-case letter.
* The environment of named λ-terms may be extended using a `~let` command: `~let Identity := λx.x`, `~let Y := λf.(λx.f (x x)) (λx.f (x x))`.

See [lib/prelude](lib/prelude) for many example λ-terms.

## Interpreter Commands

Bare inputs, that is those not beginning with a tilde, will be interpreted as a λ-term, evaluated according to the current evaluation strategy, and printed according to the current printing strategy.
Otherwise, they will have the following effect:

* `~let I := T`: Add λ-term `T` to the environment where it may then be referred to using identifier `I`.
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

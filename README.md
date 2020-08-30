# Fundamentalist Functional Programming

An interpreter for the untyped [λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus), written in Haskell.

## λ-Term Syntax

Valid λ-terms can be one of the following three things:

* Variables are non-empty strings of lower-case Latin letters: `a`, `b`, `c`, `x`, `y`, `z`, `aa`, `ab`, etc.
* λ-abstractions begin with either `λ` or `\`, then consist of a valid variable name and a subterm separated by a dot: `λx.x`, `\y.y`.
* Applications consist of two subterms separated by a mandatory space: `(λx.x x) (λy.y)`.

The standard associativity and precedence rules are observed, that is, applications associate to the left and have a higher precedence than λ-abstractions.
Parentheses may be used to delimit λ-terms.

Non-empty strings of decimal digits will be parsed as [Church numerals](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals); for example, `3` is syntactic sugar for `λf.λx.f (f (f x))`.
Additionally, the interpreter maintains an environment of named λ-terms which may be referenced by an identifier:

* Identifiers are non-empty strings of Latin letters and decimal digits which begin with an upper-case letter.
* The environment of named λ-terms may be extended using a `~let` command: `~let Identity := λx.x`, `~let Y := λf.(λx.f (x x)) (λx.f (x x))`.

As λ-terms may be difficult to read, the interpreter can pretty-print some common structures:

* If the λ-term is a numeral, it will be displayed in decimal notation.
* If the λ-term is a pair, the components of the pair will be recursively pretty-printed, and the resulting strings formatted between angle brackets `⟨X,Y⟩`.
* If the λ-term is a list, the elements of the list will be recursively pretty-printed, and the resulting strings formatted between square brackets `[X,Y,Z]`.

For example, the λ-term `λp.p (λf.λx.f (f (f x))) (λf.λx.f a (f b (f c x)))` will be pretty-printed `⟨3,[a,b,c]⟩`.
Since the empty list and the numeral `0` share the representation `λf.λx.x`, with no type information to distinguish them, they will both be displayed as `0`.

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

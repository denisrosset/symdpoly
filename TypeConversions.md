About type conversions
======================

Monomials (`Mono`) are also polynomials (`Poly`) with a single term.

However, Scala has a static type system, and we need to leave the ambiguities in the syntax (nobody wants to explicitly convert monomials to polynomials when required), but resolve these ambiguities at compile time.

We use the following approach.

- All types that can be embedded in a `Poly` extend a trait `PolyLike`. The trait `PolyLike` defines all methods that need to 'upgrade' the return value to `Poly`, for example addition or subtraction.

- Primitive types such as `Int` cannot be extended in this way, and the multiple overloads of `+` conflict with our conversions. Thus, we add to `PolyLike` the method variants that take an `Int`, `Rational` or `Cyclo` right hand side. This allows expressions such as `mono + 1` .

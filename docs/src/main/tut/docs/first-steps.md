---
layout: docs
title: Basic Parsers
---

### {{page.title}}

In this chapter we will learn the basics of using primitive parsers, and combining them to build larger parsers.

But first we need to import some stuff. Fine-grained imports are supported but it’s usually fine to just import everything. We also need cats implicits for applicative syntax below.

```tut:silent
2
```

Rock on, let's parse an integer!

```tut
2
```

This result means we successfully parsed an `Int` and have the text `"abc"` left over. We'll talk more about this momentarily. But let's back up. What's this `int` thing?

```tut
2
```

A `Parser[A]` is a computation that consumes characters and produces a value of type `A`. In this case `Int`. Let's look at another predefined parser that matches only characters where `isLetter` is true.

```tut
2
```


# Journey to web-dev with F#

This is a series of blogpost that follow the journey of developming my first web app for my computer science thesis project, using functional paradigms with F#.

I hope to share and record my thoughts and ideas, both the pre-existing ones that influenced the conception of this project, and the newcomers that I pick up along the trail. Read on if you are interested in the perspective of someone coming from a c++ and c# desktop dev background to the wild west of web-dev and functional programming.

## 1. [Motivation](motivation.md)
Where you can read all about the motivation behind this webapp in general.
## 2. [SAFE Stack](safe_stack.md)
A quick overview of the stack used and it's main promises.
## 3. [About F#](fsharp.md)
Overview of the language from my (C# dev) perspective, what makes it noteworthy and a good fit for this project.
## 4. [Sum types](sum_types.md)
Extra thoughts on the one killer feature of the functional paradigm, that I miss every day when working in non-functional languages.
## 4. [Parser combinators](parser_lib.md)
Notes on integrating Scott Wlaschin's simple parser combinator implementation into the project.
## 4.1 [Parser combinators - trouble with my concept](parser_lib_ruh_roh.md)
I explore a critical problem with my original vision of combinators with parametrized labels. This is also a not all is perfect in F# kind of post, to show that all my other praise is not just blind cheerleading.
## 5. [SQL AST: the core idea](sql_ast.md)
A rundown of my core idea for the domain model of the webapp, and how can it be implemented with the tools a functional programming language provides.
## 6. [SQL AST: parsing](sql_ast_parse.md)
Notes about parsing this AST with parser combinators, and unparsing it into sql command strings to drive our database.
## 7. [Testing with a functional frist mindset](testing_functional.md)
Quick glance at unit testing with the function combinator design pattern.
## 8. Shared web API definition - TODO
How does code sharing work, and how it leads to frictionless and less error prone communication between front- and back-end.
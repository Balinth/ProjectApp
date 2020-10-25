# F# - functional first language on .Net
Note that this is not aimed to be an actual introduction to the language itself, but a collection of my notes on it coming from a C# desktop development background.
## Briefly:
F# incorporates parts of FP languages like Haskell, while also retaining the OOP object model of .Net, and is compiled to said environment.

The former means algebraic type system, first class functions, and a path-of-least-resistance that leads to code with very little object mutation, that in turn is easier to reason about.

The latter means easy integration of the entire .Net ecosystem, using Newtonsoft.JSON is just as simple as is from C#, while consuming a class library written in F# from C# can be just as easy (but not always, as F# provides a "wider" set of language features than C#, which are still usable, but ugly in C# without language support)

## Three major differences
### 1. Language support for "sum types"
Or so called discriminated unions. Later I will go into more detail on these. Given an algebraic comprehension of types, what we have in C#, C++ and the likes, are product types. Meaning that an int is the set of all integer values that can be represented in the datatype. A stuct containing two int fields is likewise a set of all the possible values of the first field, multiplied with all the possible values of the second field, hence the name product type. Now, a sum type of two int fields, called "cases" describes that at any one point in time, the object of this type either holds the first case, or the second. The set of its possible values is all the possible values of the first case __plus__ all the possible values of the second case, hence sum type. Note that the component cases can be different shapes, eg an int or a string, in which case at any one point the object will be either an integer or a string, but not both, unless there is a third case that is a product type of an int field and a string field.
### 2. Syntactic differences
The main one visually: scope is indicated with indentation depth, instead of {} braces.
Conversely, I feel that this is fairy inconsequential, while I miss sum types every day at my work with C#, I never really lamented the need for bracing scopes, nor the freedom of just using indentations, it is just such an easy muscle memory thing either way.

`=` is not the assignment operator you are looking for. While following a let statement, eg `let a = 1` you just indicated, that from now on, if you write a, you want to insert 1 there, like in maths. It does not mean that you actually assigned the value 1 to a stack variable of a, thus mutating the "variable" contents. Eg.: if I write another `let a = 2` line, both 1 and 2 will still be in the memory, I just indicated, that the next time I pass `a`, I want to insert the value 2 into its place.
Outside of `let` statements, `=` is the equality conditional operator, and `<>` is the inequality one while we are at it. (if you actually use a mutable variable, `<-` is the mutating assignment)

### 3. Differences in default behavior
Every language has a host of decisions, and a provided default state for most of them to ease development. Eg.: in C# methods can be virtual but by default they are not, while in java, they are virtual by default.

In F# most default choices are made with maintainability and easse of reasoning in mind.
These defaults are mostly "encapsulated" by providing a "Record" type alongside the more OOP oriented class-es.
A Record is by default:
1. Immutable
2. Only constructible with all fields initialized
3. Uses structural equality
4. A reference type

Outside of records, noteworthy:
1. Local "variables" are immutable by default
2. The language standard libraries provide immutable collection types (cons list, AVL tree based map) that are cheap to add to, without modifying the originating objects.

# Actual introduction to the language
If you look for a real place to start, where you can get knee deep in functional programming, and learn about its concepts, while also learning the language, one of the best places I found is Scott Wlaschin's [F# for fun and profit](https://fsharpforfunandprofit.com/) blog / educational site.

Another good starting point is [FShart.org](https://fsharp.org/).
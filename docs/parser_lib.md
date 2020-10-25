# Parsing in a Functional language
As part of my domain centers around the manipulation of SQL ASTs, I also needed a way to parse arbitrary user strings into said ASTs. 

In functional languages, the idiomatic way for string parsing is to use so called parser combinators, instead of regular expressions. In this post, I intend to showcase integrating one into my webapp.

In F# one would normally use the excellent [FParsec](https://www.quanttec.com/fparsec/) library, which provides a broad set of heavily optimized combinators. But in this case, it would be very nice if I could use the same parsing code both on the client SPA, and on the .Net Core backend. Unfortunately Fable cannot digest FParsec, due to it's usage of C# for performance optimizations.

There are a few F# only combinator repos on github, but none of them seemed too lively, so I decided to just roll my own solution, starting from Sctott Wlaschin's excellent blog series:
[Understanding parser combinators](https://fsharpforfunandprofit.com/posts/understanding-parser-combinators/).
This series, and the whole fsharpforfunandprofit blog is one of, maybe even the best learning resource I found for someone without prior knowledge of functionaly programming.

Of course, as the author notes, the combinator library is not mean to be production ready by the end of the series, so we will have to make a number of adaptations. While we are at it, I also introduce parametrizable parser labeling, so instead of raw strings, I can use my own types as parser labels, that can be later pretty printed in language specific ways.
## Adapting Scott's teaching implementation:
The changes encompassed by my initial adaptation are encompassed in the following [commit](https://github.com/Balinth/ProjectApp/commit/9b065202d9a7a4b8a7a71d43858e3d8c0f5d9849#diff-4c35d28a383bad3443b19b98bc39243210c73de215e1977a50449002568ecf4d). The main changes are:
* Polish to make the combinators more production ready, like handling overflow while parsing numbers, or refactoring recursive functions to be tail recursive / use loops to avoid potential stack overflows.
* Parametrized parser label and parser errors, so I can use project specific types instead of hardcoded strings. This is especially important because I want to use and expose the parser functionality in the multi language UI, and was one of the reasons behind rolling "my own" parser combinator library.
### Refactoring to tail-recursive functions:
One of the changes I had to make, was to the `parseZeroOrMore` helper function of the `many` combinator. The problem with the original implementation, is that it is defined as a recursive function, that effectively uses stack space linearly with the number of successful parsings. Now of course, as recusrive functions are nothing new, and most languages including F# has the so called tail call optimization, where if the functions last expression is the recursive call, then the compiled code will use constant stack space for the given function.

For me, it is not always obvious how I should go about redefining a recursive function to be tail recursive. What I found to help in these cases, is to first refactor the function to a more imperative style which is familiar (and note that in F# you can stop here, because F# is functional _first_ but has all the tools for you to fall back to imperative style where it is just more natural) and then, It just clicked how I should refactor this imperative version into a tail recursive one. See the listing below with the three progressive variants.
{% highlight fsharp %}
/// (helper) match zero or more occurences of the specified parser
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = runOnInput parser input 
    // test the result for Failure/Success
    match firstResult with
    | Error (_,_,_) -> 
        // if parse fails, return empty list
        ([],input)  
    | Ok (firstValue,inputAfterFirstParse) -> 
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues,remainingInput) = 
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput)  

let parseZeroOrMoreLoopred parser input =
    let mutable values = []
    let mutable input = input
    while (
            match runOnInput parser input with
            | Error _ ->
                false
            | Ok (newValue, remainingInput) ->
                values <- newValue :: values
                input <- remainingInput
                true
            
        ) do
        ()
    (List.rev values,input)

let rec parseZeroOrMoreTailRecursed values parser input =
    match runOnInput parser input with
    | Error _ ->
        (values, input)
    | Ok (newValue, remainingInput) ->
        parseZeroOrMoreTailRecursed (newValue::values) parser remainingInput
{% endhighlight %}
### General notes on refactoring in a functional language
The diff of the referenced commit seems daunting, but this is something that I found F# excels in (and I suspect the case is the same in any language that is more functional oriented like Scala, Rust and obviously, Haskell). 

The strong, algebraic type system, with the automatically generalizing type inference and default immutability combined, makes refactoring and extending existing code much more straight forward, with less hidden surprises all around. It is hard to convey in words the effect these combined make. But my experience with extending the entire parsing solution with parametrized labels was that I spent very little time wondering if I unwittingly broke something that will only rear its head at runtime. After changing the most basic functions to the new form, two things happen:
* Due to the strong type system, and the default way where almost all functions end up pure, the static type check highlights all basic errors, I just had to handle some of the next most basic functions, and most of the higher abstraction ones still "just worked" as is, as there were no fixed types, and often they are general enough that through type inference, their new type constraints just "wash through".
* The more interesting experience, is that since you don't normally use type annotations, and the inferred types are automatically generalized to be the most general definitions possible, I often did not end up with an outright error message, but just looking at the more complex combinator functions' inferred type signature, I could see that what I wrote will behave differently, than what I expected. Eg.: after the first pass, when everything compiled, I noticed that my `orElseL` combinator restricted the the `labelFunc` parameter to be `'a -> 'a -> 'a` meaning that ultimately I could only orElse together two parsers that have the same label type, whereas I expected to be able to orElse together parsers of any kind, and let the output label type be decided by the parameter function, eg it should have had a type of `'a -> 'b -> 'c`. I went through roughly 90% of the changes in one go, and when I finally started actually testing out the result, almost everything worked on the first try. Not because I am awesome, but because refactoring and extending code in a functional language is!

To compare and contrast, in my daytime job developing a FEA software, that has 20 years of history and a mainly C++ code base, refactoring is often a nightmare. Constantly figuring out what other internal mutations affect the state that the refactored code also touches, and missing even the most minor of those causes runtime meltdowns, often not at all trivial bugs. Of course there is nothing stopping me from doing my due diligence, and spending ours "Find All References" -ing everything constantly, but we are only human and make mistakes. As F# is a multi paradigm language, and supports everything that an OOP language like C# does, you can very well write hard to maintain OOP code in it. But the point is, that the language is functional first. The default conventions, the path of least resistance while writing code naturally result in code that is easier to maintain.
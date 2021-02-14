# Testing, with a functional mindset

## Key point:
There is not a single important difference compared to OO languages, but a collection of small feature differences which add up. In a functional language, almost every design decision of the language helps with unit testing by default, the same ammount of testing take less effort, or providing much more rigorous testing opportunities for a given ammount of effort.

Some examples of the above observation:
1. FP languages favor pure functions, which are the easiest things to unit test.
2. FP languages favor immutability, which again helps with isolation and testability.
3. The algebraic typesystem, and exhaustive pattern matching on sum types helps ensure exhaustive type case coverage
4. As interfaces are just records with function fields, and defining ad hoc functions is favored, mocking up interfaces in place for a test is very easy and clean.
5. In reality the tests themselves are just functions that need not be members of a class, making test composition and combining very natural.

Admittedly I do not yet have experience with a large scale OO code-base's unit testing, all the above points are just from small scale experiences. I would imagine that for mor complex cases, there are other advantages I haven't experienced yet.

## Concrete FP unit testing lib: Expecto
In this project, I use the F# specific library [Expecto](https://github.com/haf/expecto).

This library has everything one would _expect_ from a FP unit testing library, support for algebraic typesystems, zero need to define dummy and placeholder classes, no need to use compiler attributes and so on.

The library's architecture is not unlike the parser combinator concept seen previously. Every test is a function, and tests can be combined, created, ignored, and manipulated with higher order functions (combinators).

The simples example of the combinator concept, is the grouping of testcases with the ```testList``` combinator. This is just a higher order function, that takes a list of test functions, and a name for the collection, and returns a single new test function.

A more complex example of this concept, is the parsing and unparsing testing done in this project. At first I just wrote a simple combinator that took a string, and the expected value, and returned a test function, with a name based on the inputs, and internal implementation calling the parser functions to be tested. With this combinators I could easily, and cleanly define the basic list of test cases:
```
    exprTestCase "0 < 1" true
    exprTestCase "0 <= 0" true
    exprTestCase "0 < -1" false
    exprTestCase "0 > 1" false
    exprTestCase "1.0 = 1" true
    exprTestCase "1.1 = 1" false
    exprTestCase "0.9 = 1" false
    exprTestCase "1 = 1.0" true
    exprTestCase "1.1 = 1.1" true
    exprTestCase "0 <> 1" true
    exprTestCase "0.5 <> 0.5" false
    exprTestCase "false and 1=1" false
    exprTestCase "1=1 and false" false
    exprTestCase "1+1*2=3 and 3-1+2=4" true
    exprTestCase "1+1.5*2=4.0 and 3-(-1+2.5*2)=-1.0" true
    exprTestCase "\"1\"=1" true
    exprTestCase "1=\"1\"" true
    exprTestCase "\"1\"=\"1\"" true
    exprTestCase "\"fisfos\"=\"fisfos\"" true
    exprTestCase "\"a\"<\"b\"" true
    exprTestCase "\"1\"=1 and 1.0 = 1 and 1 = 1.0 and " true
```
But wait, there is more!

After the initial run, there were of course some failed tests, due to white space that was not always accounted for in the parser. Fixing the whitespace problem that was found with the initial tests, I tought about how to make sure to catch ALL the possible white space cases? The solution: a new ```withWhitespacePermutations``` combinator, that takes as input a test function, its expected result, and its input string, computes the whitespace permutations of the input string, and uses the ```testList``` combinator to combine running the original test function with all whitespace variants of the input string. Then it was as easy to just insert this combinator inside the previous `exprTestCase` combinator and boom! now I have multiplied the unit test cases to account for various cases of the optional whitespaces, with minimal work.

But wait, there is even more!
The next thing to test was the unparsing, that is, converting the parsed AST back into its string representation. Of course I already have a bunch of test cases defined so hopefully you can see where this is going... lets make another combinator, and insert it into the pipeline inside the `exprTestCase` combinator. Thus the `roundtripTest` combinator was born. Its inputs are: the "forward" and "backward" functions (eg the parser and unparser function), and the input. Its output is a new test function, that runs the forward function on the input, saves the result, runs the backward function on the result, and then runs the forward function on the result again, finally checking if the result of the forward function was the same both times. Inserting this into the `exprTestCase` combinator, we again multiply our actual tests. For every single manually defined test case in the form of ```exprTestCase "0 < 1" true``` I get `a*2` as many real executed tests, where `a` is the number of whitespace permutations and 2 is the sum of the normal value testing and the round-trip testing.
# SQL AST: parsing

Let here be some of the more interesting techniques needed to parse something like SQL. The most interesting, and also ubiquitous problem of languages like this is the expression parsing. 

An example expression grammar could look like this in EBNF:
```
Expr -> Value | Expr BinaryOp Expr | OpenBrace Expr CloseBrace
```
Note that this can be directly mirrored with discriminated unions in F#, in fact the syntax is almost the exact same
```
type Expr =
    | Value of Value
    | BinaryExpr of Expr * BinaryOp * Expr
    | BracedExpr of Expr
```
Note also that this innocent looking definition is not just recursive but left recursive in the BinaryExpr case.

## 1. Creating recursive parsers that produce recursive structures

The problem that I as someone new to functional programming ran into is that parsers like everything else are immutable by default, and with good reason. Of course this means, that I could not just define a parser in terms of itself. For these cases the solution is the `createParserForwardedToRef<'a,'parserError>()` function. This function returns a tuple of a parser and a mutable reference cell containing a parser. The returned parser itself also contains a reference to the mutable ref cell and forwards all calls to the parser contained within. This way one can define the recursive parser in terms of the immutable parser, then insert it into the mutable reference cell to complete the definition.

Eg.: in terms of the example grammar above:
```
let exprP, exprPRef = createParserForwardedToRef<Expr,unit>()
let binaryExprP = exprP .>>. binaryOpP .>>. exprP
let bracedExprP = openBraceP .>>. exprP .>>. closeBraceP
exprPRef := valueP <|> binaryExprP <|> bracedExprP
```

After this, we can use `exprP` like we would a normal parser, because it is!

Except... if we tried to use it, we would run into an infinite loop, which leads us to the second point.

## 2. Handling left-recursion in the grammar.

### The problem with left-recursion:
If we take the previous naive implementation and try to follow it through on the following example: `1+1` then the exprP calls the parser enclosed in it's mutable reference, which is `valueP <|> binaryExprP <|> bracedExprP` in effect. Now this parser will succeed on the `valueP` part and return with a successful parsing of `1` ... not what we wanted!

You might think that just ensuring the ordering goes from more complex to simpler choices fixes the problem, like so: `binaryExprP <|> bracedExprP <|> valueP`. Now this parser on the previous input will start with a `binaryExprP` try which is what we want, but oh no! now we ran into an endless recursive loop (and a stack overflow exception on .Net).

To see why is this, we can follow the parser definitions. First we call `exprP` which inside calls the first choice: `binaryExprP`, but binaryExprP inside is defined as  `exprP .>>. binaryOpP .>>. exprP` meaning we get back to `exprP` where we started without actually doing anything.

This is a well known problem, with its own [Wiki page](https://en.wikipedia.org/wiki/Left_recursion). The gist of the solution is to not define the expression grammar in terms of single value or binary expression, but in terms of a `term` of single value followed by `nothing` or operator and another `expression`" our simple example refactored to this:
```
let exprP, exprPRef = createParserForwardedToRef<Expr,unit>()
let bracedExprP = openBraceP .>>. exprP .>>. closeBraceP
let termP = valueP <|> bracedExprP
exprPRef := termP .>>. (binaryOpP >>. termP |> many)
```

Note that this considers every operator to have the same precedence. The key point, is that in this form we always consume some input before stepping to the next recursive call thus avoiding the endless looping.

If we also wish to take into account operator precedence, we can start with the highest precedence operator as described, and for every lower level of precedence use the next higher one as the `term`. So with multiplication and addition:
```
let mulExprP = termP .>>. (mulP >>. termP |> many)
let addExprP = mulExprP .>>. (addP >>. mulExprP |> many)
exprPRef := addExprP
```
Where did the expression tree go? you might ask, as with this implementation we get a `list` of the following op*term tuples. We can easily fold the list into a tree like so:
```
let mulExprP = 
    termP .>>. (mulP >>. termP |> many)
    |>> (fun (firstTerm,multipliers) -> 
        List.fold (fun prevExpr multiplier -> BinaryExpr(prevExpr,Mul,multiplier)) firstTerm multipliers)
```
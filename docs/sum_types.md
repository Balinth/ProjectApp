# Sum types: the missing link between domain models, and OOP:

## Brief refresher:
As mentioned in my [About F#](fsharp.md) post, sum types are types where at any one point, the value of an object is of a type of either one of the type's predefined cases. In F# we specifically have Discriminated Unions, in which we can further discriminate between cases by a "tag" or name, so in the following DU:
```
type AccountSource =
  | GoogleID of Guid
  | AzureID of Guid
  | InternalID of Guid
  | LegacyID of string
```
the Guid cases are further discriminated by their tags, eg `GoogleID` and so on, resulting in a sumtype whose set of all possible values is all possible values of a Guid added together 4 times, plus all possible values of a .Net string.
## Sum types vs Interface types
Firstly, it is important to note, that you can't do anything with one, that the other is incapable of. They are _duals_ of each other. (see Visitor pattern).

Now then, what is the actual conceptual difference, from the perspective of software architecture? Firstly, when I think software architecture, I think the following: __"good software architecture is about delegating decisions to the point where the most informed choice can be made"__. Sadly I have no idea where I read this, but it stuck with me as the most true-ringing definition.

With this in mind, we can look at __Interfaces__, where at creation I decide what a thing can __do__ and delegate deciding what this thing can __be__ to a later time. One good example is a sequence interface, I can signal in my function signature, that this functions expects a sequence of integers. What kind of sequence? not my decision, maybe a simple array list, or a BST set, but importantly, I do not have to know to be able to write out my function, a later consumer of said function can decide what exactly the sequence of integers is.

Conversely with __Sum types__ at creation I decide what a thing can __be__ and delegate deciding what will be __done__ with it to a later time. One good example is handling domain errors via a result type. When I write a function that has expected failure paths (like deserializing from external source) the decision that I can correctly make, is "what the errors can __be__ of this function?" and at a later time, the function's future callers will have the information to decide what they want to __do__ about the errors.

Now, I wish to expand upon the error returning example, because this is one of the most ubiquitous example, present in most complex projects in some form. Unlike the syntactic differences that turned out to be not really noticeable for me, almost every day I happen upon a problem when working with C like languages, where I wish for sum types. Staying at the error returning, lets take a look at the OOP solutions for this problem:
* The most basic solution is to just return null in the fail case, and comment that you must check the return value for null... Is it barbaric? highly human error prone? used by a wide variety of code written in C style languages? Yes all of the above. 
 * A slightly better solution is the TrySomething(output variables via indirection) pattern, where the function returns a boolean value, indicating success or failure, and it has output parameters that are filled in according to the correct case. This is minimally better in the aspect that the developer is instantly made aware that a function can fail, just by it's signature, but still has no information about the different failure cases, outside of the hope for good documentation. 
 * Another solution used are compile time checked exceptions (ala Java). This is better, in some cases even a good solution (and I think it is one small point where C# misses out compared to Java) but, on functions that are used in many places, it often gets very cluttered, because it is not really part of the type system, leading to a "path of least resistance" where the developer just catches every exception, and introduces hidden failures to the software, which rear their ugly head later down the line, where it is often harder to recover, and even harder to track the original problem source.

 * The solution in functional languages: use a sum type as the function's return type, signifying that the possible values returned by the function is the sum of all possible success case values and all possible error case values. In F# this is done through Discriminated Unions, where one case is the success case, and another, the error case itself can be a nested D.U. containing all the different errors. This is nice because:
    * We have directly expressed that a function can fail through its type signature.
    * This is just right in your face information, but statically type checked information eliminating human errors
    * We don't just signify that a function can fail ala TrySomething patter, but also codify the expected failure cases in the return type
    * We delegate what should be done about the failure case. Maybe the caller want to throw an exception, or silently log the problem and insert a fallback value in the process.
    * Furthermore, in functional languages the Sum Types are supported by the whole of the language, enabling a fluent handling or further delegating the returned values, whether they happen to be a success or failure case. The best example of this is ROP or railway oriented programming.

To further evaluate the above example: say there is a deserializer functionality in the domain. I can easily write a function that returns a `Result<DomainObject,DeserializationError>` type, where `DeserializationError` is itself a discriminated union of cases like: 
```
type DeserializationError =
    | UnexpectedEndOfStream
    | NumericOverflow of string
    | UnsupportedLegacyVersion of SemanticVersion
    | MandatoryAttributeMissing of (objID:Guid*attrName:string)
```

the important thing to note, that a programmer later consuming the given function through automated static type checking is
 * made aware that the function can fail
 * made aware of all the ways its expected to fail
 * if the function's API changes, is made aware of the new failure cases (exhaustive matching)
 * can directly use all the data of the specific failure cases, while the cases can differ in their "shape" maybe in some case there really is no more data than a string message. Maybe the failure case contains an object ID that was problematic, and an attribute name that caused the problem, all typesafely ensured by the compiler preventing human oversight.
 
 This also means the failure case's consumer can easily pretty-print the problem details according to the current culture, language, etc from the data that makes up any of the particular cases, unlike with exceptions, where most often at the moment of throwing the exception, the function's publisher has to create a reasonable message string, without knowledge about the consumer's culture. Or worse, has to provide extra exception data in a `Dictionary<string,object>` (in c#) or the like, which of course involves then magic strings, unsafe casts and absolutely no help from the compiler if the "interface" of the expected errors changes.

 All this is just one simple but abundant example where there are sum types in domains, but they can be just as well the best  representation of the domain problem itself.
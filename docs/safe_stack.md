# SAFE stack
## What is it?
1. Saturn (functional wrapper around Asp.Net Core backend)
2. Azure (Microsoft's cloud provider, in this case as a PaaS)
3. Fable (F# on the frontend through transpilation to javascript)
4. Elmish (Functional UI framework based on the Elm language)

## Some key observations:
- Both for the back end, and the cloud platform, the stack is actually agnostic, one can just as easily do everything on Google Cloud, or AWS, and likewise, one can directly use Asp.Net Core, or any other .Net Core based backend solution (for F# Giraffe is another nice option).
- The main root level promise of the stack, is that both the frontend, and the backend is written in the same language. This has a number of benefits:
  - The most obvious is code sharing. The client app can use the same input validation code to give feedback to the user that the backend will use to actually validate the incoming requests, and so on.
  - A less obvious one is the explicit statically typechecked communication over http, for which I will show more indepth examples, but there are no more magic strings, mismatched API endpoint paths or response formats and the like.
- Of cource you can achieve the zen of using the same language on both sides by Node.js. To my distaste of developing anything complex in a loosely typed language speaks the promise of not just using any old language on both sides, but one with a relatively strong algebraic type system. The benefit of which I also intend to explore and showcase in depth in future posts.
- Finally, the Elmish UI framework promises a functional alternative to the object oriented MVC designs. This being the so called model-view-update or MVU pattern. The main selling point of this pattern, in combination with a functional language like F# is that it is much harder to write 'bad' code. In fact, one of the slogans of the original Elm language is that if it compiles, then it won't throw unexpected errors and fail at runtime.

If you wish to know more about the stack itself, it has it's own documentation, dotnet template, examples, and the like, found [here](https://safe-stack.github.io/docs/overview/)
# SQL AST: the core idea
As a quick rundown, the goal of the webapp is to replace the emailed excel sheet based proejct administration and communciation at an engineering office.

My core idea, is to create a front-end which can still be manipulated as an excel sheet, pasting in or copying out arbitrary rows and columns, creating arbitrary views, or "sheets" by hiding rows and columns to only share relevant data, and so on, while on the back-end provide a relational database for all the data, against which complex queries can be easily made, and said query results can again be easily manipulated and updated back into the database.

The key insight here that unlike a more traditional line of business app, all the "views" have to be dynamic. Making an AddAssembly view, an UpdateAssembly view and so on, organizing it into menus would take out the main reason the engineers still use the spreadsheet based workflow, because that view is very convenient for the kind of data entry and manipulation that is representative of the domain.

## Implementation idea: the SQL AST as part of the domain model
To fulfill these requirements, I implement a basic abstract syntax tree of a subset of SQL handling the various use cases through manipulating this AST.
* Dynamic views are encoded as specific SQL commands, that can be shared, updated, etc.
* Authorization is handled through modifying the SQL command encoding the current use case with SQL representing the user's rights. (eg.: if a user only has read rights to a specific project, and nothing else, every SQL query originating from their actions will be extended with an AND selection part filtering out only the allowed projects.)
* Updating the DB is done through auto generated SQL commands when the user modifies the displayed spreadsheet cells.
* Parsing SQL queries written by the users, we can also add advanced input and query interfaces for the users in a managed fashion, where in the background we can intelligently sanitize, constrain and repair the SQL AST itself before actually sending it for execution.
* Saving a continuous log of the executed SQL commands, we can later audit how the database got into it's current state, rebuild it from day 0 if needed (event sourcing like)

## Functional programming enters the scene
F# and other functional languages are extremely good at representing and manipulating various AST-s mainly due to the sum types that I outlined in previous posts, combined with all the supporting language features like pattern matching and higher order functions.

This allows me to encode the supported SQL AST in a concise and safe type system where most invalid cases simply can not be represented at all giving compile time errors rather than runtime problems in production. Furthermore, one of the most important functionality, parsing user input strings into this AST also can be coded more concisely, safely, with better error reporting and easier multi language support using parser combinators idiomatic in functional programming.

I choose F# specifically because it is implemented in the .Net ecosystem allowing me to use .Net Core on the backend and it has an excellent F#-to-JS transpiler (Fable) that allows me to use the same language both on front and back end. The latter of course means that I can use the same types that so well describe the AST, and parse it, run the same validations on it I would run on the backend, giving latency feedback to the users. (Of course as the client side can be compromised, the backend will always run it's own validations).
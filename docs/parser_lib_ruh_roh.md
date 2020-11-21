# Parser library - trouble with my concept
As I alluded to in [Parser combinators](parser_lib.md) one of my adaptations is to parametrize the label type of the parsers and combinators, instead of hardcoded english strings.
In my original adaptation, this meant that most combinators had a non labeling simple version (with `unit` as the label type) that could be used in the library itself and when the label will be overridden anyway and extended labeling versions (eg for orElse there was an orElseL) that accepted an additional labelFunc parameter, usually in the following form:

```
// ('label -> 'label -> 'label) ->
// Parser<'result,'label,'error> ->
// Parser<'result,'label,'error> ->
// Parser<'result,'label,'error>

orElseL labelFunc a b = (*...*)
```
This was kind of wonky to begin with, but I was not experienced enough to recognize the warning signs. While I could coerce the functions into providing signatures that _seemed_ to do what I intended, I was overconfident in the strength of the type signatures. They help of course, but not all logic is encapsulated in them. For example, in the orElse case, 
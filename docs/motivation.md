# Motivation:
## For the webapp itself:
My past workplace was a fairly large engineering office, with numerous, large projects, that were all administrated through a humungous excel spreadsheet template.  
The template itself was nice and workable, but every project had it's own file, and due to the diverse contractors, and customers, more often than not would end up emailed back and forth, then manually merged and so on.  
Not to mention that for all that excel does to help you, there comes a time when a relational database against which you can execute queries becomes neccessary.

So the basic idea is conceived: Lets move all the project administration data into a database, and create a nice web application around it to extract value from it.

### Main considerations:
- The webapp should keep its spreadsheet like interface, where an engineer can just get a table view of the data they wish to see / manipulate.
- Dynamic views: the user should be able to specify which columns and filter which rows they wish to see and save / reuse these views.
- Sharing: the dynamic views should be shareable with permalinks, for including in correspondence with partners.
- Detailed and granual authorization control: Managing who can read and write what needs to be detailed enough that the user can always share exactly as much as he needs to, and not more, as part of the data is meant to be shared with partnerts, or even filled out by them, but other parts are highly sensitive to the customers.

## For my undertaking of this journey:
First of all, my thesis project has come up, and I wanted to choose a topic, that will be hopefully usefull for someone, and that I have a pretty good idea about what should it entail. (I did a lot of day-dreaming about a better system in place of the spreadsheet of hell while I still worked as a structural engineer)

Secondly when not long ago I came across the idea of functional programming, it really resonated with me, and while I could not make too heavy use of it in my current development job at [Consteel](https://consteelsoftware.com/), it seems to be a really great fit for my ideas around this solution, which will be laid out in the coming posts.
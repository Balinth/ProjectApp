# this is a test, to see how things work with Jekyll on github pages.

here is some code for ya:
{% highlight fsharp %}

let blog markdown =
    jekyll markdown
    |> git push

type RelationOperator =
    | Equals
    | Greater
    | GreaterOrEquals
    | Smaller
    | SmallerOrEquals
    | NotEquals

type BooleanOperator =
    | And
    | Or

type Operator =
    | BooleanOperator of BooleanOperator
    | RelationOperator of RelationOperator

type FieldExpression<'c> =
    | Value of Data
    | Column of Column<'c>

type Expression<'c> =
    | ListExpr of BooleanOperator * Expression<'c> list
    | RelationExpr of RelationOperator * FieldExpression<'c> * FieldExpression<'c>
    | Not of Expression<'c>

{% endhighlight %}

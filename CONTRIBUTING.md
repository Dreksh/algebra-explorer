How to run:
-----------

1. Download elm
1. compile with `elm make src/Main.elm --output=www/js/elm.js --optimize` in the terminal, in the current directory.
...
3. Profit

Load the index file on a web browser, probably by right-clicking the file, and selecting a web browser to
open it

(Or on a Mac: `open -a Safari.app index.html`)

(Or for hot reloading `elm-live src/Main.elm --open -d www -- --output=www/js/elm.js`)

# Jonny's noob notes
## Elm
* every function only takes one parameter, and then is curried into another function
* the parameter can be a list, in which case you can use `[]` with commas between elements
* the dict type is called 'records' and uses `{}`

### Resources
* https://guide.elm-lang.org/
  * the 'official' guide, learn by example top-down
* https://elmprogramming.com/
  * learn bottom-up

### types
* yea! normal types are like enums, and then everything else is just an object
* pattern matching allows us to think of types as a sort of state machine
  * that's why the cardinality of `|` types is addition since it is an 'or'
  * https://guide.elm-lang.org/appendix/types_as_sets
* each type can have a 'type variable' which is like a generic type
  * e.g. `List a` is a list filled with type a
  * this is lower-case and defined BEFORE the = sign, e.g. in `type Foo bar = ...` the `bar` is the generic
* the purpose of type alias when applied to Record type is to constrain what is in there
* `()` is an empty tuple which counts as a type! Crazy

* you can have a type with [only one variant](https://github.com/mdgriffith/elm-animator/blob/master/examples/Mario.elm#L84) which then [doesn't need a `case`](https://github.com/mdgriffith/elm-animator/blob/master/examples/Mario.elm#L376) statement

### statements
* `case` ... `of` handles addition (enum) types
* `let` ... `in` is temporary variables for convenience
* `if` ... `then` ... `else` is what it usually is

### anonymous functions
* a bracket but the first argument has a backslash, e.g. `(\foo -> foo + 1) 3`
* _beware_ because if the types don't match then the only error you'll get is 'not expected'

### Lists
* `foldl` takes a function which has current and agg in it, just like `reduce` in js

### pipes
these two are equivalent:
```
    , Html.div [] (List.foldl (\elem result -> result ++ [treeToDivRecursive elem] ) [] children)
```
and
```
    , children
      |> List.foldl (\elem result -> result ++ [treeToDivRecursive elem] ) []
      |> Html.div []
```

but the second one arranges it into more manageable steps

This piping is usually why functions like [Dict.insert](https://package.elm-lang.org/packages/elm/core/latest/Dict#insert) have Dict as the last parameter as well as the return type, because that lends itself more towards chaining them together
* e.g. `dict |> Dict.insert "a" "b" |> Dict.insert "c" "d"` will insert twice into `dict`

here is some good intuition for the pipe: it is like having objects where you do `foo.bar()`, but instead here we do foo |> bar

### Browser.application
* `subscriptions` is for javascript events such as onTimer and websockets

### JSON
* Json.Decode.* is how to parse json objects (e.g. events from onSubmit)
* here you can use `<|` to make it read nicely

### Commands and Subscriptions

* a command can be returned by an update
  * e.g. a button click returns an event that updates the model, the model then returns a command which makes a HTTP request, this request can then return another event to update the model again
  * it is called a command because you are *commanding* the Html/Javascript runtime to do something
  * it then automatically hooks in the callback to update the model for you
* a subscription *listens* to the runtime
  * a sub needs to return an event to update the model

### Parser
* the `a` in `Parser a` means that the result of the parsing is an `a`
  * but then the `|=` and `|.` operators expect `Parser (a -> b)` **(note that the argument is a function!)**
  * each time you see the `|=` then it gets 'chomped' into the constructor for the `a`
    * OTOH the `|.` just gets ignored
  * `Parser.succeed` does nothing but act as the first link in the chain into these operators
    * this is a bit like `elm-animator` which has `Animator.animator` as the first link of its chain
* the `Step` in `Parser (Step state a)` is just like a `Result`, where if you want to keep looping then you return a `state` and if you're done you return an `a`
* `generateDivisibleNode_` needs to be recursive because a divide node cannot have multiple children

## Maths
* DisplayEvent is passed into the Display.view as an argument
  * that is because it is used as a converter from Display.Event to DisplayEvent
  * this works because the type *is a constructor*
  * the converted type is then pattern matched in the Main.update to update the Display.Model (which is stored in Main.model.display)

* Math.Tree vs Math.Symbol
  * symbolicate adds brackets that allow transformation into linearised form or standard notation
    * standard notation is sort of like the top-down view of the AST
  * the treatment of functions like "+" or "-" is a bit like __syntactic sugar__
    * the sugar is to place them in between arguments while adding precedence rules to their evaluation
    * this is how humans decided to write equations
  * This then makes it easy to convert into the <div>s shown in the equation
    * that is why collapsedView_ is such a short function!

* Parsing
  * the reason "+" is the name of a function instead of a pattern-matched type is that it IS actually just the name of a function. It is nothing special and should not be treated as such. Only when you apply the rules do you actually care about the name, at which point it *should* be a string
  * state is everywhere, and it gets passed in pattern matching bits usually as just `s`
  * in each Function there are `args` and `parameters`, and the reason they exist is so that we can display a semicolon between them. This is useful for things like log where the base is a parameter, NOT an arg
    * the semicolon will also be in the syntax

* the reason expressive type systems are nice are that it is like you are asserting things everywhere
  * cya later Herb Sutter who recommends adding manual asserts everywhere to your C++

* tree operations
  * deleteSubTree also has to be recursive because we have to clear out the helper ParentMap_

* commutative vs associative
  * matrix multiplication is NOT commutative, but is associative
  * rock-paper-scissors is NOT associate, but is commutative (winner-stays-on RPS https://math.stackexchange.com/a/1549811)

### brainstorming
* when you are solving an equation, e.g. x^2 + 2x + 1 = 0, it is only for a specific CONTEXT
* Formal Rules of Algebra are substitutions valid for ALL CONTEXTS
    * https://faculty.ung.edu/mgoodroe/PriorCourses/Math_0999_General/The%20Formal%20Rules%20of%20Algebra.pdf
    * solving an equation is performing an operation on BOTH sides and then one of them cancelling out
      * these could be tabs on the left
    * by default we want to visualise the associative rule because otherwise you end up with tall trees that are unwieldy
* creation is not the same as modification

### animation
we also want to animate the linearised view
look at https://github.com/3b1b/manim for inspiration

* [elm-style-animation](https://package.elm-lang.org/packages/mdgriffith/elm-style-animation/3.5.5) was the precursor to [elm-animator](https://package.elm-lang.org/packages/mdgriffith/elm-animator/latest/) which was made by the same guy
  * here is the [blog post](https://discourse.elm-lang.org/t/announcing-elm-animator/5443) with more useful links


* you start with an `Animator.Animator`
  * using `Animator.watching`
  * it is a bit confusing that this takes the *empty* `Animator.animator` as an argument, but that's so that you can chain them together `|>`
    * this should just be a list of things to watch tbh

* MODEL: `Animator.Timeline a`
  * in the `init` you use `Animator.init a`

* UPDATE:
  * in the normal update we 'add keyframes' to the aforementioned timeline
    * the position of the keyframe is decided by `Animator.go`
  * also need a subscription to advance the timeline
    * but it is `Animator.toSubscription Tick model animator` not `Time.every 1000 Tick`
      * where `animator` your custon `Animator.Animator`
    * this will emit the Tick msg which then gets fed into the update
      * `Tick newTime -> ( Animator.update newTime animator model , Cmd.none )`

* VIEW:
  * here is where you decide the effect of the keyframes you added
  * e.g. to animate opacity, you use `Animator.Inline.opacity` which takes the `Timeline a` and then mapping of that state to an opacity value
    * this means that it needs to be very efficient to extract the value from that state, i.e. we cannot traverse the whole tree for each attribute we'd like to animate, especially because every single node will need many attributes animated
    * that means the state needs to be the layed out coordinates already


* each grid vertical is a variable to be optimised
*   but what happens in this situation?
```
x  | x
xxx|xxxx
```
* e.g. if a parent block wider than both its 2 children, there are infinite solutions for where to place the grid line between the 2 children
  * ideally it would try to distribute the children as evenly as possible
  * I think in general this then becomes like a linear programming problem, but there must be some good heuristic for it
* but maybe this is best solved as part of the recursion because we have a tree structure that makes the problem much simpler
  * when a parent receives the width of its children, if it is wider than all its children then simply scale all the children grid lines up to the width of the parent!

* what is prevID?
  * it is not where the node came from in the previous equation, but it signals which nodes are literally the same thing
  * i.e. a node can have a prevID that doesn't even exist in the previous equation
  * every node gets a new ID on every operation

### query
* Query.elm stores a list of strings as equations, but also sources
  * sources is where topics comes from
  * Main.elm `parseEquations_` parses the `Query.Model` into our AST
  * then it gets turned back into they url via `updateQuery` which is passed into `Display.init`
    * this uses `Query.pushEquations` which captures the current `Query.Model` in a lambda
      * this is then used in `updateQueryCmd` which is called everywhere
      * it also takes `Animation.Tracker` because equations have to fade in at the start
    * this then eventually calls `Rules.process` which converts the `Math.Tree` back into the string format
      * the arguments are `combine convert`, they are used to convert to types other than strings if you so wish
        * the other type used here is in Display.elm when rendering the symbolicated equation above the blocks
        * WAIT this means that the word symbolicate doesn't exist anymore! lol

### Matcher
* selecting a node is from Display.Event.Select (or hold for selecting multiple)
* this updates Display.Model.selected with eqNum and nodeIDs
* this gets fed into ActionView.elm via conversion with `Display.getSelected`
  * uses `Matcher.selectedSubtree` to find the LCA as the root
  * needs to be in Display because it needs to extract the right equation from History
* it eventually makes it into `Matcher.matchRule` to `Matcher.matchSubtree` which does the crazy matching

* the matcher takes many `Rules.Rule`s, each of which contains a list of matches: `Matcher.Matcher` to `Matcher.Replacement`s (plural because find zeros can produce two results)
  * the Rules themselves get downloaded via `ProcessSource` to add them to the menu, and then `Rules.Download`
  * which then issues a command to eventually triggers a `ProcessTopic` which triggers a `Rules.addTopic`
  * there are `Math.FunctionProperty`s (like + or *) stored in a `Math.FunctionProperties` (just a plain Math.Tree)
    * these are used in `expressionDecoder_` (`Parser.parseMatcher`) and `replacementDecoder_` (`Parser.toReplacement`)
    * the former converts to the `Matcher.Matcher` which is also another tree type
      * RealMatcher matches numbers, so 1-to-1 map
      * AnyMatcher is different though, since it matches entire subtrees
      * ExactMatcher matches functions like +-*/, so has to match the exact node
      * DeclarativeMatcher is for statements like =<>
    * the latter converts to `Matcher.Replacement` which is simply a `Math.Tree (Maybe Int)`
      * it passes around a `argDict` which is used to assign the same Ints to the same variables
      * e.g. `m(a+b)` to `ma+mb` will get the same Int for both `m`s in the replacement

* Backtrack.elm is required because we have commutative nodes where any two children can be used
* the final output of matching is `MatchResult state` which retains the `state` of the actual tree
* this is stored in `ActionView.Action` which contains a `Rules.Apply` (type `Rules.Event`) which is assigned to the onClick of the menu bar
  * sometimes you need extra info, so extra events like ApplyParameters or ApplySubstitution are required to feed back from the Modal
  * this event is fed into `Display.transform` -> `Matcher.replaceSubtree`
    * this takes a list of replacements, because e.g. for find zeros it adds both of the results to the history!
  * processSubtree_ takes a List int in order to more quickly hop towards the subtree that needs to be replaced
  * it then applies the second argument which is a function to actually change the subtree
  * constructFromReplacement_ always gives new IDs to all replacement nodes
  * this is where the types `ArgResult_` and `Replaced_` come into play
  * `Value_` contains different types of replacements
    * `ExternalValue_` is a new Math.Tree that doesn't exist in the "from", where the Maybe Int is the id in the "from"
    * `InternalValue_` is a Math.Tree that exists in the "from", which additionally contains the `State` of that
      * Tracker.newState and .copyState are passed in, and are what allows Animation.createState and Animation.updateState to actually be called, this is initialised in Matcher.parseEquation

### Actions
* mouseDown on an svg element triggers an `svgMouseCmd` which issues a javascript command to start subscribing to mouse events
  * these events then feed back via the port `svgMouseEvent` which fires the `Display.svgDragEvent`
* whether a block is draggable is determined in `Bricks.stackRecursive_`
  * `GridItem` now contains a `Maybe (Int, List (Int, Int))` which contains the order of the sibling (e.g. first child, second child) plus sibling column start+ends
* how to handle undo and redo?
  * we need some sort of staging in the history
    * as you hover over the undo you stage some history, then if you click it actually does it
    * just like the toolbar will be
  * which actions are difficult to do because they spawn modals?
    * Expand (Core)
    * Substitution
    * Operate on both sides
    * in this situation we can use the confirm button as the staging button
  * which actions are difficult to do because they split history?
    * Find Zeros
    * in this situation maybe we should just spawn a modal to make them pick one...

* where should Rules.Event actually go?
  * ActionView is actually what calls `Matcher.matchSubtree`
  * but it doesn't actually store the `Display.SelectedNode`, only renders it via a view
  * should SelectedNode be stored somewhere else? Because it is only really


* Any file x higher in the tree than another file y can handle events from y via
* But y cannot ever handle an event from x because it cannot import x to avoid cyclic dependencies

              Main
              /
          ActionView
            /
          Display
        /    \    \        \
Animation  Bricks  History  Draggable
  /
Rules
  /
Matcher
  /
Math
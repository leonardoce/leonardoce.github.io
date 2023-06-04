---
layout: post
title:  "Iliad Framework, reacting to events"
date:   2014-02-17 21:28:24
tags: ["programming", "smalltalk"]
---

In the [previous post]({{< ref "post/2014-02-16-iliad-lesson-three" >}} "previous post") we talked about widgets.

For now we have created a web application with a child widget that
generate a static HTML. Now we need to receive and send data to make
this application interactive.

<!--more-->

I would like to create with you the example that you already find in
the Iliad framework examples, the counter example. You can already see
it in action at this link:
[http://localhost:7070/examples/counters](http://localhost:7070/examples/counters).

A counter is simply an integer that starts with zero and can be
incremented and decremented using two links. We start with creating an
instance variable in the `LcCounterWidget` class that will hold the
current value of the counter:

```smalltalk
ILWidget subclass: #LcCounterWidget
    instanceVariableNames: 'value'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'LeonardoBlog'
```

We create the accessors accordingly with what we have said in the
[previous post]({{< ref "post/" >}} "previous post").

```smalltalk
"LcCounterWidget>>value protocol accessing"
value
    ^ value ifNil: [ value := 0 ].

"LcCounterWidget>>value: protocol accessing"
value:aNumber
    value := aNumber 
```

Now I create the actions corresponding to the links under the counter:

```smalltalk
"LcCounterWidget>>increase protocol accessing"
increase
    self value: (self value + 1).
    self markDirty.

"LcCounterWidget>>decrease protocol accessing"
decrease
    self value: (self value - 1).
    self markDirty.
```

As you may see these actions barely increment and decrement the value
using the accessor method and then call the `markDirty` method, who
means that the component has to be redrawn on the client side.

Now we need to modify the `contents` method to display the value of
the counter on the page:

```smalltalk
"LcCounterWidget>>firstWidget protocol building"
contents
	^ [ :e |
		e p text: self value asString.
		e a
		text: '++';
		action: [ self increase ].
		e a
		text: '--';
		action: [ self decrease ] ]
```

Now you can try the URL:
[http://localhost:7070/leonardoBlog/widgetExample](http://localhost:7070/leonardoBlog/widgetExample)
to see that our implementation is working.

The next post is [here]({{< ref "post/2014-02-18-iliad-framework-lesson-five" >}} "next post").

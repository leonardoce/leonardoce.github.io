---
layout: post
title:  "Iliad Framework, the first widget"
date:   2014-02-16 21:28:24
tags: ["programming", "smalltalk"]
---

In the [previous post]({{< ref "post/2014-02-14-iliad-lesson-two" >}} "previous post") we talked about controllers.

In this lesson we will create the first widget using the Iliad
framework.

<!--more-->

We have studied applications and controllers but the core of the Iliad
framework is the concept of **widget**. Every page served by the Iliad
framework is composed by widgets, as widget are written on the page as
HTML tags. A widget is:

- *stateful* and this means that the instace variables of the widget
  class are conserved between HTTP requests in the session state;

- *reusable* every widget class can (and will) be used many times even
  in the same session using multiple widgets;

- *a container* of child widgets.

Every widget is a subclass of the `ILWidget` class. Let's create out
first widget:

```smalltalk
ILWidget subclass: #LcCounterWidget
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'LeonardoBlog'
```

The contents of a widget must be defined overriding the `contents`
method of the widget class:

```smalltalk
"LcCounterWidget>>contents protocol building"
contents
    ^ [ :e | e p text:'I''m a widget!' ]
```

When I said that **every** page server by the Iliad framework is
composed by widgets I was actually right because also the
`ILApplication` class extends `ILWidget`!

To attach out widget to the application we will add an instance
variable to the application class to store the widget instance.

Create a new accessor for the `firstWidget` component that create the
instance if needed:

```smalltalk
"LcBlogHelloIliad>>firstWidget protocol accessing"
firstWidget
    ^ firstWidget ifNil: [ firstWidget := LcCounterWidget new ]
```

Now I will create a new controller just to show how the widget can be
rendered:

```smalltalk
"LcBlogHelloIliad>>widgetExample protocol contollers"
widgetExample
    ^ [ :e | e p text:'Hi! Widget example'.
    e build:self firstWidget ]
```

As always *pay attention* on putting the `widgetExample` in the right
protocol!

Now you can go here:
[http://localhost:7070/leonardoBlog/widgetExample](http://localhost:7070/leonardoBlog/widgetExample)
to see the example working.

This article will be long but I should make a remark on the
`firstWidget` accessor method. We could also use the `initialize`
method to create a new instance of the widget when the
`LcBlogHelloIliad` class is created and this will actually work but,
if you choose my implementation (which is copyied from the Iliad
examples) you can reuse your old browser session to try new code and
this is really good, believe me.

The next post is [here]({{< ref "post/2014-02-17-iliad-framework-lesson-four" >}} "next post").

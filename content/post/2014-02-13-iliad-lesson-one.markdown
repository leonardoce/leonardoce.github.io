---
layout: post
title:  "Iliad Framework, how to create your first application"
date:   2014-02-13 21:28:24
tags: ["programming", "smalltalk"]
---

Hi. In this post we will create a basic web application using the
Iliad Framework. 

<!--more-->

Please consult the [previous post][previous-post] if you need to
install the web framework in your image.

An Iliad web application consists in a class that extends from
`ILApplication` so lets create one:

```smalltalk
ILApplication subclass: #LcBlogHelloIliad
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'LeonardoBlog'
```

Now we need to declare where our application will be published. To
configure the starting point for our web app we need to override the
`path` class method:

```smalltalk
"LcBlogHelloIliad class>>path"
path
    ^ 'leonardoBlog'
```

Every Iliad Application implements the Front Controller partner. This
means that it will receive every request addressed to the application
from the http server.
 
The `index` controller will handle the request to the `/leonardoBlog`
address, just like your `index.html` file. Let's implement an `index`
controller on the instance side of our `LcBlogHelloIliad` class,
remember to put this method on the `controllers` protocol:

```smalltalk
"LcBlogHelloIliad>>index protocol controllers"
index
    ^ [ :e | e h1: 'Hi world!' ]
```

Now, to check that everything is correct, you can go to
[http://localhost:7070/leonardoBlog](http://localhost:7070/leonardoBlog).
The controller method must return a "buildable object". What's a
buildable object?

A buildable object can be many things... widgets, applications, and
blocks of code. In the example I returned a block that, from an
`ILHTMLBuilderElement` create an `h1` HTML element.

Please browse the code from the `ILHTMLBuilderElement` class. You will
find many useful methods to create basically every HTML element you
want.

The next article is [here][next-post]

[previous-post]: {% post_url 2014-02-12-iliad-introduction %}
[next-post]: {% post_url 2014-02-14-iliad-lesson-two %}

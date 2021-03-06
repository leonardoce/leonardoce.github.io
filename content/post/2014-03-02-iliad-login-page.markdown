---
layout: post
title:  "Iliad Framework, the login form"
date:   2014-03-02 21:28:24
tags: ["programming", "smalltalk"]
---

In the [previous post]({{< ref "post/2014-02-27-iliad-framework-customizing-page " >}} "previous post") we talked about customizing the
generated page to include references to the [Bootstrap](http://getbootstrap.com)
project.

This is the first lession where we start working on a complete
example, a projecs-notes app usable from desktop and mobile web
browsers using the twitter [bootstrap framework](http://getbootstrap.com).

<!--more-->

Now we will start working on a login page.

To build a new page with a form I will start making a component that
will be built from the application controller named `login`. The
controller will be named `PnProjectLogin`.

Let's start building this new component creating a class whose
superclass is `ILWidget`:

```smalltalk
ILWidget subclass: #PnProjectLogin
	instanceVariableNames: 'username password loginError'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'
```

remember that widgets are stateful: the instance variables will be
populated by the application and by the user.

* `username` will be the text entered by the user in the login form;
* `password` as previous;
* `loginError` will be the string of the error message of the login or
  `nil` if there is no error message.

I won't show the accessor methods because they can be automatically
generated by the Pharo browser but I will show the initializer method:

```smalltalk
!PnProjectLogin methodsFor: 'initialization'!
initialize
	super initialize.
	loginError := nil.
	username := ''.
	password := ''.! !
```

Let's start by creating a method to build the username field and the
relative label:

```smalltalk
!PnProjectLogin methodsFor: 'building'!
buildLoginRow: aForm
	aForm div class: (loginError ifNotNil: [ 'form-group has-error' ] ifNil:  [ 'form-group' ]);
			build: [ : row | 
				row label text:'Login:'.
				row input class:'form-control'; action: [ :text | username := text ]; value:username ]! !
```

See how interesting it the `build:` method: it takes a block and
evaluate it passing, as argument, the receiver. Before executing the
passed block the method opens the element and closes it when the block
finished. In the previous example the `aForm div` execution results in
a `DIV` element that get automatically closed when the block has
terminated its execution.

Another interesting thing is the `class:` method invocation on the
`aForm div` element: to comply with the bootstrap rules we need to
insert the `has-error` class when this element of the form is
wrong. To do this we check the `loginError` contents.

In the `input` element, the action block takes the element text
content.

This method is somewhat similiar:

```smalltalk
!PnProjectLogin methodsFor: 'building'!
buildPasswordRow: aForm
	aForm div class:(loginError ifNotNil: [ 'form-group has-error' ] ifNil:  [ 'form-group' ]);
			build: [ : row |
				row label text:'Password'.
				row input class:'form-control'; type:'password'; action: [ :text | password := text ] ]! !
```

As you see, while the login field get constructed with a value (or the
previous one), the password is always empty.

Ok. Now we will construct the error message row of the form:

```smalltalk
!PnProjectLogin methodsFor: 'building'!
buildErrorMessageRow: aForm
	loginError ifNotNil: [
	(aForm div class: 'alert alert-warning') text: loginError ]! !
```

Obviously we have to build the errors row only if there are errors!
Well. Now we are missing the "register" link.

```smalltalk
!PnProjectLogin methodsFor: 'building'!
buildRegisterRow: aForm
	(aForm div class: 'form-group') a
		href: 'register';
		text: 'Want to register?'! !
```

Let's bind all together in the `contents` method:

```smalltalk
!PnProjectLogin methodsFor: 'building' stamp: 'LeonardoCecchi 2/28/2014 23:03'!
contents
	^ [ :e | 
	e h1 text: 'Project Notes - Login'.
	e p
		text:
			'Project Notes is a note taking app that you
			can use for your project. It will store memos for
			you and for your team.'.
	e p text: 'Believe me, you really need this app!!'.
	e form
		build: [ :form | 
			self buildLoginRow: form.
			self buildPasswordRow: form.
			self buildErrorMessageRow: form.
			self buildRegisterRow: form.
			form button
				class: 'btn btn-default';
				text: 'Login!!';
				action: [ self loginAction ] ] ]! !
```

In the `loginAction` method, which is called when the user press on
the "Login" button we execute the `loginAction` method of this class,
which is defined like this:

```smalltalk
!PnProjectLogin methodsFor: 'actions'!
loginAction
	self markDirty .
	username ifEmpty: [ ^ loginError := 'Please enter the user name' ].
	password ifEmpty: [ ^ loginError := 'Please enter the password' ].
	loginError := nil.
	"self redirectToLocal: 'browseProject'."
```

In this method, which for now is only a stub, we check for empty data
and then we will call the main page.

Now we need to integrate this widget in our application,
`LcBlogProjectNotes`. We must add an instance variable named `loginWidget`.

```smalltalk
ILApplication subclass: #LcBlogProjectNotes
	instanceVariableNames: 'loginWidget
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'
```

As we discussed in the [relative article]({{< ref "post/2014-02-16-iliad-lesson-three" >}} "lesson three") we need an
accessor method that lazily creates the widget:

```smalltalk
!LcBlogProjectNotes methodsFor: 'accessing'!
loginWidget
	^ loginWidget ifNil: [ loginWidget := PnProjectLogin new ]! !
```

Now we can create the controller method:

```smalltalk
!LcBlogProjectNotes methodsFor: 'controllers'!
login
	^ [ :e | e div class:'container'; build: self loginWidget ]! !
```

If you go to
[http://localhost:7070/ProjectNotes/login](http://localhost:7070/ProjectNotes/login)
you should see the following result:

![Project notes login form](/images/pnotes-login.png)

Yeah! We succesfuly build our first form, even with error checking.
When the `loginError` is not empty the form looks like this:

![Project notes login form error](/images/pnotes-login-error.png)

The next post is [here]({{< ref "post/2014-03-03-users-dao-intermezzo" >}} "next post").

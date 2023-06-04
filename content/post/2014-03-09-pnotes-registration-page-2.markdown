---
layout: post
title:  "Iliad Framework, the registration form (part 2)"
date:   2014-03-09 21:28:24
tags: ["programming", "smalltalk"]
---

In the [previous post]({{< ref "post/2014-03-07-pnotes-registration-page" >}} "previous post") we build a registration form and
in this one we will attach it to our application.

<!--more-->

We start adding an instance variable to our application to contain the
registration widget and another instance variable to contain the
current user:

```smalltalk
ILApplication subclass: #LcBlogProjectNotes
	instanceVariableNames: 'loginWidget registrationWidget currentuser' 
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'
```

As we have done for the `loginWidget`, we create an accessor method
that will construct the widget if it hasn't been instanciated:

```smalltalk
!LcBlogProjectNotes methodsFor: 'accessing'!
registrationWidget
	^ registrationWidget ifNil: [ registrationWidget := PnCreateUser new ]
```

We also create accessors for the current user instance variable:

```smalltalk
!LcBlogProjectNotes methodsFor: 'accessing'!
currentuser: anObject
	currentuser := anObject

!LcBlogProjectNotes methodsFor: 'accessing'!
currentuser
	^ currentuser
```

Now we create a controller for the registration page:

```smalltalk
!LcBlogProjectNotes methodsFor: 'controllers'!
register
	^ [ :e | e div class:'container'; build: self registrationWidget ]
```

Now we have our new controller and we can test it from the login
page. The controller name, `register`, match with the `href` in the
login page.

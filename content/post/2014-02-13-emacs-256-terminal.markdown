---
layout: post
title:  "Emacs in the terminal"
tags: ["emacs"]
date:   2014-02-13
---

Despite being an avid Emacs user, I usually start Emacs with a GUI
interface. I used to start Emacs inside a terminal only when working
on another computer using ssh.

<!--more-->

Today I had 10 minutes of free time and I wondered if I could get
Emacs themes working on my terminal.

A few Google searches and Stackoverflow helped me to find the
solution:

* use a 256-colors supporting terminal;
* set the `TERM` environment variable with the right value.

```sh
export TERM=xterm-256color
```

With this settings you can use your emacs theme (if it supports
256-color mode) in the console.

---
layout: post
title:  "ZShell, how to enable menu completion"
date:   2014-02-22 21:28:24
tags: ["shell"]
---

If you are using ZShell you must try to use the `autocd` option and
the `menu` completion.

<!--more-->

To enable the autocd option you insert in your `.zshrc` this line:

```console console %}
setopt autocd
```

Now you can change directory simply saying the directory name:

```console console %}
[leonardo@fabula ~]$ pwd
/home/leonardo
[leonardo@fabula ~]$ leonardoce.github.io
[leonardo@fabula leonardoce.github.io]$ pwd
/home/leonardo/leonardoce.github.io
[leonardo@fabula leonardoce.github.io]$
```

With the `menu` completion you can choose interactively an item
between many possible answers. I think that the better description is
yours so try it. Append to your `.zshrc`:

```console console %}
zstyle ':completion:*' menu select
```


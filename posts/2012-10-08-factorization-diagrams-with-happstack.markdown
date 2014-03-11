---
title: Factorization diagrams with Happstack
author: Alp Mestanogullari
date: 2012-10-08
description: Using Brent's recent factorization diagram code, I wrote a small web application that lets you enter numbers and have their factorization diagrams.
tags: haskell, diagrams, happstack, math
---

After having seen [Brent Yorgey's blog post](http://mathlesstraveled.com/2012/10/05/factorization-diagrams/), I started hacking-up a simple Happstack webapp to display factorization diagrams. This has been pretty straight forward, although [the code](http://hub.darcs.net/alp/factorization-diagrams-happstack) isn't really clean nor idiomatic. I just felt like quickly putting something up and running during the little free time I have had this weekend. You can play with the app at [diagrams.alpmestan.com](http://diagrams.alpmestan.com/). Here is an example of generated SVG.

![1235's factorization](http://diagrams.alpmestan.com/diagrams/1235.svg)

---
title: Scoutess, continuous integration, cabal and the Google Summer of Code
author: Alp Mestanogullari
date: 2012-03-21 0:00
tags: haskell, scoutess, cabal, gsoc
description: Let me tell you about scoutess and send a message to prospective GSOC students
---

Recently, many people have been publishing blog posts or Google+ threads about cabal(-install), dependency hells, continuous integration, and some other issues ([here](http://blog.johantibell.com/2012/03/cabal-of-my-dreams.html) and [there](http://www.yesodweb.com/blog/2012/03/cabal-nirvana) for example). You may also have noticed some attempts to work around some of these issues, like [cabal-nirvana](http://hackage.haskell.org/package/cabal-nirvana). Some of you may have also read about or even used [Travis CI](http://travis-ci.org/) ([they recently announced](http://about.travis-ci.org/blog/announcing_support_for_haskell_on_travis_ci/) Haskell project support).

<!-- more -->

# Scoutess

Jeremy Shaw and I have been discussing a lot about many of the issues that make a Haskell project hard to maintain, in particular how hard it is to know when/how someone will enter the dependency hell when installing your project, and how/why that person got there. Then we talked about how nice it would be to actually have a flexible continuous integration platform for (cabal-powered) Haskell projects. (This was before cabal-nirvana has been written and also before Travis CI's announcement.)

So we decided to start such a project. Scoutess aims at providing the Haskell community a powerful and viable build bot with a few key services like dependency tracking, on-commit build testing, nightly builds with handling of multiple GHC versions (and dependency versions), report generation, etc. Of course, all the builds will be sandboxed and many of the services will be configurable (and in particular, you will be able to turn them on/off). You can find a list of features/ideas we plan to implement (or already have implemented) on [this page](http://patch-tag.com/r/alpmestan/scoutess/wiki/).

In particular, you may notice that scoutess. scope is deeper than Travis CI's. For example, if you want to do dependency tracking, or handle multiple package and GHC versions, or handle automatic posting of reports somewhere, you have to handle it all by yourself with Travis CI, whereas scoutess will provide this out-of-the-box. Also, as far as I know, Travis CI is currently restricted to github (although this may not be the biggest drawback since it represents a fair amount of the most commonly used Haskell packages). And we plan to benefit from focusing only on cabal-powered Haskell projects by using all the information we can gather from cabal files to offer much better functionality.

We already have implemented some core features and designed a part of the project, but it is still far from being usable. If you want to talk about the project with us, give your opinion or some ideas, do not hesitate to drop by the [#scoutess](irc://irc.freenode.net/happs) IRC channel on freenode and ping stepcut and/or alpounet.

# Want to hack on scoutess for the Google Summer of Code?

If you are a prospective student for the GSoC and if you are interested in working on scoutess, we would gladly accept mentoring you during the GSoC. Possible project ideas include (but are not restricted to):

* writing the sandboxed building service around cabal-dev, virtualhenv or hs-env, and integrating it with the current code-base (for example, making it use the `LocalHackage` service instead of just fetching packages over and over again)
* writing the dependency-tracking service (which would eventually also include tracking the development repositories of the dependencies)
* working on the report generation and posting system (but that would most likely require the build service to be implemented)

These are the 3 ideas I have in mind right now, but we could find something more "restricted", or just tailored for an interested student after some discussion.

So feel free to ask any question about the project and/or the ideas for the GSoC in the comments, on IRC or by email at alpmestan@gmail.com.

Note: I just created a ticket on the Haskell GSoC trac [here](http://hackage.haskell.org/trac/summer-of-code/ticket/1612).


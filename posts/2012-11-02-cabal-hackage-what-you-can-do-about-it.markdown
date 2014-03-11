---
title: The cabal/hackage situation, and what you can do about it
author: Alp Mestanogullari
date: 2012-11-02 0:00
tags: haskell, cabal, hackage, scoutess
description: Cabal has some problems, hackage too, so I just take a look at some of those and make a case for having a decent community effort for helping hackage development, toward a Hackage 2 release.
---

It's that time of the year, again, when the community talks a lot about what is wrong with cabal and hackage. A few discussions happened on reddit (see [1](http://www.reddit.com/r/haskell/comments/12hbzb/why_cabal_has_problems/), [2](http://www.reddit.com/r/haskell/comments/12e3a0/the_good_the_bad_and_the_ugly_haskell_in/) and [3](http://www.reddit.com/r/haskell/comments/12alw1/why_inbreeding_is_bad_for_your_community_cabal/) for example). So, we all know what the issues are (if you don't, reading the comments on these links will certainly help you realize what the issues are). This post is about what can be done from different point of views.

<!-- more -->

Using other people's libraries or programs in your projects
===========================================================

So, you have a project, and that project has a .cabal file, listing the dependencies. But when you want to build your project, you install the dependencies in your user package database. Just having a couple of projects may be enough to introduce conflicts and get you a one-way ticket to the dependency hell. For that kind of issues, there is a solution. There are several of them, actually, but the most popular one probably is [cabal-dev](http://hackage.haskell.org/package/cabal-dev). You can learn how to use it on [its github page](https://github.com/creswick/cabal-dev/blob/master/README.md). 

Basically, you will have per-project package databases. The big disadvantage in that approach is that your projects tend to always use libraries like bytestring, text, etc and that you will have to build them once per project, but I do not find this to be that big a problem with today's hardware. So, with that solution, unless different dependencies *of the same project* use conflicting versions of some package, you should be fine. 

An important note is that the next release of cabal-install will have sandboxes (just like cabal-dev) built-in. This is the result of a hard work during the GSoC this year, and even after the end of it. So what you'll be used to do with cabal-dev will be possible with the usual 'cabal' program. However, this isn't about importing some code from cabal-dev. It provides better sandboxes than cabal-dev. For instance, cabal-dev only take snapshots of dependent packages whereas cabal-install will support live source trees.

The Cabal/cabal-install developers have a lot of ideas to improve the situation. Also, [the code is on github](https://github.com/haskell/cabal). They even provide a [HACKING](https://github.com/haskell/cabal/blob/master/HACKING) file and their [issue tracker](https://github.com/haskell/cabal/issues?labels=&page=1&state=open) has an awful lot of open issues (483 right now). This provides pretty much all you need if you want to help on the Cabal front.

The Hackage mess
================

So, you are looking on Hackage for a library that provides some specific functionality. You find 5 of them that seem to offer it, but don't know which one to choose? Do you really have to test all of them to figure out their ease of use, their performance characteristics and whatnot? Well, except when the package is one that is known to be a reference, that's pretty much your only solution, yes. However, many of you may be aware that a new version of Hackage is being worked on, but probably don't know where that work is happening, who is working on it, what needs to be done, etc.

These last few months, Duncan Coutts and Ian Lynagh have been working hard on improving the performances of the current Hackage2 code, on some refactorings and on getting it closer and closer to being deployed to replace the current hackage. 

This new version of Hackage will have a *huge* amount of new features, eventually, that will help sorting out "the mess" that we see when looking at [this page](http://hackage.haskell.org/packages/archive/pkg-list.html). And it will bring a lot more than just this. However, obviously that requires a lot of hacking, so how about joining the effort? There are many simple tasks that one can do to get Hackage2 closer to a deployment. For instance, I am working on adding some code to back-up some data the Hackage2 server stores in memory while running, and on the corresponding import code. Really, right now, most of the tasks are about filling in some gaps: making sure we can import the old data, making sure backups will work, etc. It's about getting it ready for the switchover. The initial version of Hackage2 will be similar to the current one, but will be a *much better basis* for adding all the features needed to improve our experience as a community (number of downloads and reverse dependencies as empirical "quality" metrics? attaching "getting started" code snippets to your package's page? etc).

If you want to help with this, you can find a lot of information about the Hackage2 project [here](http://hackage.haskell.org/trac/hackage/wiki/HackageDB/2.0). That includes where you can find the code, the tasks that need to be done, the people you can contact to talk about what you should do, etc. 

Maintaining a project is hard
=============================

A lot of package maintainers know this situation: a dependency of your project introduces some breaking change, and someone sends you a message on IRC or by email to tell you they can't install your package. You thus either have to adjust the version bounds for that dependency, or make your code compatible with the changes (or you write different code for the two different version ranges, but that of course scales badly). The former keeps you from benefiting from all future improvements to this dependency, the latter keeps your code from working with all the previous versions. And you can't do anything about it anymore, because the new version of your dependency is released on Hackage.

Another scenario: you introduce some breaking change in your package. All your reverse dependencies (packages using yours) may now be broken. What can you do about that? Maybe luckily some of them had the right version bounds for you in their .cabal file (this isn't that hard to get right, if you follow the [PVP](http://www.haskell.org/haskellwiki/Package_versioning_policy)). But then again, every now and then, some package doesn't follow it. Or the version bounds for some dependency of a package are just wrong and that pretty much keeps you from being able to install that package, unless you figure out the exact problem, which... let's be honest, is tiresome and not always reasonable to do.

These scenarios (among many others) are something Jeremy Shaw ([Happstack](http://www.happstack.com/)'s lead dev) and I were so tired of that we decided to start the *scoutess* project. For more information about it, please visit [this post](http://alpmestan.com/posts/2012-03-21-scoutess-continuous-integration-cabal-gsoc.html) and visit us on #scoutess on 'irc.freenode.net'. This aims at being a build-bot-on-steroids for cabal-powered projects. There's still plenty to be done before we can get a first release out, and we would benefit a lot from having a few more hands to help us write down some more code to get us closer to a release. So if you're interested, contact us, by email, IRC, or [the fresh mailing-list](http://groups.google.com/group/scoutess), take a look at the code [on darcshub](http://hub.darcs.net/alp/scoutess) (although it may change drastically soon).

A side note about this: there will be a feature in the new Hackage that will let package maintainers or Hackage trustees adjust dependencies *after* a release without having to update the tarball. cabal-install's latest release has the client-side code for this, and the server-side code is partly written too.

Conclusion
==========

There is yet another interesting and related project, about a nix-style persistent package store. You can read about it [here](http://www.haskell.org/haskellwiki/HaskellImplementorsWorkshop/2012/Schuster) and watch the Haskell Implementors Workshop talk about it [here](http://www.youtube.com/watch?v=h4QmkyN28Qs). This may be a key for opening the door of the dependency paradise. So do not hesitate to take a look at what Philip did and whether you can help.

You can do a lot to improve the situation. Just contributing a tiny feature to one of the projects I talked about is already a huge win for that project's authors, and almost-directly for the *whole community*. We know the problems, so it's not that helpful to keep talking about it on Google+, reddit, twitter and whatnot without doing anything to solve them. The people working on all of these projects know the technical details to improve the situation, but do not have an awful lot of time to spend working on it. Anyone in the community can make a difference, can bring us closer to a state where we do not hear/read complaints every single day about cabal, hackage, broken packages, dependency hells, etc. Pretty much all the Hackage/Cabal/Scoutess devs would be delighted to welcome new contributors, whatever the size of the contribution is, may it be good bug reports, some documentation improvements, bug fixes and of course new features. And contributing to these projects may seem scary, but you will figure out that there are an awful lot of simple tasks that need to be done. For instance, I have something like 10 of them in mind for Scoutess. So if you're interested, please come talk to us, let us know what you want to work on, we'll most likely give you a few advices/directions, and you'll be good to go!

_Many thanks to Duncan Coutts for the many helpful comments on the drafts of this post. I definitely owe you a beer or two!_

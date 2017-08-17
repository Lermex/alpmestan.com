---
title: "Write web services around databases with 0 boilerplate: announcing servant 0.1"
author: Alp Mestanogullari
date: 2014-07-26 17:00
description: If you need to write web services that basically wrap database operations, check out servant whose goal is to let you do that in just a couple of lines. No boilerplate anymore.
comments: true
tags: haskell, web, rest, webservices
---

At [Zalora](http://github.com/zalora), we write a lot of web services and web applications in general. We use [scotty](http://hackage.haskell.org/package/scotty) a lot. And after having written a couple of web-services, despite some small handy abstractions we came up with, it really felt like we could achieve the same thing in a very concise and minimalist manner, by letting the compiler do more work for us so that we would just have to write wrapper for our SQL queries in haskell. All we had to do was to take advantage of a couple of extensions that landed in GHC in the past few years and propagate the right bits of information at the type-level. And this is what we've done.

The result is servant ([github](http://github.com/zalora/servant), [hackage](http://hackage.haskell.org/package/servant)), which lets you declare _resources_, which just represent a bunch of operations (think endpoints) that operate on some type. So you could for example declare a **users** resource that supports adding, deleting and listing users in the following way:

``` haskell
  mkResource "users" ctx exceptions
    & addWith     addUser
    & deleteWith  deleteUser
    & listAllWith listUsers
```

where:

- `ctx` is just there to specify how to get our hand on our database connection for example, think of it as a `withConnection` function
- `exceptions` is a bunch of functions that catch exceptions of various types and turns them into an error type of yours
- `addUser`, `deleteUser` and `listUsers` are functions that run the corresponding SQL queries using the connection provided by `ctx`

And now you can turn this into a JSON-based webservice by simply applying `Servant.Scotty.runResource` to this simple definition. Then, provided you have written a handful of instances as required by each operation, you'll have a small REST-y webservice with 3 endpoints that do what you expect.

The more interesting aspect of servant however is that the add, delete and listall operations just happen to be some *prelude* operations provided by the servant packages. You can define your own in just the same way the standard ones are defined. The same applies to the automatic JSON-based request body/response body handling or to the web-framework backend used (we only have a *scotty* one for now but you could write your own for any other framework by drawing some inspiration from the scotty one). You can extend servant in basically every possible direction.

If you want to learn more about **servant**, how it can be used and how it works, you may be interested to check out [the README from github](https://github.com/zalora/servant#servant) which contains some documentation links, that I'll reproduce here:

- [Getting started with `servant`](https://github.com/zalora/servant/blob/master/getting-started.md), which guides you through building the simple webservice we've seen above. There's an example in the repository with the code covered in this *getting started* guide, with a cabal file and everything.
- [Tutorial](https://github.com/zalora/servant/blob/master/tutorial.md), which dives much more into servant's packages and modules and its inner workings, with some illustrations of the extensibility of servant.
- [Haddocks for all servant packages](http://alpmestan.com/servant/)

We would of course be glad to hear any kind of feedback, so please do not hesitate to shoot us an email with comments, and report any issue you may encounter on our github.

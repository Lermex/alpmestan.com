---
title: Testing attoparsec parsers with hspec
author: Alp Mestanogullari
date: 2014-06-18 14:00
description: If you have been writing haskell code for long enough, you probably have written a couple of parsers. I have written a package that makes testing attoparsec parsers a piece of cake.
comments: true
toc: true
tags: haskell, testing, attoparsec, hspec
---

Almost all haskellers end up, some day, having to write a parser. But then, that's not really a problem because writing parsers in Haskell isn't really annoying, like it tends to be elsewhere. Of special interest to us is [attoparsec](http://hackage.haskell.org/package/attoparsec), a **very fast** parser generator library. It lets you combine small, simple parsers to express how data should be extracted from that specific format you're working with.

# Getting our feet wet with attoparsec

For example, suppose you want to parse something of the form `|<any char>|` where `<any char>` can be... well, any character. We obviously only care about that precise character sitting there -- once the input is processed, we don't really care about these `|` anymore. This is a no-brainer with attoparsec.

``` haskell
module Parser

import Data.Attoparsec.Text

weirdParser :: Parser Char
weirdParser = do -- attoparsec's Parser type has a useful monad instance
  char '|'       -- matches just '|', fails on any other char
  c <- anyChar   -- matches any character and returns it
  char '|'       -- matches just '|', like on the first line
  return c       -- return the inner character we parsed
```

Here we go, we have our parser. If you're a bit lost with these combinators, feel free to switch back and forth between this article and the documentation of [Data.Attoparsec.Text](http://hackage.haskell.org/package/attoparsec-0.12.1.0/docs/Data-Attoparsec-Text.html).

This parser will fail if any of the 3 smaller parsers I'm using fail. If there's more input than just what we're interested in, the additional content will be left unconsumed.

Let's now see our parser in action, by loading it in ghci and trying to feed it various inputs.

First, we want to be able to type in [`Text`](http://hackage.haskell.org/package/text) values directly without using conversions functions from/to `String`s. For that reason, we enable the `OverloadedStrings` extension. We also import `Data.Attoparsec.Text` because in addition to containing `char` and `anyChar` it also contains the functions that let us run a parser on some input (_make sure attoparsec is installed_).

``` haskell
λ> :set -XOverloadedStrings
λ> import Data.Attoparsec.Text
```

`Data.Attoparsec.Text` contains a `parse` function, which takes a parser and some input, and yields a `Result`. A `Result` will just let us know whether the parser failed, with some diagnostic information, or if it was on its way to successfully parsing a value but didn't get enough input (imagine we just feed `"|x"` to our parser: it won't fail, because it looks almost exactly like what we want to parse, except that it doesn't have that terminal `'|'`, so attoparsec will just tell us it needs more input to complete -- or fail), or, finally, if everything went smoothly and it actually hands back to us a successfully parser `Char` in our case, along with some possibly unconsumed input.

Why do we care about this? Because when we'll test our parsers with `hspec-attoparsec`, we'll be able to test the kind of `Result` our parsers leaves us with, among other things.

Back to concrete things, let's run our parser on a valid input.

``` haskell
λ> parse weirdParser "|x|"
Done "" 'x'
```

That means it successfully parsed our inner `'x'` between two `'|'`s. What if we have more input than necessary for the parser?

``` haskell
λ> parse weirdParser "|x|hello world"
Done "hello world" 'x'
```

Interesting! It successfully parsed our `'x'` and also tells us `"hello world"` was left unconsumed, because the parser didn't need to go that far in the input string to extract the information we want.

But, if the input looks right but lets the parser halfway through completing, what happens?

``` haskell
λ> parse weirdParser "|x"
Partial _ 
```

Here, the input is missing the final `|` that would make the parser succeed. So we're told that the parser has _partially_ succeeded, meaning that with that input, it's been running successfully but hasn't yet parsed everything it's supposed to. What that `Partial` holds isn't an just underscore but a function to resume the parsing with some more input (a continuation). The `Show` instance for parsers just writes a `_` in place of functions.

Ok, and now, how about we feed some "wrong data" to our parser?

``` haskell
λ> parse weirdParser "bbq"
Fail "bbq" ["'|'"] "Failed reading: satisfy"
```

Alright! Equipped with this minimal knowledge of attoparsec, we'll now see how we can test our parser.

# Introducing `hspec-attoparsec`

Well, I happen to be working on an HTML parsing library based on *attoparsec*, and I've been using [hspec](http://hspec.github.io/) for all my testing needs these past few months -- working with the author surely helped, hello [Simon](http://github.com/sol)! -- so I wanted to check whether I could come up with a minimalist API for testing attoparsec parsers.

If you don't know how to use hspec, I warmly recommend visititing [hspec.github.io](http://hspec.github.io/), it is well documented.

So let's first get the boilerplate out of our way.

``` haskell
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

-- we import Text, this will be our input type
import Data.Text (Text)
-- we import hspec, to run the test suite
import Test.Hspec
-- we import 'hspec-attoparsec'
import Test.Hspec.Attoparsec
-- we import the module where our parser is defined
import Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec = return () -- this is temporary, we'll write our tests here
```

And sure enough, we can already get this running in ghci (ignore the warnings, they are just saying that we're not yet using our parser or `hspec-attoparsec`), although it's quite useless:

``` haskell
λ> :l example/Parser.hs example/ParserSpec.hs
[1 of 2] Compiling Parser           ( example/Parser.hs, interpreted )
[2 of 2] Compiling ParserSpec       ( example/ParserSpec.hs, interpreted )

example/ParserSpec.hs:8:1: Warning:
    The import of ‘Test.Hspec.Attoparsec’ is redundant
      except perhaps to import instances from ‘Test.Hspec.Attoparsec’
    To import instances alone, use: import Test.Hspec.Attoparsec()

example/ParserSpec.hs:10:1: Warning:
    The import of ‘Parser’ is redundant
      except perhaps to import instances from ‘Parser’
    To import instances alone, use: import Parser()
Ok, modules loaded: Parser, ParserSpec.
λ> ParserSpec.main

Finished in 0.0001 seconds
0 examples, 0 failures
```

Alright, let's first introduce a couple of tests where or parser should succeed.

``` haskell
spec :: Spec
spec = do
  describe "weird parser - success cases" $ do

    it "successfully parses |a| into 'a'" $
      ("|a|" :: Text) ~> weirdParser
        `shouldParse` 'a'

    it "successfully parses |3| into '3'" $
      ("|3|" :: Text) ~> weirdParser
        `shouldParse` '3'

    it "successfully parses ||| into '|'" $
      ("|||" :: Text) ~> weirdParser
        `shouldParse` '|'
```

We're using two things from `hspec-attoparsec`:

- `(~>)`, which connects some input to a parser and extracts either an error string or an actual value, depending on how the parsing went.
- `shouldParse`, which takes the result of `(~>)` and compares it to what you expect the value to be. If the parsing fails, the test won't pass, obviously, and `hspec-attoparsec` will report that the parsing failed. If the parsing succeeds, the parsed value is compared to the expected one and a proper error message is reported with both values printed out. 

``` haskell
(~>) :: Source parser string string' result 
     => string           -- ^ the input
     -> parser string' a -- ^ the parser to run
     -> Either String a  -- ^ either an error or a parsed value

shouldParse :: (Eq a, Show a) 
            => Either String a -- ^ result of a call to ~>
            -> a               -- ^ expected value
            -> Expectation     -- ^ resulting hspec "expectation"
```

Running them gives:

``` haskell
λ> ParserSpec.main

weird parser - success cases
  - successfully parses |a| into 'a'
  - successfully parses |3| into '3'
  - successfully parses ||| into '|'

Finished in 0.0306 seconds
3 examples, 0 failures
```

If we modify our first test case by expecting `'b'` instead of `'a'`, while still having `"|a|"` as input, we get:

``` haskell
λ> ParserSpec.main

weird parser - success cases
  - successfully parses |a| into 'b' FAILED [1]
  - successfully parses |3| into '3'
  - successfully parses ||| into '|'
  - successfully parses a digit character from |3|

1) weird parser - success cases successfully parses |a| into 'b'
  expected: 'b'
  but got: 'a'

Randomized with seed 1330009810

Finished in 0.0267 seconds
4 examples, 1 failure
*** Exception: ExitFailure 1
```

Nice! But what else can we test? Well, we can test that what we parse satisfies some predicate, for example. Let's add the following to `spec`:

``` haskell
-- you have to add: import Data.Char (isDigit)
-- in the import list
    it "successfully parses a digit character from |3|" $
      ("|3|" :: Text) ~> weirdParser
        `parseSatisfies` isDigit
```

where

``` haskell
parseSatisfies :: Show a 
               => Either String a -- ^ result of ~>
               -> (a -> Bool)     -- ^ predicate the parsed value should satisfy
               -> Expectation     -- ^ resulting hspec expectation
```

And we get:

``` haskell
λ> ParserSpec.main

weird parser - success cases
  - successfully parses |a| into 'a'
  - successfully parses |3| into '3'
  - successfully parses ||| into '|'
  - successfully parses a digit character from |3|

Finished in 0.0012 seconds
4 examples, 0 failures
```

Great, what else can we do? Well, sometimes we don't really care about the concrete values produced, we just want to test that the parser succeeds or fails on some precise inputs we have, because that's how it's supposed to behave and we want to have a way that changes in the future won't affect the parser's behavior on these inputs. This is what `shouldFailOn` and `shouldSucceedOn` are for. Let's add a couple more tests:

``` haskell
spec :: Spec
spec = do
  describe "weird parser - success cases" $ do

    it "successfully parses |a| into 'a'" $
      ("|a|" :: Text) ~> weirdParser
        `shouldParse` 'a'

    it "successfully parses |3| into '3'" $
      ("|3|" :: Text) ~> weirdParser
        `shouldParse` '3'

    it "successfully parses ||| into '|'" $
      ("|||" :: Text) ~> weirdParser
        `shouldParse` '|'

    it "successfully parses a digit character from |3|" $
      ("|3|" :: Text) ~> weirdParser
        `parseSatisfies` isDigit

    -- NEW
    it "successfully parses |\160|" $ 
      weirdParser `shouldSucceedOn` ("|\160|" :: Text)

  -- NEW
  describe "weird parser - failing cases" $ do

    it "fails to parse |x-" $
      weirdParser `shouldFailOn` ("|x-" :: Text)

    it "fails to parse ||/" $
      weirdParser `shouldFailOn` ("||/" :: Text)
```

where

``` haskell
shouldSucceedOn :: (Source p s s' r, Show a) 
                => p s' a  -- ^ parser to run
                -> s       -- ^ input string
                -> Expectation

shouldFailOn :: (Source p s s' r, Show a) 
             => p s' a  -- ^ parser to run
             -> s       -- ^ input string
             -> Expectation
```

And we run our new tests:

``` haskell
λ> :l example/Parser.hs example/ParserSpec.hs
[1 of 2] Compiling Parser           ( example/Parser.hs, interpreted )
[2 of 2] Compiling ParserSpec       ( example/ParserSpec.hs, interpreted )
Ok, modules loaded: Parser, ParserSpec.
λ> ParserSpec.main

weird parser - success cases
  - successfully parses |a| into 'a'
  - successfully parses |3| into '3'
  - successfully parses ||| into '|'
  - successfully parses a digit character from |3|
  - successfully parses | |

weird parser - failing cases
  - fails to parse |x-
  - fails to parse ||/

Finished in 0.0015 seconds
7 examples, 0 failures
```

I think by now you probably understand how to use the library, so I'll just show the last useful function: `leavesUnconsumed`. This one will just let you inspect the unconsumed part of the input if there's any. Using it, you can easily describe how eager in consuming the input your parsers should be.

``` haskell
  describe "weird parser - leftovers" $ 
    it "leaves \"fooo\" unconsumed in |a|fooo" $
      ("|a|fooo" :: Text) ~?> weirdParser
        `leavesUnconsumed` "fooo"
```

Right now, `hspec-attoparsec` will only consider leftovers when the parser succeeds. I'm not really sure whether we should return `Fail`'s unconsumed input or not.

# Documentation

The code lives at [github.com/alpmestan/hspec-attoparsec](http://github.com/alpmestan/hspec-attoparsec), the package is on hackage [here](http://hackage.haskell.org/package/hspec-attoparsec) where you can also view the documentation. A good source of examples is the package's own test suite, that you can view [in the repo](https://github.com/alpmestan/hspec-attoparsec/blob/master/tests/Test/Hspec/AttoparsecSpec.hs). The example used in this article also lives in the repo, see [example/](https://github.com/alpmestan/hspec-attoparsec/tree/master/example). Let me know through github or by email about any question, feedback, PR, etc.

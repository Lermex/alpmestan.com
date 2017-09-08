---
title: Quick and minimal Haskell hacking with Nix
author: Alp Mestanogullari
date: 2017-09-06 23:00
description: A very short post showing how nix lets me hack quickly on haskell projects without any ceremony.
comments: true
tags: haskell, nix
---

This post will not explain in detail how [Nix](https://nixos.org/nix/) can
build haskell projects or set up development environments,
but simply show one workflow that I use a lot. Please refer to
[this manual](https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure),
[Gabriel Gonzalez's guide](https://github.com/Gabriel439/haskell-nix#nix-and-haskell-in-production)
and various blog posts out there to find out more about Nix and Haskell.

Alright, so I often end up wanting to quickly experiment with some ideas
in a ghci session or a standalone Haskell module. Sometimes I can get away
by simply prototyping my idea with data types and functions from base, but sometimes
(most of the time, really) I want (or need) to use other libraries. I'd like to have
a magic command to which I would give a ghc version and a list of haskell packages,
and I would end up with an environment with ghc/ghci and all the aforementionned
packages pre-installed in that ghc's package database, so that just running `ghci`
would be enough for being able to use those packages. Similarly, standalone modules
could be built just with `ghc --make` for example. No need for a cabal file,
cabal-install or stack.

Well, this is possible with Nix. The `nix-shell` program allows us to enter some
environment that possibly requires downloading, building and running several things.
The precise provisioning process depends a lot on what the environment consists in.
It turns out that the Haskell infrastructure in Nix provides some functions out of
the box for describing environments that consist in a ghc install along with some
Haskell packages from Hackage. Making use of them is as simple as:

``` sh
$ nix-shell -p "haskell.packages.ghc821.ghcWithPackages (pkgs: [pkgs.aeson pkgs.dlist])"
```

This enters a shell with GHC 8.2.1 with aeson and dlist (and their transitive dependencies)
installed in its package database. You might even fetch all those things from a binary cache
with a little bit of luck!

The text between double quotes is a function call, in the Nix programming language.
It calls a function, `haskell.packages.ghc821.ghcWithPackages` (`.` here is simply
field/attribute access, like in many OO languages), provided by the standard Haskell
infrastructure in `nixpkgs`, with an argument that's itself a function:
`pkgs: [pkgs.aeson pkgs.dlist]` is equivalent to (pseudo Haskell code) `\pkgs -> [aeson pkgs, dlist pkgs]`
where `aeson` and `dlist` would be fields of a big record containing all Haskell packages.
So this `ghcWithPackages` function lets us provision a ghc package database using a package
set that it is providing us with, `pkgs`.

_Note_: a haskell package set in nix is basically any record that provides
the packages you need, where packages must be declared under a very precise
shape. The one we're using here is derived from the latest stackage LTS (9.x)
but we could very well be calling `thebestpackageset.ghcWithPackages` instead,
provided that we define `thebestpackageset` somewhere and that its definition
is valid. For example, you could simply extend the latest LTS with a few packages
of yours that nix should get from github, by simply using the record extension syntax
of Nix. Or you could override just a few package versions from the LTS.
Or you could even put together an entire package set yourself.

In that new shell, you can verify that you indeed get the
GHC you asked for and that you can use the said packages:

``` sh
[nix-shell:~]$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.2.1

[nix-shell:~]$ ghc-pkg list
/nix/store/wrrw9c2dsw3d4vmfck7dfx3br33c6pd1-ghc-8.2.1-with-packages/lib/ghc-8.2.1/package.conf.d
    Cabal-2.0.0.2
    aeson-1.1.2.0
    array-0.5.2.0
    attoparsec-0.13.1.0
    base-4.10.0.0
    base-compat-0.9.3
    binary-0.8.5.1
    bytestring-0.10.8.2
    containers-0.5.10.2
    deepseq-1.4.3.0
    directory-1.3.0.2
    dlist-0.8.0.3
    filepath-1.4.1.2
    ghc-8.2.1
    ghc-boot-8.2.1
    ghc-boot-th-8.2.1
    ghc-compact-0.1.0.0
    ghc-prim-0.5.1.0
    ghci-8.2.1
    hashable-1.2.6.1
    haskeline-0.7.4.0
    hoopl-3.10.2.2
    hpc-0.6.0.3
    integer-gmp-1.0.1.0
    integer-logarithms-1.0.2
    old-locale-1.0.0.7
    pretty-1.1.3.3
    primitive-0.6.2.0
    process-1.6.1.0
    random-1.1
    rts-1.0
    scientific-0.3.5.1
    tagged-0.8.5
    template-haskell-2.12.0.0
    terminfo-0.4.1.0
    text-1.2.2.2
    time-1.8.0.2
    time-locale-compat-0.1.1.3
    transformers-0.5.2.0
    transformers-compat-0.5.1.4
    unix-2.7.2.2
    unordered-containers-0.2.8.0
    uuid-types-1.0.3
    vector-0.12.0.1
    xhtml-3000.2.2

[nix-shell:~]$ ghci
GHCi, version 8.2.1: http://www.haskell.org/ghc/  :? for help
Prelude> import Data.Aeson
Prelude Data.Aeson> import Data.DList
Prelude Data.Aeson Data.DList>
```

Finally, you can define a very handy shell function for making it easier to spin up
a new shell without having to remember the precise syntax for the `nix-shell` call above.
Add this to your `~/.bash_profile`, `~/.bashrc` or what have you:

``` sh
function nix-haskell() {
	if [[ $# -lt 2 ]];
	then
		echo "Must provide a ghc version (e.g ghc821) and at least one package"
		return 1;
	else
		ghcver=$1
		pkgs=${@:2}
		echo "Starting haskell shell, ghc = $ghcver, pkgs = $pkgs"
		nix-shell -p "haskell.packages.$ghcver.ghcWithPackages (pkgs: with pkgs; [$pkgs])"
	fi
}
```

The `with pkgs;` bit simply allows us to say `[aeson dlist]` instead of `[pkgs.aeson pkgs.dlist]`,
it just "opens" the `pkgs` "namespace", like `RecordWildCards` in Haskell, when you write `SomeRecord{..}`
to bring all the fields of a record in scope, with the field selector name as variable name for each field.

Now entering our previous shell is even simpler!

``` sh
$ nix-haskell ghc821 aeson dlist
```

Of course, if you need private dependencies or fancier environments, you do need to resort to writing a
`shell.nix` that carefully puts everything together (not that it's that complicated, but definitely
not as simple as this `nix-haskell` command). However, for a quick hacking session or exploration of an idea,
I'm not aware of a nicer solution.

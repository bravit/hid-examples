This is the sample code to accompany the book *Haskell in Depth* (Vitaly Bragilevsky, Manning Publications 2020). 

To get the source code on your system, you may want to clone this repository:
```
$ git clone https://github.com/bravit/hid-examples.git
```

To work with the code on your system, you need either:

* [Stack](http://haskellstack.org)
* [A Minimal GHC installation](https://www.haskell.org/downloads)
* [The Haskell Platform](https://www.haskell.org/platform/)


## Using `cabal` (3.0+)

To build the whole package, issue the following command:
```
$ cabal build
```

To build only one project component:
```
$ cabal build hello
```

To run a component:
```
$ cabal run hello
Up to date
Hello, world
$ cabal -v0 run hello
Hello, world
$ cabal run stockquotes -- data/quotes.csv -p -v
...
```

To explore a component in GHCi:
```
$ cabal repl hello
...
$ cabal repl stockquotes
ghci> :load Statistics
```

To test the whole package:
```
$ cabal test
```

To run one test-suite:
```
$ cabal test locator-test
```


## Using Stack

To build:
```
stack build
```

To run:

```
stack exec <executable> [ -- <arguments>]
```
For example:

```
stack exec stockquotes -- data/quotes.csv -p -v
```

To test:

```
stack test
```


Unfortunately, exploring code in GHCi doesn't work due to [bug in stack](https://github.com/commercialhaskell/stack/issues/4564):

```
stack ghci <module file>
```

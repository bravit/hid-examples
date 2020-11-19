This is the sample code to accompany the book *Haskell in Depth* (Vitaly Bragilevsky, Manning Publications 2021).

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
$ cabal run stockquotes -- data/quotes.csv -c
...
$ file chart.svg
chart.svg: SVG Scalable Vector Graphics image
```

To explore a component in REPL:
```
$ cabal repl hello
...
$ cabal repl stockquotes
ghci> :m StatReport
ghci> :type mean
mean :: (Fractional a, Foldable t) => t a -> a
```

To test the whole package:
```
$ cabal test
```

To run one test-suite:
```
$ cabal test radar-test
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
stack exec stockquotes -- data/quotes.csv -c
...
$ file chart.svg
chart.svg: SVG Scalable Vector Graphics image

```

To test:

```
stack test
```

To explore a component in REPL:

```
$ stack repl :stockquotes
ghci> :m StatReport
ghci> :t mean
mean :: (Fractional a, Foldable t) => t a -> a
```

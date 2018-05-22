This is the sample code to accompany the book *Haskell in Depth* (Vitaly Bragilevsky, Manning Publications 2019).

To work with the code on your system, you need either:

* [Stack](http://haskellstack.org)
* [A Minimal GHC installation](https://www.haskell.org/downloads)
* [The Haskell Platform](https://www.haskell.org/platform/)



## Using Stack

### Building

```
stack build
```

### Running

```
stack exec <executable>
```

Or with command line arguments:
```
stack exec <executable> -- <arguments>
```

For example:

```
stack exec stockquotes -- data/quotes.csv -p -v
```

### Exploring in GHCi

```
stack ghci <module file>
```

e.g.:

```
stack ghci stockquotes/Statistics.hs
```

## Using Cabal sandbox

### Building

```
cabal sandox init
cabal install --only-dependencies
cabal configure
cabal build
```

### Running

```
cabal run <executable>
```

Or with command line arguments:
```
cabal run <executable> -- <arguments>
```

For example:

```
cabal run stockquotes -- data/quotes.csv -p -v
```

### Exploring in GHCi

```
cabal repl <executable>
```

e.g.:

```
cabal repl stockquotes
```

To work with particular module you have to load it in GHCi with `:load` then.


## Using Cabal new-*

### Building

```
cabal new-build
```


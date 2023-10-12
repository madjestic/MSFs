# MSFs
experiments with Monadic Streaming Functions, which, supposedly, culminate in a simple game example.

## Creating a new Material:
```bash
$ cabal run genMaterial matDir/matName
$ cabal run genMaterial mat/graph
> ./mat/graph/graph...
```
(that generates a ./mat/testMat01 material directory with a default conent (constant shader material)

## Fix material's UUIDs:
```bash
$ cabal run exe:genUUID -- -m mat/test/test
```


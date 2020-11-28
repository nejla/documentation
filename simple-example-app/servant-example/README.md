# An example REST api

This directory contains an example rest API demonstrating the use of
[servant](https://hackage.haskell.org/package/servant) and
[hspec-wai](https://hackage.haskell.org/package/hspec-wai)

## The server

[Kitteh.hs](src/Kitteh.hs) contains a simple API that allows us to store, update
and retrieve a list of cats and their colors.

[App.hs](src/App.hs) is the main application. It consolidates all the
sub-endpoints (only one, in our case) and creates the runnable webserver.

## The test suite

[KittehSpec.hs](test/Kittehspec.hs) has the tests relating to the Kitteh.hs module.

[Spec.hs](test/Spec.hs) combines all the test modules (only one, in this case)
using hspec-discover

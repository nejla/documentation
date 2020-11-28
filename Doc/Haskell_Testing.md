# Testing

## Why and how to test

Before we can write code we need to understand at least _roughly_ what it is
supposed to be doing (otherwise, what business do we have writing it?) But once
we have written it, it's anyone's guess whether it does what we would like it to.
(Usually it does not), But how do we know?

Let's for example look at this toy function, which adds 2 to an Int:

``` haskell
frobnicate :: Int -> Int
frobnicate x = go $ go x
  where
    go = succ
```

Ideally we'd like to have a formal specification and prove that our code
works as intended (perhaps using a proof assistant like coq), but that's often
not practical. So we settle for the next-best thing: checking it for compliance
empirically.

That's quite a natural thing to do, actually. Once we are finished writing the
code and finagling the compiler into accepting it, we are inclined to _try_
it. This might be as simple as invoking it in GHCi and comparing the result to
our expectation (which might be as low as it not crashing and burning).

```
>>> frobnicate 3
5
```

This is already a test. All we need to do is copy the invocation into a
.hs file, and encode our expectation so it gives us a True or False answer
that we don't have to check manually.

``` haskell
testFrob = frobnicate 3 == 5
```

To make the test a little more usable we would use some functionality of hspec
to describe what we are testing:

``` haskell
testFrob = describe "frobnicate" $
    it "returns 5 when give 3" $
        frobnicate 3 `shouldBe` 5
```

The `shouldBe` function not only tests for equality, but also gives us the
actual value when the test fails, simplifying debugging.

## Unit Tests

Unit tests test a _single_ unit of functionality. Each test should test a single
property and have a three-part structure (called the "Arrange-Act-Assert
pattern", or AAA short): First we arrange the state of the application so that
preconditions are fulfilled. Then we invoke the component under test (act), and
finally we check post-conditions (assert).

This makes it obvious what is being tested and avoids common testing smells,
such as testing more than one thing at a time or mixing setup and acting steps.

Consider this [library](testing/src/Persistent.hs). It contains an "transactionTotal"
which we would like to test. It's intended purpose is to return the sum of all
transactions belonging to an account. A test could look like this:

``` haskell
do
  -- Arrange
  insertTransaction "a1" 200
  insertTransaction "a1" 300

  -- Act
  total <- transactionTotal "a1"

  -- Assert
  liftIO $ total `shouldBe` 500
```

The comments delimiting the arrange/act/assert parts are only added for
demonstration and would not be added to the actual tests. However,
separating the parts with a blank line is a good idea.

Note that we are _not_ testing the `insertTransaction` function here, but assume
it behaves correctly (we want to have separate tests for it). Note that we are
also _not_ testing the effect transactions in different accounts have on this
total. This is also a separate test. By keeping tests small and focused we gain
a much clearer picture of how end when things fail

In general, we want to write unit tests as early as possible, maybe even before
the actual code. This forces us to clearly state our intent and encourages code
that's logically structured and easy to follow.


## Frameworks

There are two major contenders:
[Hspec](https://hackage.haskell.org/package/hspec) and
[Tasty](https://hackage.haskell.org/package/tasty).

Both promise to fulfil our wishlist at different tradeofs. Luckily we don't need
to choose as there are libraries that convert between the two
(e.g. [tasty-hspec](https://hackage.haskell.org/package/tasty-hspec)), so we may
simply select the one we like better.

We will conentrate on hspec, because it seems to be used more often and the
[hspec-wai](https://hackage.haskell.org/package/hspec-wai) library is so useful.

It comes with QuickCheck-support of out of the box, just use the `property`
function like this:

```haskell
spec = do
  describe "frobnicate" $ do
    it "Is strictly increasing" $ property $ \x ->
        frobincate x > x
```

[Example](testing/test/UnitTest.hs)

First we need to include the relevant module

```haskell
import Test.Hspec
```

Ignore the withTestDB function for now (it's necessary to run code with a test
database).

To define a test, we describe a component

```haskell
  describe "transactionTotal" $ do
```

then, in a component, we can define individual test cases.

```haskell
    it "returns the sum of transactions" . withTestDB $ do
      insertTransaction "a1" 200
      insertTransaction "a1" 300

      total <- transactionTotal "a1"

      liftIO $ total `shouldBe` 500
```

We do so by describting the expected behaviour: `it "returns the sum of
transactions"` and then running code that checks that the tested code actually
behaves as expected

## Example project

See [servant-example](servant-example)


## Unit Tests for Database Transactions

### Using SQLite memory database

Possibly the easiest way to test database-related code is to run it in an sqlite
in-memory database. To do this, we create a new database before each test and
tear it down afterwards. Here's a simple function that accomplishes that:

``` haskell
import Database.Persist.Sqlite

withTestDB f = runNoLoggingT . withSqliteConn ":memory:" $ \backend -> lift $
  runReaderT ( do
      runMigrationSilent migrateAll    -- Don't want chatter between tests
      f
             ) backend
```

note that `migrateAll` comes from persistent's schema-generating code. To run a
test with the memory database, we pass it to withTestDB:

``` haskell
spec = do
  describe "my component" $ do
    it "does something" . withTestDB $ do
      <your test code here>
    it "does something else" . withTestDB $ do
      <more test code>

```

See [example](testing/test/UnitTest.hs)

### Using external database

* Much harder
* Need to reset database after each test
* Re-creating schema after each test too slow, truncating tables faster
* Need to make sure database is set up before tests are run and connection
  details are available
* Inconvenient for CI

## Property based testing

Instead of writing test cases by hand (which is tedious and bound to miss some
corner case), we can let a library generate them for us

### QuickCheck

QuickCheck generates test cases. We write down a _property_ we think should
hold, and QuickCheck tries to come up with counterexamples.

For example, our frobincate function should always return a result that is larger than it's input:

``` haskell
prop_FrobnicateIncreasing x =
  frobnicate x > x
```

(It is customary to prefix properties with "prop_", allowing frameworks to find
them more easily

Running it with `quickCheck` confirms that this seem to be the case:

```
>>> quickcheck prop_FrobnicateIncreasing
+++ OK, passed 100 tests.
```

One problem with QuickCheck is that it doesn't necessarily find a good counterexample

Take

``` haskell
prop_FrobincateSmall x =
  frobnicate x < 100
```

we are claiming that the result of frobincate will always be smaller
than 100. That's plainly ridiculous (just try frobnicate 100), and yet, QuickCheck seems satisfied:

```
>>> quickCheck prop_FrobincateSmall
+++ OK, passed 100 tests.
```

Why is that? We can find out by diagnosing the test cases QuickCheck generated

``` haskell
prop_FrobnicateSmallDiag x = collect x $
    frobnicate x < 100
```

and

```
>>> quickCheck prop_FrobnicateSmallDiag
+++ OK, passed 100 tests:
 4% 0
 3% 10
 3% -35
 3% -3
 3% -19
[...]
```

We see that x never reaches the value of 98 which is necessary for the test cast
to fail. It turns out that in order to avoid unreasonable blowup
of the example size, there is a "size" parameter which is set to 100 by
default. In case of Ints that means it won't generate integers larger than 100,
and even values close to 100 will be rare. We can fix the problem by increasing
the parameter:

``` haskell
check :: Testable prop => prop -> IO ()
check = quickCheckWith args
  where
    args = stdArgs {maxSize = 1000}
```

and now QuickCheck will find the problem reliably:

```
>>> check prop_FrobnicateSmall
*** Failed! Falsifiable (after 15 tests):
98
```

To help avoid problems with unrepresentative samples we can use the `cover`
function: It checks that a sufficient portion of the sample satisfies a predicate:

```
prop_FrobnicateSmallChecked :: Int -> Property
prop_FrobnicateSmallChecked x = cover (x > 100) 10 "large" $
    frobnicate x < 100
```

We are demanding that at least 10% of the examples are larger than 100 (we
label them as "large", which is only for the diagnostic). And now the default
"quickCheck" will tell us that it didn't generate enough large examples:

```
>>> quickCheck prop_FrobnicateSmallChecked
*** Insufficient coverage after 100 tests (only 0% large, not 10%).
```

* Shrinking
* Arbitrary instances

## Hedgehog
* [Hedgehog](https://hackage.haskell.org/package/hedgehog)

### Differences:
* Quickcheck requires instances (Arbitrary), Hedgehog does not
* Hedgehog more flexible, no orphan instances, shrinking "for free"
* Quickcheck more mature, more existing libraries, better known
* Hedgehog can make use of QuickCheck instances

QuickCheck requires `Arbitrary` instances for data types
we want auto-generated, but many packages don't provide them. This leads to the
problem of orphan instances. Hedgehog on the other hand allows us to specify the
generator where we are using it, giving us improved flexibility and avoiding the
orphan problem.


## Integration Tests
* Test multiple components
* Test interaction between components
* Test only one property at a time
* Three-part structure (AAA-pattern)

### hspec discover

Having to collect all the tests is tedious and
error-prone. [hspec-discover](https://hackage.haskell.org/package/hspec-discover)
automatically finds our test cases and bundles them into a tree.

#### Usage

Copy the following into the main module of your test suite (it should contain
nothing else except comments):

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

This instructs ghc to run hspec-discover as a preprocessor. It will look for
files ending in "Spec.hs" (e.g. "ApiSpec.hs", "UserSpec.hs" etc) and consolidate
them into a test suite. Each of them has to contain a value named `spec` of type
`Spec`


See [documentation](https://hspec.github.io/hspec-discover.html)


### hspec-wai

Hespec-wai helps to test WAI-based REST service (for example those written in
servant) without having to actually run a web server.

Let's look at a [simple API](servant-example/src/Kitteh.hs):

```haskell

type KittehAPI = "kitteh" :> (Get '[JSON] [Kitteh]
                              :<|> ReqBody '[JSON] Kitteh :> Post '[JSON] NoContent
                             )

```

The server stores a list of cats (each one having a name and a color), and
returns the list on request. We can add or update a cat by POSTing it.  So we
have Two end-points
* `GET /kitteh`, returning a list of cats
* `POST /kitteh`, accepting a cat in the request body and not returning anything.

Let's [test](servant-example/test/KittehSpec.hs) that the API works as advertised:

``` haskell
spec :: Spec
spec = with app $ do
    describe "GET /kitteh" $ do
        it "responds with 200" $ do
            get "/kitteh" `shouldRespondWith` 200
        it "responds with [Kitteh]" $ do
            get "/kitteh" `shouldRespondWith` [json|[ {"kittehName":"Mittens","kittehColor":"Tabby"}
                                                    , {"kittehName":"Fluffy","kittehColor":"Orange"}
                                                    ] |]
    describe "POST /kitteh" $ do
      it "adds a new kitteh" $ do
        postJ "/kitteh" [json|{"kittehName":"Tigger","kittehColor":"Brown"}|]
          `shouldRespondWith` 200
        get "/kitteh" `shouldRespondWith`
            [json|[ {"kittehName":"Tigger","kittehColor":"Brown"}
                  , {"kittehName":"Mittens","kittehColor":"Tabby"}
                  , {"kittehName":"Fluffy","kittehColor":"Orange"}
                  ] |]
      it "replaces an existing kitteh" $ do
        postJ "/kitteh" [json|{"kittehName":"Fluffy","kittehColor":"Black"}|]
          `shouldRespondWith` 200
        get "/kitteh" `shouldRespondWith`
            [json|[ {"kittehName":"Fluffy","kittehColor":"Black"}
                  , {"kittehName":"Mittens","kittehColor":"Tabby"}
                  ] |]

```

We start by telling hspec-wai which API to test (`with app`). Then we describe
the component we are testing (`describe "GET /kitteh"` and `describe "POST
/kitteh"` respectively) In each of those blocks we introduce test cases by
describing the property they test (`it "respons with 200"` etc.) and then give
the actual test code.

In the tests we use the `postJ` function, a simple helper very similar to `post`
which just sets the `Content-Type` header to "application/json":

```haskell
postJ :: ByteString -> BSL.ByteString -> WaiSession SResponse
postJ json = request "POST" json [("Content-Type", "application/json")]
```

We use `shouldRespondWith` to check that the response matches our
expectations. It is polymorphic in it's second argument and accepts numerals
matching the return code (e.g. `200`), string literals that match the body or
more complex ResponseMatchers. We use the `json` quasiquoter to construct such
a matcher that parses the result as JSON and compares it to the value we gave,
making the test robust against perturbations in the returned json string that
don't change the semantics

Note that we are still adhering to the AAA pattern, even though
`shouldRespondWith` condenses the Act and Assert step into a single expression.


* [Another Example](https://github.com/hspec/hspec-wai#readme)
* [Documentation](https://hackage.haskell.org/package/hspec-wai-0.8.0/docs/Test-Hspec-Wai.html)


* Use `with app` to run spec in the context of a WAI `Application`
* Use get/put/post/patch etc. to invoke endpoints
* `shouldResponsWith` to assert the expected result

### servant-quickcheck
https://hackage.haskell.org/package/servant-quickcheck
### servant-mock
https://hackage.haskell.org/package/servant-mock


### doctest
* Test properties in documentation
* Great for examples and invariants that need to be documentated anyway
* Ensures that documentation stays correct.


https://hackage.haskell.org/package/doctest
https://hackage.haskell.org/package/cabal-doctest

## How to fix a bug
* Write test first
* Fix Bug
* Check that test passes

## Getting started

Setting up tests requires some boilerplate. The easiest way to get it in place
is to use an appropriate template when creating the project, e.g. with stack:
`stack new <project-name> servant`

Or use the following snippets

Add this to the project cabal file, replace $(library) with the name of the
library you're testing. It includes some of the most common libraries, but you
will have to add you own depending on your needs.

You should add the modules containing the test cases to other-modules. It is a
good idea to have one test module for each source module.

```
test-suite tests
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test
  main-is:             Spec.hs
  other-modules:       $(add test modules here)
  build-depends:       base
                     , $(library)
                     , HUnit
                     , bytestring
                     , exceptions
                     , hspec
                     , hspec-wai
                     , hspec-wai-json
                     , lens
                     , mtl
                     , text
                     , uuid
                     , wai
                     , wai-extra
  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```

Explanation of the ghc options:

* -Wall enables useful warnings. It should be enabled by default in every
  project and only individual warnings disabled on a case-by-case basis
* -threaded selects the threaded runtime. This enables the use of multiple OS
  threads, e.g. to run foreign code without blocking. The number of threads that
  can evaluate Haskell code at the same time is called _capabilities_ and by
  default is 1 (i.e. while a program might use multiple OS threads to avoid
  locking, it won't run _Haskell_ code concurrently)
* -rtsopts enables the ability to pass options to the run time system (rts)
* -with-resopts sets some options to the rts by default (in this case it
  allocates a number of _capabilities_ that is equal to the available number of
  processers/cores, so multiple threads of Haskell code can run at the same
  time, if enough processors are available to handle it)

or if you're using hpack, add this to your package.yaml:

```yaml
tests:
  tests:
    main: Spec.hs
    source-dirs: test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - base
    - $(library)
    - HUnit
    - bytestring
    - exceptions
    - hspec
    - hspec-wai
    - hspec-wai-json
    - lens
    - mtl
    - text
    - uuid
    - wai
    - wai-extra
```

Note that you do not need to add dependencies here that you have declared globally.

in test/Spec.hs, you only need to add

```
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

and for each Module in test/$(Module).hs :

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Remove modules you don't need
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec = describe "component" $
  it "does something" $ do
    True `shouldBe` True

```

## Testing Email

A convenient way to test the actual sending of email (rather than just stubbing
out out / mocking it) is [mailhog](https://github.com/mailhog/MailHog).

Advantages compared to a real mail server ar that it can be run locally in a
docker container and controlled via an API (for programmatic checking of the result).


Example docker-compose service section:

```yaml
  mail:
    image: mailhog/mailhog:latest
    networks:
      - {{network}}
    environment:
      - MH_HOSTNAME=authservice.test
    ports:
      # For web UI access. Not necessary/advisable for CI
      - 8025:8025
```

(Fill in {{network}} )

Example msmtprc file:

```
defaults

timeout 30

account defaultaccount
host mail
port 1025
from {{fromAddress}}
auth off

account default : defaultaccount
```

(Fill in {{fromAddress}} )

Note that you don't need authentication or tls/starttls (and in fact it won't work with it enabled)

Then we can access the mails via the [API](https://github.com/mailhog/MailHog/blob/master/docs/APIv1.md)


# Best practices

Warnings help us avoid bugs and write better code. We always want to have -Wall
enabled always and only disable specific warnings on a case by case basis.

Enabling the threaded runtime (add -threaded to ghc-options) allows Haskell
programs to use multiple OS threads This means that FFI calls can run
concurrently with Haskell code and don't block the entire program. Note that FFI
calls marked as _unsafe_ still block the capability. The run time system will
create new OS threads to facilitate the concurrent execution of FFI calls.
[\[1\]](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#multi-threading-and-the-ffi).
The threaded runtime by itself will not however by default spead the execution
of Haskell code to multiple threads, rather the number of threads that can
execute Haskell code is determined by the number of _capabilites_, each of which
resides on an OS thread and can execute Haskell code. The number of capabilites
must be set during program start as a rts option (see below)

The downside of the threaded runtime is a slight increase in overhead for safe
FFI calls and care must be taken to always call non-thread-safe FFI functions
from the same OS thread (see forkOS for bounded threads). However, these
downsides are small compared to the upsides, as programs blocking for seemingly
no reason can be a major headache and parallelizing the Haskell code can give
performance improvements for little cost, so it is a good idea to default to the
threaded runtime, even if the number of capabilities is left at 1.

The number of capabilites can be set to p via the `-Np` rts option (defaults to
one but is set to the number of available processors/cores if p is left out)

Example ghc options: `-threaded -rtsops -with-rtsopts=-N`

It select the threaded runtime, enables run time system options and sets the
default options to use on capability per available processor/core, so it doesn't
need to be set at each program start

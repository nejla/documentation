# Testing

Now that we have a running web server, an important topic to cover is *testing*. We want to start writing tests as soon as possible, since bugs are much cheaper to fix early in the development process.

# Testing the API

We can write tests against our API using [hspec-wai](https://hackage.haskell.org/package/hspec-wai). Using the library we bypass the network and HTTP layers and match requests directly against out handlers. This simplifies and speeds up the tests (we don't want to test the web server or our OS' networks stack)

Let's a test for our API.

First we need to import the relevant modules:

```
import Data.Data           (Proxy(..))
import Test.Hspec
import Test.Hspec.Wai      as Wai
import Test.Hspec.Wai.JSON (json)

import NejlaCommon.Test    as NC

import RunMain             ( AppState(..) , AppConfig(..)
                           , migrate, serveApp
                           )
```



First we need to create a configuration that our web server should run it:


```haskell
testAppState =
  AppState
  { appStateConfig =
      AppConfig
      { appConfigEmail = "test@example.com"
      }
  }
```

Next we want to write a simple test case; we create a user and retrieve it,
hoping that we get the same data back. Using
[hspec](https://hackage.haskell.org/package/hspec) syntax we `describe` the
thing under test (in this case, the `/users` resource). Then we use `it` to
start a test case. `it` takes a textual description of what we are testing (it
should read like a sentence) and a test script.

```
specs :: DBApiSpec ()
specs = do
  describe "/users" $ do
    it "accepts a POST request" $ do
      postJ "/users"
        [json|{ "name": "Robert"
              , "points": 33
              , "group": "development"
              }
             |] `shouldRespondWith` 201
```

Some things of note:

* postJ (short for "post json" creates a POST request to a resource with the
  "Content-Type" header set to "application/json"
* The `[json| ... |]` construction is a quasi quoter, allowing us to write
  literal json that is converted to Haskell code during compilation.
* Don't muss the `shouldRespondWith`, which checks that the call reponds with
  the expected error code.



finally we need to write a main function that runs these tests:

```haskell
main :: IO ()
main = do
  connectInfo <- dbTestConnectInfo
  specApi connectInfo migrate mkApi specs
  where
    mkApi pool run =
      liftIO $ run () (serveApp pool testAppState)
```

* dbTestConnectInfo reads database connection information from environment
  variables, defaulting to the PostgreSQL defaults when the variables are unset
* specApi connects to the database, clears it out and runs the migration (the
  one we have imported from our library) and cleans the database between every
  test (by running "DELETE FROM ..." on every table.
* **NB** this is obviously destructive. Never run the test suite with a database
  that contains data you want to keep.
* mkApi allows us to run additional setup before and after every test. It get's
  the (already migrated and cleaned) database pool and the actual test which we
  have to call with additional state. Say we needed to create a temporary
  directory for every test case (and clean it up after the test is done, even
  when an exception occurs), then we could write:

```haskell
mkApi pool run = liftIO $
    withSystemTemporaryDirectory "my-temp-dir" $ \path ->
      run path (ServeApp pool testAppState)
```


# Building the tests

Our tests will need access to our infrastructure - at least a database. Since we
are already using Docker to coordinate the various parts of our system it stands
to reason that we should rely on it for running the test suite too.

Firstly, we need to define a Dockerfile to run the tests in. Docker has
[multi-stage
builds](https://docs.docker.com/develop/develop-images/multistage-build/) which
allow us to re-use the Dockerfile we use for our production build, which not
only saves us effort but also makes it easier to ensure that the testing and
production environments stay in sync. All we need to do to enable multi-stage
builds is to add stage annotations to our existing Dockerfile like this:

```Dockerfile
FROM ubuntu:eoan AS baseimage

LABEL project=nejlacommon-tutorial

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y --no-install-recommends  \
      ca-certificates \
      curl \
      libicu-dev \
      libpq-dev \
      locales \
      netbase  \
        && \
    rm -rf /var/lib/apt/lists/* && \
    localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

ENV LANG=en_US.UTF-8

FROM baseimage

LABEL project=nejlacommon-tutorial

ADD ./dist/* /app/

ENTRYPOINT ["/app/nejlacommon-tutorial"]
```

note the `FROM «baseimage» as «stage»` line at the top, it declares that we are
starting a new stage. We also added a new line `FROM «stage»` in the middle,
indicating that everything that comes after builds on top of the earlier stage.

Now we can create the baseimage via `docker build . --target baseimage`. Instead of doing that by hand, though, we will write a docker-compose file, `docker-compose.testing.yaml` :

```yaml
version: '3.4'

services:
  testing-database:
    image: postgres:12
    environment:
      POSTGRES_HOST_AUTH_METHOD: trust
    networks:
      test:
    command: -c "fsync=off" -c "synchronous_commit=off" -c "full_page_writes=off"
  test:
    build:
      context: .
      target: baseimage
    environment:
      DB_HOST: testing-database
    volumes:
      - ./dist/tests:/tests:ro
    networks:
      test:
    links:
      - testing-database
    command: sh -ec 'for t in /tests/*; do $$t; done'

networks:
  test:
```

Let's go over this file step by step.

* We need a compose-file version of at least 3.4 for Dockerfile stages to work.
* Next we declare the database that we want to use for testing, similarly to the
  one we declared for production. We pass it some additional parameters that
  turn off data integrity for a bit of improved performance (since we don't
  really care about data loss in the testing database in case of a crash)
* Next we declare the test service.
* We want to use the baseimage as declared in the Dockerfile, docker-compose
  will automatically build it if necessary.
* We tell our test suite how to reach the database by setting the DB_HOST
  environment varliable. The test-database host will be reachable within the
  docker container because it's in the same network named "test"
* We volume in the built test executable (read only). We could bake it into an
  image, but that's unnecessary since we only want to run it once locally and
  not distribute it
* As mentioned earlier, we want the test container to be in the same network as
  the database so it can reach it.
* Declaring a link ensure that `docker-compose run` will start the database
  service for us when we run the `test` service as a one-shot
* The command is a short shell-script that runs every test executable in turn,
  stopping on the first failure with an error code (the `-e` flag)

If we need additional services for testing
(e.g. [mailhog](https://hub.docker.com/r/mailhog/mailhog) for testing emails) we
would declare them here and add them in the `links` section so they get started
when we run `docker-compose run`

This gives us a neat description of the environment our tests should run in. To
start the tests all we need to do is

* Ensure that test executabes are built and availabe in `dist/tests`
* run `docker-compose -f docker-compose.testing.yaml run --rm test`

This will start the linked services and then run the test script.

Let's add it to our Makefile:

```Makefile
.PHONY: test
test: $(tests)
	docker-compose -f docker-compose.testing.yaml run --rm test
	docker-compose -f docker-compose.testing.yaml down
```

Running tests is now as simple as `make test`; the project will be built,
necessary services be started and finally the built tests are run. We leave the
database up and running after the tests are done, which improves startup time un subsequent tests and enables debugging, in case we need to go dig through the database after a test


Let's also amend the `clean` target to remove containers and volumes

```Makefile
.PHONY: clean
clean:
	docker-compose -f docker-compose.testing.yaml down -v
    ...
```

# Running the tests

To run the tests, simply `make test`:


```
# make test
 [... happy compiler noises]
/users
  accepts a POST request

Finished in 0.0041 seconds
1 examples, 0 failures

 [...]
```

# More tests

Let's also test `GET`ing from the `/users` resource. To accomplish that, we add another case:


``` haskell
specs = do
  describe "/users" $ do

    -- [...]

    it "GETs the user" $ do
      postJ "/users"
        [json|{ "name": "Robert"
              , "points": 33
              , "group": "development"
              }
             |] `shouldRespondWith` 201

      users <- get "/users" `shouldReturnA` (Proxy @NC.JSON)
      users `NC.shouldBe` [json|[{ "name": "Robert"
                                 , "points": 33
                                 , "group": "development"
                                 }]
                               |]
```

* Note that the new test case starts from an empty database again, so we have to
  start by creating the user before we can fetch it. This helps ensure that the
  test cases are reproducible.
* We could factor out the code to create a new users, especially if we use it
  more often. It's e good idea to have it spelled out explicitly when it is the
  focus of the test (as it was in the first test case), but as part of the setup
  routine it adds a lot of noise.
* The `get` action fetches data from a resource.
* We use `shouldResturnA` to assert a successful HTTP status code (200-299) and
  that the response body can be parsed as JSON. It will then return the parsed
  result.
* We pass a `Proxy` value to `shouldReturnA` to indicate which type it should
  parse as.
* `@NC.JSON` is a "visible type application". It is another way of writing
  `Proxy :: Proxy NC.JSON`
* `NC.JSON` means that we want the result as (structural) JSON, but not parsed
  any further. It is a newtype wrapper around Aeson's
  [`Value`](https://hackage.haskell.org/package/aeson-1.5.3.0/docs/Data-Aeson.html#t:Value)
  type.
* We use the `json` quasi-quoter again to write out the json we expect as result
* `NC.shouldBe` to assert that the response is equal. The two json values are
  compared structurally (because that's how `Eq` is implemented on the `JSON`
  type). If the equality fails, both values are printed as json and differences
  are highlighted.

Let's run this test:

```
# make test
  [...]
/users
  accepts a POST request
  GETs the user

Finished in 0.0053 seconds
2 examples, 0 failures
[...]

```

Let's see what happens when we intentionally mess up a test. For example, say we
changed the second test.

```haskell
    it "GETs the user" $ do
      postJ "/users"
        [json|{ "name": "Robert"
              , "points": 33
              , "group": "development"
              }
             |] `shouldRespondWith` 201

      users <- get "/users" `shouldReturnA` (Proxy @NC.JSON)
      users `NC.shouldBe` [json|[{ "name": "Peter"
                                 , "points": 33
                                 , "group": "development"
                                 }]
                               |]
```

Note how we have changed `Robert` to `Peter`

```
# make test
users
  accepts a POST request
[Debug#SQL] SET client_min_messages TO ERROR;
SET CONSTRAINTS ALL DEFERRED;
DO $$
DECLARE
    statements CURSOR FOR
        SELECT tablename FROM pg_tables
        WHERE schemaname = 'public';
BEGIN
    FOR stmt IN statements LOOP
        EXECUTE 'DELETE FROM ' || quote_ident(stmt.tablename)
          || ';';
    END LOOP;
END;
$$;
RESET client_min_messages;
; []

[Debug#SQL] SET TRANSACTION ISOLATION LEVEL READ COMMITTED; []

[Debug#SQL] INSERT INTO "user"("name","points","group") VALUES(?,?,?) RETURNING "id"; [PersistText "Robert",PersistInt64 33,PersistText "development"]

[Debug#SQL] SET TRANSACTION ISOLATION LEVEL READ COMMITTED; []

[Debug#SQL] SELECT "id", "name", "points", "group" FROM "user"; []

  GETs the user FAILED [1]

Failures:

  test-suite/Main.hs:37:5:
  1) /users GETs the user
       expected: [{"points":33,"group":"development","name":"Peter"}]
        but got: [{"points":33,"group":"development","name":"Robert"}]
```

As you see, we get a hole bunch of stuffdata dumped into our lap, let's look
through it:

* The first test is still passing
* Next we get a dump of the logs, which are hidden for passing tests. In our
  case, the logs consist of all the SQL statements that were run.
* Finally, we get a report of the failure. It includes an explanation of what
  went wrong, in our case it tells us that the response we got differs from the
  assertion, which is exactly what we hoped to see
* Note that the json you see is parsed and re-serialized, so it might be
  slightly different from what the server actually produced, but it should be
  structurally equivalent

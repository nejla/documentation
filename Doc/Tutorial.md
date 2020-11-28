# NejlaCommon Tutorial

NejlaCommon is a library containing tools for writing a database-driven web
backend using the Haskell packages `servant` and `persistent-postgres`. In
particular, it strives to provide better integration ("glue") between the parts,
missing functionality ("batteries") for a typical webserver and opinionated
defaults. Users should be able to concentrate on writing business code.

# How to get started

NejlaCommon is designed to work with Postgresql as a database backend and Docker
for deployment.

# Packages

NejlaCommon works with the following packages:
 * [persistent](https://hackage.haskell.org/package/persistent) and [persistent-postgresql](https://hackage.haskell.org/package/persistent-postgresql) for database connectivity
 * [Esqueleto](https://hackage.haskell.org/package/esqueleto) for generating SQL
 * [servant-server](https://hackage.haskell.org/package/servant-server) as API
   middleware
 * [warp](https://hackage.haskell.org/package/warp) as webserver

# Database

Almost any web service will have to store some data; however, the service itself
should be stateless, that is, it should not (itself) remember any data between
requests; this simplifies the design and allows freely restarting and scaling
it. Instead, data should be stored in a database. NejlaCommon assumes the use of
Postgresql.

# Getting started
## Preamble
### Extensions

We will assume the following extensions are enabled, either in each source file
(e.g. `{-# LANGUAGE OverloadedStrings #-}`) or as defaultExtensions in the
package.yaml

* `OverloadedStrings` for Text literals (and rearely ByteStrings,
  e.g. postgres connection string)
* StrictData - This simplifies reasoning about performance a lot and we rarely
  need to define our own lazy data types.
* DataKinds - For working with type level parameters


Persistent requires these extensions:

* GADTs
* TypeFamilies
* DerivingStrategies
* GeneralizedNewtypeDeriving
* StandaloneDeriving
* UndecidableInstances
* TemplateHaskell
* QuasiQuotes - if you are using inline schema definition

### Imports

We import NejlaCommon qualified or with explicit import lists (or both),
such as

```haskell
import qualified NejlaCommon as NC
import           NejlaCommon (db')
```

@TODO!

# Configuration

Our server will probably need some way to be configured, e.g. the port to
listen on, Email credentials etc. To keep track of these options we create a data type to contain them:

```haskell
data AppConfig = AppConfig
  { appConfigEmail :: Text -- just an example
  }
```

This data will then be passed around in what's called the [ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/).

Typically in a docker environment we woudl configure the webserver via
environment variables, but NejlaCommon also supports reading a configuration
file. To get started we load the configuration file (if it exists)

```haskell
  conf <- loadConf "my-app"
```

`loadConf` expects the configuration file under `/data/my-app.conf` (where
my-app is the parameter passed to loadConf). The loading of the configuration
file is skipped if none exist, so if we don't want to use files we can ignore
them.

Alternatively we can use the load function from `Data.Configurator` from the [configurator](https://hackage.haskell.org/package/configurator) package for more flexibility.

Then we use the `getConf`-functions to parse the configuration; they will
check both environment variables and the configuration files.

`getConf` takes the following arguments:

* The environment variable to read from (e.g. "DB_USER")
* The configuration file option with dots separating sub-options (e.g. "db.user")
* The configuration object to read from (the one we got from loadConf)
* `Either` an error text describing the expected value in case the value was not provided or a default value

`getConf` will return the configured text value or default value if either is
set or abort the program after printing an error message. This means that all
configuration parsing should be done _before_ that main program is run. This is
a good idea anyway to avoid what's called [shotgun
parsing](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/),
i.e. we don't want configuration parsing to be able to fail after the
server has already done work.

There also exist the following variants:

* `-Generic`: Takes a function that parses the configuration value from text
* `-Maybe`: Will return `Nothing` if the variable is unset unstead of aborting the program
* `-'`: (apostrophe), will use the "Read" instance to parse the value
* `-Bool`: Will parse the text value as a boolean, accepting `true` and `false`
  as values (case insensitive)

So for example to parse database-related options and generate a connection
string:

```haskell

parseDatabaseConf :: (MonadLogger m, MonadIO m) => Config -> m ConnectionString
parseDatabaseConf conf = do
   dbHost <- getConf "DB_HOST" "db.host" (Right "localhost") conf
   dbPort <- getConf' "DB_PORT" "db.port" (Right 5432) conf -- uses read instance
   dbUser <- getConf "DB_USER" "db.user" (Left "database user name") conf -- will abort Program when value is unset
   dbPassword <- getConf "DB_PASSWORD" "db.password" (Right "") conf
   dbDatabase <- getConf "DB_DATABASE" "db.database" (Right dbUser) conf

   -- Uses postgresql-simple for more structured configuration
   return . postgreSQLConnectionString $
     ConnectInfo { connectHost = Text.unpack dbHost
                 , connectPort = dbPort
                 , connectUser = Text.unpack dbUser
                 , connectPassword = Text.unpack dbPassword
                 , connectDatabase = Text.unpack dbDatabase
                 }
```

[`ConnectInfo`](https://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo)
and
[`postgreSQLConnectionString`](https://hackage.haskell.org/package/postgresql-simple-0.6.2/docs/Database-PostgreSQL-Simple.html#v:postgreSQLConnectionString)
come from
[postgresql-simple](https://hackage.haskell.org/package/postgresql-simple) but
are re-exported by NejlaCommon to make it easier to create connection strings

## Defining the database schema

To store data in the database we first need to define its _schema_. We use
persistent-template's DSL (domain-specific language) to generate both the
relevant code on the haskell side as well as the SQL that updates the schema in
the database. For a complete documentation on persistent and the schema DSL refer to the [persistent chapter in the Yesod book](https://www.yesodweb.com/book/persistent).

`mkPersist` generates the Haskell types that corespond to the database entities, `mkMigrate` generates code that will update the database schema

Here's an example:

```haskell
share [ mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Group
    name Text
  User
    name Text
    group GroupId

   |]
```

The way this works is via [`Template
Haskell`](https://wiki.haskell.org/Template_Haskell), Haskell's "metaprogramming
facility": Sometimes weed need to write a lot of code that is repetitive, the
kind that we would rather pawn off to an intern: "I need 50 type class
instances, they all look like this, but I need you to fill in some blanks". In
fact, it's so predictable that we could write a program to generate the source
code for us and just paste it into a file. That would already work, but Haskell
makes things a little more convenient; we can give it the program generator
(written itself in Haskell) and GHC will run it and paste ("splice") the result
for us during compilation. The details are a little more involved (the
program-generator has to produce a certain intermediate representation instead
of source text), but luckily we can ignore the details because we already have
pre-made generators to use.

The second part of the puzzle arises because In order to use the generator we
need to tell it the database structure in Haskell code. But that would be heavy
in boilerplate and not very readable. Instead, we would much rather write it in
a special-purpose language (often called domain-specific language, or DSL for
short) and then translate that to Haskell. We could write a transpiler that
generates Haskell source from the DSL and then paste the result into a file,
(parser generators do exactly that). But again, Haskell makes things a little
more convenient by allowing us to write the DSL-code directly in our Haskell
source file, wrapped in a special syntactic form, called a quasi-quoter, that
also tells it the name of the translation function and it will translate it for
us during compilation. That's the meaning of the `[persistLowerCase| ... |]`
block.

instead of the `persistLowerCase` quasi-quoter we can also use `persistFileWith lowerCaseSettings` and have the schema definition in a separate file.


## Connecting to the Database

Next we need to connect to the database. We can use `withPostgresqlPool` from
`Database.Persist.Postgresql` to acquire a connection pool:

```haskell
runMain :: IO ()
runMain = runStderrLoggingT $ do
  conf <- loadConf "my-app"
  dbConString <- parseDatabaseConf conf
  runStderrLoggingT $ withPostgresqlPool dbConString 5 $ \pool -> do
     return () -- todo
```

The action is wrapped in runStderrLoggingT to satisfy the logging requirements
of the sub-commands.

Now we should ensure that the database has the correct schema. The simples way
to do that is to use `runMigration` with the migration we created in the
persistent-template block (`migrateAll` in this example), which will
automatically compare the schema we defined with the one the database is
currently in and apply necessary changes (it will refuse to apply destructive
changes like dropping columns and instead prompt we to perform them
manually). This will of course need to be run on an active connection to the
database - so far we have only created a pool of (potential) connections. We
can use `runPoolRetry` from `NejlaCommon` to grab one of them. It will keep
retrying until it can be established, this is also useful to ensure that the
database is available before starting the service.


```haskell
runMain :: IO ()
runMain = runStderrLoggingT $ do
  conf <- loadConf "my-app"
  dbConString <- parseDatabaseConf conf
  runStderrLoggingT $ withPostgresqlPool dbConString 5 $ \pool -> do
     _ <- runPoolRetry pool $ runMigration migrateAll
     return () -- todo
```

# Defining the API

To write a web service we need to define the API of our service. Check out the
[servant documentation](https://docs.servant.dev/en/stable/) for an in-depth
introduction and reference.

Let's define a simple api with two endpoints, one to store a new user and one that retrieves the list of users.

First the endpoint to retrieve users:

```haskell
type GetUsersApi = "users" :> Get '[ JSON ] [User]
```

Next we add an endpoint to add new users:

```haskell
type AddUserApi = "users"
                :> ReqBody '[ JSON ] User
                :> PostCreated '[ JSON ] NoContent
```

Finally we combine them into the complete API:

```haskell
type Api = GetUsersApi
         :<|> AddUserApi
```

# Implementing the handlers

## The App Monad
When we implement the handlers we will need access to both an ongoing database transaction and the application state. To that end, NejlaCommon provides a custom Monad, called `App`, that is essentially a `ReaderT` over `IO`. This [FPComplete blogpost](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) explains this pattern in more detail

The App monad also has two [phantom parameters](https://wiki.haskell.org/Phantom_type) (all that means is that they add information purely at the type level and there's no data of that type within it):

* The privilege level (either `Privileged` or `Unprivileged`). It tracks whether
  any sub-operation needs `Privileged` state. There's no strictly defined
  semantics for what `Privileged` means, but we can for example use it to ensure
  that an action that writes to the database can't be called from an action that
  is supposed to be read-only
* The Database Isolation level (`ReadCommitted`, `RepeatableRead` or
  `Serializable`). This ensures that we always run transactions at the strictest
  required isolation level of all the sub-actions. (See also [PostgreSQL documentation](https://www.postgresql.org/docs/current/transaction-iso.html))

Now, to use the App Monad, we should

### Define the application state

For now, we want the state to contain the (static) configuration, so:

```haskell
data AppState = AppState
    { appStateConfig :: AppConfig
    }
```

The App monad will carry this state around for us. The state is immutable, but
if you want to be able to update something you can include a mutable reference
(e.g. an `IORef`), e.g. say you wanted to remember the
number of requests served:

```haskell
data AppData = AppData
    { appStateConfig :: AppConfig
    , appStateRequests :: IORef Int
    }
```

Do keep in mind that requests will be served in parallel, so you'd have to use `atomicModifyIORef` or replace the IORef with a [`TMVar`](https://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM-TMVar.html#t:TMVar) or a [`TVar`](https://hackage.haskell.org/package/base-4.14.0.0/docs/GHC-Conc.html#t:TVar)

### Define your local Monad as a `type` synonym

We define a type synonym to keep the type short and easy to read. We fix the
privilege and isolation level for now, to keep it simple.

```haskell
type App = NC.App AppState 'NC.Privileged 'NC.ReadCommitted
```

*NB*: If you *eta-expand* the type synonym by specifying the return parameter as
in `type App a = NC.App <...> a` you won't ever be able to mention the `App`
type without it, e.g. if you want to implement a new class instance for
it. That's why we just partially apply `NC.App` instead.

### Define the run function

NejlaCommon provides a `runApp` function that runs an `App` action within a new
database transaction. It takes 4 parameters:

* The SQL-configuration
* The database connection pool
* The application state
* The action to run

```haskell
run :: MonadIO m => AppState -> ConnectionPool -> App a -> m a
run conf pool f = liftIO $ NC.runApp' def pool conf f
```

We don't need to modify the sql configuration for now, so we set it to `def`
from
[data-default](https://hackage.haskell.org/package/data-default-0.7.1.1/docs/Data-Default.html)

The type of the action is `m a` (where `m` is a MonadIO). This polymorphic type allows us to re-use the same `run` action in the test suite. For easier understanding, here is the specialised type we are using it with here:

```haskell
run :: AppState -> ConnectionPool -> App a -> Handler a
```


([`Handler`](https://hackage.haskell.org/package/servant-server-0.18/docs/Servant-Server.html#t:Handler) is the type of HTTP-handlers from servant-server)


We don't need to modify the sql configuration for now, pin the type parameters
and use `liftIO` to lift the resulting `IO` action into a servant-`Handler`.


## Writing the handler


### Getting users

Now we can implement a handler. Let's start with the one for the `GetUsersApi`.

```haskell
getUsersHandler :: ConnectionPool -> AppState -> Server GetUsersApi
getUsersHandler pool conf = do
  usrs <- run conf pool $ db' (P.selectList [] [])
  return $ P.entityVal <$> usrs
```

`Server` is a type-level function that computes the type of the function we need
to implement as a handler. You can ask ghci to give you the computed type like this:

```
> :kind! Server GetUsersApi
Server GetUsersApi :: *
= Handler [User]
```

This tells us that an implementation of "GetUserApi" is a `Handler`-action that produces a list of `User`s

We use our `run` function to run a database transaction. Within that transaction
we simply select all users and return them. However, `selectList` returns a list
of `Entity User` instead of `User`, so we map [`entityVal`](https://hackage.haskell.org/package/persistent-2.10.5.2/docs/Database-Persist-Types.html#v:entityVal) over the returned
list.

We should also note that persistent's database functions expect to run with direct access to a database transaction; their type ends in `ReaderT backend m a`. NejlaCommon provides `db` and `db'` functions as adaptors, they provide the open connection to the database functions, the difference is that `db` will create an `App`-action that is tagged with `Unprivileged` whereas `db'` creates `Privileged` actions. In our case we don't care about the distinction, so we use db'

### Adding new users

We still need to implement the handler to add a new user. We can check the required type:

```
> :kind! Server AddUserApi
Server AddUserApi :: *
= User -> Handler NoContent
```

So we need to accept a user and return NoContent

```haskell
addUserHandler :: ConnectionPool -> AppState -> Server AddUserApi
addUserHandler pool conf user = do
  run conf pool $ do
    _ <- db' $ P.insert user
    return ()
  return NoContent
```

Note the `user` parameter after `pool` and `conf` (these are extra parameters that are not part of the `Server AddUserApi`)

Again, we use `run` to start a new transactions, `db'`to run a database action within the current transaction. In this case we just add the user we've been handed, ignoring the returned database key.

### Putting it together

Now we have handlers for each of the endpoints, we still need to combine them
into a handler for the entire API:

```haskell
handler :: ConnectionPool -> AppState -> Server Api
handler pool conf = addUserHandler pool conf
                  :<|> getUsersHandler pool conf

```

The shape of the handler follows the shape of the API type. We just need to pass
around the application data to each handler.

Finally, we need to combine our API and handlers and hand them off to warp to create a webserver:

```haskell

-- Use @Proxy@ to be able to pass the API as an argument
api :: Proxy Api
api = Proxy

-- Specify port
warpSettings :: Int -> Warp.Settings
warpSettings port = Warp.setPort port Warp.defaultSettings

-- Build WAI Handler from servant API description
serveApp :: ConnectionPool -> AppState -> Application
serveApp pool s = serve api $ handler pool s

runMain :: IO ()
runMain = runStderrLoggingT $ do
  conf <- loadConf "my-app"
  dbConString <- parseDatabaseConf conf
  appConf <- parseAppConfig conf
  -- Make port configurable
  port <- getConf' "PORT" "port" (Right 80) conf
  withPostgresqlPool dbConString 5 $ \pool -> do
     _ <- NC.runPoolRetry pool $ runMigration migrateAll
     let appState = AppState { appStateConfig = appConf }
     liftIO $ Warp.runSettings
                (warpSettings port)
                (serveApp pool appState)
```

And that is our basic webserver done.

[Continue reading about how to build it](Buildsystem.md)

# Logging
## Logging of exceptions

`NejlaCommon.Helpers` provides `logOnException` as a plugin for
[warp](https://hackage.haskell.org/package/warp) to log all uncaught exceptions
that occur during request handling. The default behaviour is to just `print` the
Exception, which is unsatisfactory since Show instances for many Exceptions
dont' say what the type of the Exception is. Also, they are hard to parse.

`logOnException` expects as a first argument a function of type `LogRow -> IO
()` that tells it how to print the log row. You can use e.g. `withFileLogger` or
[`askLoggerIO`](https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#v:askLoggerIO)
together with `fromLogFun` to get appropriate functions.


An example:

```haskell
import qualified Network.Wai.Handler.Warp as Warp
import qualified Control.Monad.Logger     as Logger

import NejlaCommon

myApp = _

run = Logger.runStderrLoggingT $ do
  logFun <- askLoggerIO
  Warp.runSettings ( logOnException (fromLogFun logFun) $ Warp.defaultSettings ) myApp

```

The output is encoded as json (for easier analysis) and contains the following fields:

Fields:
  * 'exception': Type of the exception (as produced by 'typof')
  * 'description': String representation of the exception (as produced by show)
  * 'method': HTTP method of the request (or "N/A" if not available)
  * 'path': HTTP request path (or "server" if exception happened outside a request)
  * 'time': ISO 8601 formatted timestamp
  * 'event': Always "unhandled exception" (helps parsing the log message)
  * 'level': Always "ERROR"
  * 'source': Always "webserver"

Example (prettyfied):
```json
 {
  "event": "unhandled exception",
  "exception": "ErrorCall",
  "path": "/crash",
  "time": "2021-01-12T15:47:08.496182106Z",
  "method": "GET",
  "source": "webserver",
  "level": "ERROR",
  "description": "crash!"
 }
```

verbatim example:
```
[Error#webserver] {"event":"unhandled exception","exception":"ErrorCall","path":"/crash","time":"2021-01-12T15:47:08.496182106Z","method":"GET","source":"webserver","level":"ERROR","description":"crash!\nCallStack (from HasCallStack):\n  error, called at src/Handlers.hs:155:15 in test-server-0.0.0-6lviqPCMnxDJ4JGt2SmoWC:Handlers"}
```

The `[Error#webserver] ` prefix is added by monad-logger

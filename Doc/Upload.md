# Accepting file uploads in servant

To accept uploaded files in servant, use the
[servant-multipart](https://hackage.haskell.org/package/servant-multipart)
package. Unfortunately it is not on stackage yet, so you will have to add an
extra-dep to your stack.yaml:

```yaml
  extra-deps:
    - servant-multipart-0.11
```

Template / Example

Use `MultipartForm` to handle file uploads. It requires 2 parameters:
* A `tag` determining which backend to use (`Tmp` or `Mem`)
* The data type it returns (just use `MultipartData tag` unless you want to customize)

```haskell
import           Data.Directory
import qualified Servant.Multipart                  as Servant
import           Servant


type CreateVersionApi
   = Capture "slide show" SlideShowId
   :> "versions"
   -- This accepts multipart data (files) and stores them in a
   -- temporary directory:
   :> MultipartForm Tmp (MultipartData Tmp)
   :> Post '[ JSON] SlideShowVersion

serveCreateVersion :: Server CreateVersionApi
serveCreateVersion slideShow multipartData =
  forM_ (files multiPartData) $ \file -> do -- For each of the uploaded files
    -- Note that fdPayload gives us the path to the temporary file
    -- because we're using the Tmp backend (See Api type)
    liftIO $ copyFile (fdPayload file) $
        "/path/to/my/directory" ++ (fdFileName file)

```


The handler takes multipartData, which is a record with two fields:

```
MultipartData
    inputs :: [Input] -- form fields
    files :: [FileData tag] -- files
```

One for form fields and one for files.

```
Input
    iName :: Text -- name attribute of the input
    iValue :: Text -- value given for that input

FileData
    fdInputName :: Text -- name attribute of the corresponding HTML <input>
    fdFileName :: Text -- name of the file on the client's disk
    fdFileCType :: Text -- MIME type for the file
    -- Path to the temporary file (will be removed after the handler exits)
    -- or the actual data if the Memory storage option is selected
    fdPayload :: MultipartResult tag
```

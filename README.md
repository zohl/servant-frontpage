# servant-frontpage

## Description
  Serving html page from file with caching and detecting changes in the file.

  It's a simple wrapper that keeps the page in memory and updates it
  when necessary.
  
## Example
The following code
```haskell
type ServerAPI = ... :<|> Get '[HTML] BSL.ByteString

serveAPI :: ServerAPI
serveAPI = ... :<|> (return . BSL.readFile $ "../static/index.html")
```

can be rewritten like this:
```haskell
type ServerAPI = ... :<|> FrontPageEndpoint

frontPage <- mkFrontPage "../static/index.html"
let serveAPI = ... :<|> frontPage
```

## See also
- [Changelog](../master/CHANGELOG.md)

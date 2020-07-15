getRequestURL host apiKey resource id = host ++
                                        "/" ++ resource 
                                        ++ "/" ++ id
                                        ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id ->
                            getRequestURL host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)
genApiRequestResourceBuilder hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337haskell"
getBook = getRequestURL "http://example.com" "1337haskell" "book"


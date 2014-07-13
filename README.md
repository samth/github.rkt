### GitHub API Bindings for Racket

License: MIT

Example:

```racket
#lang racket
(require github)
(define c (new simple-client%))
(hash-ref (send c create-gist (hash "a-file" "some content")) 'html_url)
```

### GitHub API Bindings for Racket

License: MIT

Example:

```racket
#lang racket
(require octokit)
(define c (new octokit%))
(hash-ref (send c create-gist (hash "a-file" "some content")) 'html_url)
```

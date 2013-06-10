### GitHub API Bindings for Racket

License: MIT

Example:

```racket
#lang racket
(require octokit)
(define c (new client% [login "samth"] [password ""]))
(send c create-gist (hash "a-file" "some content"))
```

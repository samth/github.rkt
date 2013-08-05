### GitHub API Bindings for Racket

License: MIT

Example:

```racket
#lang racket
(require octokit)
(define c (new octokit%))
(hash-ref (send c create-gist (hash "a-file" "some content")) 'html_url)
```

Installation from git checkout:

    git clone git://github.com/samth/octokit.rkt
    raco pkg install --link -n octokit octokit.rkt

Recompilation of the package, once it's installed and you have edited
some of the sources:

    raco setup octokit

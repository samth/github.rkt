#lang racket/base

(require racket/class json net/url racket/port net/base64 racket/match racket/trait
         racket/format)

(provide client%)

(define ua-header "User-Agent: Racket github package; github.com/samth/github.rkt")

(define (http-authorization-header username password)
  (let ([username-bytes (string->bytes/utf-8 username)]
        [password-bytes (string->bytes/utf-8 password)])
    (string-append
     "Authorization: Basic "
     (bytes->string/utf-8
      (base64-encode
       (bytes-append username-bytes #":" password-bytes) #"")))))

(define (http-mixin %)
  (class %
   (inherit-field endpoint)
   (inherit mk-auth has-auth?)
    (super-new)
   (define/public (request method url [data 'null] #:auth [auth #f])
     (define auth-header 
       (case auth
         [(#f) #f]
         [(basic) (mk-auth #t)]
         [(maybe) (and (has-auth?) (mk-auth))]
         [(#t) (mk-auth)]))
     (define hs (filter values (list auth-header ua-header)))
     (define (simple f)
       (define result-port (f (string->url url) hs))
       (purify-port result-port) ;; currently ignored
       (bytes->jsexpr (port->bytes result-port)))
     (define (simple/post f #:data [data #""])
       (simple (λ (u hs) (f u data hs))))
     (match method
       ['get (define-values (p headers)
               (get-pure-port/headers 
                (string->url url) hs
                #:redirections 5))
             (bytes->jsexpr (port->bytes p))]
       ['post (simple/post #:data (jsexpr->bytes data))]
       ['patch (request 'post url data #:auth auth)] ;; net/url doesn't support PATCH
       ['head (simple head-pure-port)]
       ['delete (simple delete-pure-port)]
       ['put (simple/post put-pure-port)]))
   
   (define/public (post url data #:auth [auth #f])
     (request 'post (string-append endpoint url) data
              #:auth auth))
    (define/public (patch url data #:auth [auth #f])
     (request 'patch (string-append endpoint url) data
              #:auth auth))
    (define-syntax-rule (def-http method ...)
      (begin (define/public (method url #:auth [auth #f])
               (request 'method (string-append endpoint url) #:auth auth))
             ...))
   (def-http get put delete head)))

(define no-auth-trait
  (trait
   (define/public (mk-auth [x #f]) #f)
   (define/public (has-auth?) #f)))

(define auth-trait
  (trait
   (inherit-field password login oauth-token)
   (define/public (has-auth? [basic? #f])
     (cond [(and basic? password login) #t]
           [basic? #f]
           [(or oauth-token (and password login)) #t]
           [else #f]))
   (define/public (mk-auth [basic? #f])
      (cond [(and basic? password login)
             (http-authorization-header login password)]
            [basic?
             (error 'github "this method requires basic authentication")]
            [else
             (unless (or password oauth-token)
               (error 'post "no authentication method available"))
             (if oauth-token 
                 (format "Authorization: bearer ~a" oauth-token)
                 (http-authorization-header login password))]))))

;; for general github methods that require no authorization
(define gh-trait
  (trait
   (inherit get post)
   (define/public (meta) (get "/meta"))
   (define/public (rate-limit) (get "/rate_limit"))))

(define (->symbol v) (if (symbol? v) v (string->symbol (~a v))))

(define gist-trait
  (trait
   (inherit get post put delete)
   ;; #:user (or/c #f Username)
   (define/public (gists #:user [u #f])
     (match u
       [(? string?) (get (format "/users/~a/gists" u) #:auth 'maybe)]
       [#f          (get "/gists/public")]))
   (define/public (create-gist #:public [public? #t]
                               #:desc [desc 'null]
                               #:auth [auth 'maybe]
                               files)
     (post "/gists" #:auth auth
           (hash 'public public?
                 'description desc
                 'files (for/hash ([(name value) files])
                          (values (->symbol 
                                   (or name (object-name value)
                                       (error 'create-gist "file requires a name")))
                                  (hash 'content
                                        (match value
                                          [(? string?) value]
                                          [(? bytes?) (bytes->string/utf-8 value)]
                                          [(? input-port?) (port->string value)]
                                          [_ (~a value)])))))))
   ;; use #f as the contents to remove a file
   (define/public (edit-gist #:desc [desc 'null] files)
     (post "/gists" #:auth #t
           (hash 'description desc
                 'files (for/hash ([(name value) files])
                          (values (->symbol 
                                   (or name (object-name value)
                                       (error 'create-gist "file requires a name")))
                                  (hash 'content
                                        (match value
                                          [(? string?) value]
                                          [(? bytes?) (bytes->string/utf-8 value)]
                                          [(? input-port?) (port->string value)]
                                          [#f 'null]
                                          [_ (~a value)])))))))
   (define/public (fork-gist id)
     (post (format "/gists/~a/forks" id) #:auth #t))
   (define/public (star-gist id)
     (put (format "/gists/~a/star" id) #:auth #t))
   (define/public (gist-star? id)
     (get (format "/gists/~a/star" id) #:auth #t))
   (define/public (delete-gist id)
     (delete (format "/gists/~a" id) #:auth #t))))

;; for performing actions on behalf of a user
(define client-trait 
  (trait 
   (inherit get post)
   
   (define/public (get-token [options (hash)])
      (send this post "/authorizations" options #:basic #t))
    
    (define/public (authorizations [n #f])
      (if n
          (send this get (format "/authorizations/~a" n) #:auth 'basic)
          (send this get "/authorizations" #:auth 'basic)))
        
    (define/public (post-bug repo title [body 'null] [options (hash)])
      (send this post (format "/repos/~a/issues" repo)
            (hash-set* options 'title title 'body body)))))
 
(define github% 
  (class object% 
    (init-field [endpoint "https://api.github.com"])
    (super-new)))

(define client-state%
  (class github%    
    (init-field [login #f] [oauth-token #f] [password #f])
    (super-new)))

(define methods (trait->mixin (trait-sum gh-trait gist-trait)))
(define client-methods (trait->mixin (trait-sum gh-trait gist-trait client-trait)))

(define client% (client-methods (http-mixin ((trait->mixin auth-trait) client-state%))))
(define octokit%  (methods (http-mixin ((trait->mixin no-auth-trait) github%))))

(provide client% octokit%)
;(define me (new client% [login "samth"] #;[oauth-token "token"] [password ""]))

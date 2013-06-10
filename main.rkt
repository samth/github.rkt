#lang racket/base

(require racket/class json net/url racket/port net/base64 racket/match racket/trait)

(provide client%)

(define ua-header "User-Agent: Racket github package; github.com/samth/github.rkt")

(define (do-post token title body url)
  (define auth-header (format "Authorization: bearer ~a" token))
  (define json (jsexpr->bytes (hash 'title title 'body body)))
  (define result-port
    (post-impure-port
     (string->url url)
     json
     (list auth-header ua-header)))
  (define headers (purify-port result-port))
  (when #f
    (printf "Headers: \n~a" headers))
  (bytes->jsexpr (port->bytes result-port)))

(define (http-authorization-header username password)
  (let ([username-bytes (string->bytes/utf-8 username)]
        [password-bytes (string->bytes/utf-8 password)])
    (string-append
     "Authorization: Basic "
     (bytes->string/utf-8
      (base64-encode
       (bytes-append username-bytes #":" password-bytes) #"")))))

(define http-trait 
  (trait
   (inherit-field endpoint)
   (inherit mk-auth)
   (define/public (request method url [data 'null] #:auth [auth #f])
     (define auth-header (and auth (mk-auth (eq? auth 'basic))))
     (define hs (filter values (list auth-header ua-header)))
     (match method
       ['get (define-values (p headers)
               (get-pure-port/headers 
                (string->url url) hs
                #:redirections 5))
             (bytes->jsexpr (port->bytes p))]
       ['post (define result-port (post-impure-port (string->url url)
                                                    (jsexpr->bytes data)
                                                    hs))
              (purify-port result-port) ;; currently ignored
              (bytes->jsexpr (port->bytes result-port))]
       ['patch (request 'post url data #:auth auth)] ;; net/url doesn't support PATCH
       ['head (define result-port (head-impure-port (string->url url) hs))
              (purify-port result-port) ;; currently ignored
              (bytes->jsexpr (port->bytes result-port))]))
   
   (define/public (post url data #:auth [auth #f])
     (request 'post (string-append endpoint url) (jsexpr->bytes data)
              #:auth auth))
   
   (define/public (get url #:auth [auth #f])
     (request 'get (string-append endpoint url)
              #:auth auth))))

(define no-auth-trait
  (trait
   (define/public (mk-auth) #f)))

(define auth-trait
  (trait
   (define-values (password login oauth-token)
     (values (get-field password this) (get-field login this) (get-field oauth-token this)))
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

(define client%
  (class ((trait->mixin (trait-sum auth-trait http-trait)) object%)
    (inherit get post)
    (init-field [login #f] [oauth-token #f] [password #f]
                [endpoint "https://api.github.com"])
    (super-new)
    
    (define/public (meta) (get "/meta"))
    
    (define/public (get-token [options (hash)])
      (post "/authorizations" options #:basic #t))
    
    (define/public (authorizations [n #f])
      (if n
          (get (format "/authorizations/~a" n) #:basic #t)
          (get "/authorizations" #:basic #t)))
    
    
    
    
    
    (define/public (post-bug repo title [body 'null] [options (hash)])
      (post (format "/repos/~a/issues" repo)
            (hash-set* options 'title title 'body body)))))

(define me (new client% [login "samth"] [oauth-token "token"]))

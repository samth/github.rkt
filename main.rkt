#lang racket/base

(require racket/class json net/url racket/port net/base64 racket/match
         racket/format racket/trait net/head framework/preferences
         racket/file)

(provide client%)

(define-local-member-name login)
(define-local-member-name password)
(preferences:set-default 'github:oauth-token #f (λ _ #t))

(define ua-header
  "User-Agent: Racket 'github' package; github.com/samth/github.rkt")

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
    (define/public (request method url [data 'null]
                            #:auth [auth #f] #:json-result [json? #t]
                            #:headers [headers? #f])
      (define auth-header 
        (case auth
          [(#f) #f]
          [(basic) (mk-auth #t)]
          [(maybe) (and (has-auth?) (mk-auth))]
          [(#t) (mk-auth)]))
      (define hs (filter values (list auth-header ua-header)))
      (define (simple f)
        (define result-port (f (string->url url) hs))
        (define headers (purify-port result-port))
        (define result 
          ((if json? read-json port->bytes) result-port))
        (begin0 (if headers? (values result headers) result)
                (close-input-port result-port)))
      (define (simple/post f #:data [data #""])
        (simple (λ (u hs) (f u data hs))))
      (match method
        ['get (define-values (p headers)
                (get-pure-port/headers 
                 (string->url url) hs
                 #:redirections 5))
              (define result (read-json p))
              (begin0 (if headers? (values result headers) result)
                      (close-input-port p))]
        ['post (simple/post post-impure-port #:data (jsexpr->bytes data))]
        ;; net/url doesn't support PATCH so we use POST
        ['patch (simple/post post-impure-port #:data (jsexpr->bytes data))]
        ['head (simple head-impure-port)]
        ['delete (simple delete-impure-port)]
        ['put (simple/post put-impure-port)]))
    
    
    (define-syntax-rule (def-http/body method ...)
      (begin (define/public (method url data #:auth [auth #f]
                                    #:headers [headers #f]
                                    #:json-result [json? #t])
               (request 'method (string-append endpoint url) data
                        #:auth auth #:json-result json? #:headers headers))
             ...))
    (define-syntax-rule (def-http method ...)
      (begin (define/public (method url
                                    #:auth [auth #f]
                                    #:headers [headers #f]
                                    #:json-result [json? #t])
               (request 'method (string-append endpoint url)
                        #:auth auth #:json-result json? #:headers headers))
             ...))
    (def-http/body post patch)
    (def-http      get put delete head)
    
    (define/public (get/check-status url #:auth [auth #f])
      (define-values (b headers)
        (get url #:auth auth #:headers #t #:json-result #f))
      (define code (status-code headers))
      (match code
        [204 #t] [404 #f]
        [_ (error 'get/check-status "bad response code: ~a" code)]))))

(define (status-code h)
  (define s (extract-field "Status" h))
  (define r (and s (regexp-match #rx"^[0-9]+" s)))
  (and r (string->number (car r))))

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
                 (format "Authorization: token ~a" oauth-token)
                 (http-authorization-header login password))]))))

;; for general github methods that require no authorization
(define gh-trait
  (trait
   (inherit get post request)
   (define/public (meta) (get "/meta"))
   (define/public (markdown content #:mode [mode 'null] #:context [ctx 'null])
     ;; use `request` explicitly b/c this doesn't return JSON
     (post "/markdown" (hash 'text content 'mode mode 'context ctx)
           #:auth #f #:json-result #f))
   (define/public (rate-limit) (get "/rate_limit" #:auth 'maybe))))

(define (->symbol v) (if (symbol? v) v (string->symbol (~a v))))

(define gist-trait
  (trait
   (inherit get post put delete get/check-status)
   ;; (or/c #f Username)
   (define/public (gists [u #f])
     (match u
       [(? string?) (get (format "/users/~a/gists" u) #:auth 'maybe)]
       [#f          (get "/gists/public")]))
   (define/public (gist n)
     (get (format "/gist/~a" n) #:auth 'maybe))
   (define/public (gist-comment n [id #f])
     (if id 
         (get (format "/gist/~a/comments/~a" n id) #:auth 'maybe)
         (get (format "/gist/~a/comments" n) #:auth 'maybe)))
   (define/public (gist-add-comment n comment)
     (post (format "/gist/~a/comments" n) (hash 'body comment) #:auth #t))
   ;; #f to delete
   (define/public (gist-edit-comment n id comment)
     (if comment 
         (post (format "/gist/~a/comments/~a" n id) (hash 'body comment)
               #:auth #t)
         (delete (format "/gist/~a/comments/~a" n id) #:auth #t)))
   (define/public (create-gist #:public [public? #t]
                               #:desc [desc 'null]
                               #:auth [auth 'maybe]
                               files)
     (post "/gists" #:auth auth
           (hash 'public public?
                 'description desc
                 'files (for/hash ([(name value) files])
                          (values 
                           (->symbol 
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
                          (values
                           (->symbol 
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
     (get/check-status (format "/gists/~a/star" id) #:auth #t))
   (define/public (delete-gist id)
     (delete (format "/gists/~a" id) #:auth #t))))

(define collab-trait
  (trait
   ;; none of the query parameters are supported
   (inherit get put post delete patch get/check-status)
   (define/public (collaborators repo)
     (get (format "/repos/~a/collaborators" repo) #:auth 'maybe))
   (define/public (add-collaborator repo user)
     (put (format "/repos/~a/collaborators/~a" repo user) #:auth #t))
   (define/public (remove-collaborator repo user)
     (delete (format "/repos/~a/collaborators/~a" repo user) #:auth #t))
   (define/public (collaborator? repo user)
     (get/check-status (format "/repos/~a/collaborators/~a" repo user)
                       #:auth 'maybe))))

(define hook-trait
  (trait
   (inherit get put post delete patch get/check-status)
   (define/public (hooks repo)
     (get (format "/repos/~a/hooks" repo) #:auth #t))
   (define/public (add-hook repo hook config #:events [events (list "push")] #:active? [active #t])
     (post (format "/repos/~a/hooks" repo) #:auth #t
           (hash 'active active
                 'events events
                 'config config
                 'name hook)))
   (define/public (add-irc-hook repo server channel [nick "GitHubBot"]
                                #:events [events (list "push")] #:active? [active #t])
     (add-hook repo "irc" 
               (hash 'server server 'room channel 'nick nick)
               #:events events #:active? active))
   (define/public (add-email-hook repo address 
                                  #:send-from-author [author #f]
                                  #:events [events (list "push")] #:active? [active #t])
     (add-hook repo "email" 
               (hash 'address address 'send_from_author (if author "1" "0"))
               #:events events #:active? active))))

(define issues-trait
  (trait 
   ;; none of the query parameters are supported
   (inherit get put post delete patch get/check-status)
   (define/public (issues #:organization [org #f] #:repo [repo #f])
     (cond [repo (get (format "/repos/~a/issues" repo) #:auth 'maybe)]
           [org (get (format "/orgs/~a/issues" org) #:auth #t)]
           [else (get (format "/user/issues") #:auth #t)]))
   (define/public (issue repo n)
     (get (format "/repos/~a/issues/~a" repo n) #:auth 'maybe))
   (define/public (create-issue repo title [body 'null] [options (hash)])
     (post (format "/repos/~a/issues" repo)
           (hash-set* options 'title title 'body body)
           #:auth #t))
   (define/public (edit-issue repo n [title 'null] [body 'null] [opt (hash)])
     (patch (format "/repos/~a/issues/~a" repo n)
            (hash-set* opt 'title title 'body body)
            #:auth #t))
   ;; n is issue number or 'all
   (define/public (issue-comments repo n)
     (if (eq? n 'all)
         (get (format "/repos/~a/issues/comments" repo) #:auth 'maybe)
         (get (format "/repos/~a/issues/~a/comments" repo n) #:auth 'maybe)))
   (define/public (issue-comment repo n id)
     (get (format "/repos/~a/issues/~a/comments/~a" repo n id) #:auth 'maybe))
   (define/public (comment-issue repo n comment)
     (post (format "/repos/~a/issues/~a/comments" repo n)
           (hash 'body comment) #:auth #t))
   (define/public (edit-issue-comment repo n id comment)
     (patch (format "/repos/~a/issues/~a/comments/~a" repo n id)
            (hash 'body comment) #:auth #t))
   (define/public (delete-issue-comment repo n id)
     (delete (format "/repos/~a/issues/~a/comments/~a" repo n id) #:auth #t))
   (define/public (create-issue-label repo label [color "FFFFFF"])
     (post (format "/repos/~a/labels" repo) (hash 'name label 'color color)
           #:auth #t))
   (define/public (update-issue-label repo label [color "FFFFFF"])
     (patch (format "/repos/~a/labels/~a" repo label)
            (hash 'name label 'color color) #:auth #t))
   (define/public (delete-issue-label repo label)
     (delete (format "/repos/~a/labels/~a" repo label) #:auth #t))
   (define/public (issue-labels repo n)
     (get (format "/repos/~a/issues/~a/labels" repo n) #:auth 'maybe))
   (define/public (issue-label? repo n label)
     (get/check-status (format "/repos/~a/issues/~a/labels/~a" repo n label)
                       #:auth 'maybe))
   (define/public (add-issue-labels repo n labels)
     (post (format "/repos/~a/issues/~a/labels" repo n) labels #:auth #t))
   (define/public (remove-issue-label repo n label)
     (delete (format "/repos/~a/issues/~a/labels/~a" repo n label) #:auth #t))
   (define/public (set-issue-labels repo n labels)
     (put (format "/repos/~a/issues/~a/labels" repo n) labels #:auth #t))))

;; for performing actions on behalf of a user
(define client-trait 
  (trait 
   (inherit get post)
   (inherit-field oauth-token)
   
   (define/public (get-token [options (hash)])
      (post "/authorizations" options #:auth 'basic))
   
   (define/public (get+save-token [options (hash)])
     (define-values (r hs)
       (post "/authorizations" options #:auth 'basic #:headers #t))     
     (unless (= 201 (status-code hs))
       (error 'get+save-token "failed to retrieve token"))     
     (define token (hash-ref r 'token))
     (set! oauth-token token))
   
   (define/public (authorizations [n #f])
     (if n
         (get (format "/authorizations/~a" n) #:auth 'basic)
         (get "/authorizations" #:auth 'basic)))))
 
(define github% 
  (class object% 
    (init-field [endpoint "https://api.github.com"])
    (super-new)))

(define client-state%
  (class github%    
    (init-field [login #f] [oauth-token #f] [password #f])
    (super-new)    
    
    (define/public (load-token)
      (define p-token (preferences:get 'github:oauth-token))
      (if (string? p-token)
          (and (set! oauth-token p-token) #t)
          (let ()
            (define v (file->value (build-path (find-system-path 'home-dir) ".config" "racket_github_token")))
            (when (symbol? v) (set! v (symbol->string v)))
            (and (string? v) (set! oauth-token v) #t))))
    
    (define/public (delete-token)
      (set! oauth-token #f)
      (preferences:set 'github:oauth-token #f))
    
    (define/public (write-token)
      (if oauth-token
          (begin (preferences:set 'github:oauth-token oauth-token)
                 (with-output-to-file (build-path (find-system-path 'home-dir) ".config" "racket_github_token")
                   (λ ()
                     (write oauth-token))))
          (error 'write-token "no token available to write")))
    
    (define/public (token-available?)
      (or oauth-token (load-token)))
    
    (define/public (authorize #:scopes [scopes (list "gist" "public_repo")])
      (unless (token-available?)
        (unless (and login password)
          (error 'authorize
                 "login name and password required for authorization"))
        (send this get+save-token (hash 'scopes scopes)))
      (write-token))
    
    (define/public (reauthorize)      
      (delete-token)
      (authorize))))

(define (make-client callback)
  (define c (new client%))
  (cond [(send c load-token) (void)]
        [else (define-values (l p) (callback))
              (set-field! login c l)
              (set-field! password c p)
              (send c authorize)])
  c)

(define no-auth-traits (list gh-trait gist-trait issues-trait collab-trait hook-trait))
(define auth-traits (append (list client-trait) no-auth-traits))

(define methods (trait->mixin (apply trait-sum no-auth-traits)))
(define client-methods (trait->mixin (apply trait-sum auth-traits)))

;; for a real client
(define client%
  (client-methods (http-mixin ((trait->mixin auth-trait) client-state%))))
;; for un-authenticated use
(define simple-client%  (methods (http-mixin ((trait->mixin no-auth-trait) github%))))

(provide client% simple-client% make-client)

(define c (new client%))
(send c load-token)

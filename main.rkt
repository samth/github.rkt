#lang racket/base

(require racket/class json net/url racket/port net/base64 racket/match
         racket/format racket/trait net/head framework/preferences racket/serialize)

(provide client%
	 octokit%
	 make-client
	 forget-github-token!
	 forget-all-github-tokens!
	 (struct-out token-context))

;; Describes a key into the token store.
;;
;; The name is to be a human-readable string describing the token; the
;; scopes, a list of strings describing API scopes per the API
;; documentation.
;;
(struct token-context (name scopes) #:prefab)

(define-local-member-name login)
(define-local-member-name password)

(define PREFERENCE-NAME 'github:oauth-tokens)

(preferences:set-default PREFERENCE-NAME (hash) (λ _ #t))
(preferences:set-un/marshall PREFERENCE-NAME serialize deserialize)

(define (forget-all-github-tokens!)
  (preferences:set PREFERENCE-NAME (hash)))

(define (forget-github-token! context)
  (preferences:set PREFERENCE-NAME (hash-remove (preferences:get PREFERENCE-NAME) context)))

(define ua-header
  "User-Agent: Racket 'octokit' package; github.com/samth/octokit.rkt")

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
        (define headers (purify-port result-port)) ;; currently ignored
        (define result 
          ((if json? bytes->jsexpr values) (port->bytes result-port)))
        (if headers? (values result headers) result))
      (define (simple/post f #:data [data #""])
        (simple (λ (u hs) (f u data hs))))
      (match method
        ['get (define-values (p headers)
                (get-pure-port/headers 
                 (string->url url) hs
                 #:redirections 5))
              (define result (bytes->jsexpr (port->bytes p)))
              (if headers? (values result headers) result)]
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
                 (format "Authorization: bearer ~a" oauth-token)
                 (http-authorization-header login password))]))))

(define-syntax-rule (general-boolean-setter switch put delete url)
  (let ((u url))
    (if switch
	(put u #:auth #t)
	(delete u #:auth #t))))

(define-syntax-rule (general-api-predicate get url)
  ;; Different to get/check-status in that it treats ANY STATUS CODE
  ;; OTHER THAN 204 as #f, rather than throwing an exception on
  ;; anything other than 204 or 404.
  (let ()
    (define-values (b headers) (get url #:auth #t #:json-result #f #:headers #t))
    (= (status-code headers) 204)))

;; for general github methods that require no authorization
(define gh-trait
  (trait
   (inherit get post request)
   (define/public (meta) (get "/meta"))
   (define/public (markdown content #:mode [mode 'null] #:context [ctx 'null])
     ;; use `request` explicitly b/c this doesn't return JSON
     (post "/markdown" (hash 'text content 'mode mode 'context ctx)
           #:auth #f #:json-result #f))
   (define/public (rate-limit) (get "/rate_limit"))))

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

(define repos-trait
  (trait
   (inherit get put post delete)

   (define/public (user-repos [user #f])
     (get (if user
	      (format "/users/~a/repos" user)
	      (format "/user/repos"))
	  #:auth #t))

   (define/public (repo-info repo)
     (get (format "/repos/~a" repo) #:auth #t))

   (define/public (repo-contributors repo)
     (get (format "/repos/~a/contributors" repo) #:auth #t))

   (define/public (repo-languages repo)
     (get (format "/repos/~a/languages" repo) #:auth #t))

   (define/public (repo-teams repo)
     (get (format "/repos/~a/teams" repo) #:auth #t))

   (define/public (repo-tags repo)
     (get (format "/repos/~a/tags" repo) #:auth #t))

   (define/public (repo-branches repo)
     (get (format "/repos/~a/branches" repo) #:auth #t))

   (define/public (repo-branch-info repo branch)
     (get (format "/repos/~a/branches/~a" repo branch) #:auth #t))

   (define/public (create-repo! #:name name
				#:description [description ""]
				#:homepage [homepage ""]
				#:private [private #f]
				#:issues? [issues? #t]
				#:wiki? [wiki? #t]
				#:downloads? [downloads? #t]
				#:org [org-name #f]
				#:team [team-id #f]
				#:auto-init? [auto-init #f]
				#:gitignore-template [gitignore-template #f])
     (let* ((opts (hash 'name name
			'description description
			'homepage homepage
			'private private
			'has_issues issues?
			'has_wiki wiki?
			'has_downloads downloads?
			'auto_init auto-init))
	    (opts (if (and org-name team-id)
		      (hash-set opts 'team_id team-id)
		      opts))
	    (opts (if gitignore-template
		      (hash-set opts 'gitignore_template gitignore-template)
		      opts)))
       (post (if org-name
		 (format "/orgs/~a/repos" org-name)
		 "/user/repos")
	     opts
	     #:auth #t)))

   (define/public (delete-repo! repo)
     (delete (format "/repos/~a" repo) #:auth #t))

   ))

(define orgs-trait
  (trait
   (inherit get put delete)

   (define/public (user-orgs)
     (get (format "/user/orgs") #:auth #t))
   (define/public (org-members org)
     (get (format "/orgs/~a/members" org) #:auth #t))
   (define/public (org-public-members org)
     (get (format "/orgs/~a/public_members" org) #:auth #t))

   (define/public (org-member? org user)
     (general-api-predicate get (format "/orgs/~a/members/~a" org user)))
   (define/public (org-public-member? org user)
     (general-api-predicate get (format "/orgs/~a/public_members/~a" org user)))

   (define/public (org-delete-member! org user)
     (delete (format "/orgs/~a/members/~a" org user) #:auth #t))

   (define/public (set-org-membership-public! org user public?)
     (general-boolean-setter public? put delete (format "/orgs/~a/public_members/~a" org user)))

   (define/public (org-repos org)
     (get (format "/orgs/~a/repos" org) #:auth #t))

   (define/public (org-teams org)
     (get (format "/orgs/~a/teams" org) #:auth #t))))

(define teams-trait
  (trait
   (inherit get put patch post delete)

   (define/public (team-info id)
     (get (format "/teams/~a" id) #:auth #t))

   (define/public (team-members id)
     (get (format "/teams/~a/members" id) #:auth #t))

   (define/public (team-repos id)
     (get (format "/teams/~a/repos" id) #:auth #t))

   (define/public (create-team! org team-name repo-names permission)
     (post (format "/orgs/~a/teams" org) (hash 'name team-name ;; string
					       'repo_names repo-names ;; listof string
					       'permission permission) ;; string
	   #:auth #t))

   (define/public (update-team! team-id team-name permission)
     (patch (format "/teams/~a" team-id) (hash 'name team-name ;; string
					       'permission permission) ;; string
	   #:auth #t))

   (define/public (delete-team! id)
     (delete (format "teams/~a" id) #:auth #t))

   (define/public (team-member? id user)
     (general-api-predicate get (format "/teams/~a/members/~a" id user)))

   (define/public (set-team-membership! id user is-member?)
     (general-boolean-setter is-member? put delete (format "/teams/~a/members/~a" id user)))

   (define/public (team-manages-repo? team-id repo)
     (general-api-predicate get (format "/teams/~a/repos/~a" team-id repo)))

   (define/public (set-team-manages-repo! id name manages?)
     (general-boolean-setter manages? put delete (format "/teams/~a/repos/~a" id name)))))

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
   (inherit-field oauth-token-context oauth-token)
   
   (define/public (get-token [options (hash)])
      (post "/authorizations" options #:auth 'basic))
   
   (define/public (get+save-token c)
     (define options (hash 'note (token-context-name c)
			   'scopes (token-context-scopes c)))
     (define-values (r hs)
       (post "/authorizations" options #:auth 'basic #:headers #t))     
     (unless (= 201 (status-code hs))
       (error 'get+save-token "failed to retrieve token"))     
     (define token (hash-ref r 'token))
     (set! oauth-token-context c)
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
    (init-field [login #f] [oauth-token-context #f] [oauth-token #f] [password #f])
    (super-new)    
    
    (define/public (load-token c)
      (define p-token (hash-ref (preferences:get PREFERENCE-NAME) c #f))
      (if (string? p-token)
	  (begin (set! oauth-token-context c)
		 (set! oauth-token p-token)
		 #t)
          #f))
    
    (define/public (delete-token)
      (forget-github-token! oauth-token-context)
      (set! oauth-token-context #f)
      (set! oauth-token #f))
    
    (define/public (write-token)
      (when (not (and oauth-token-context oauth-token))
	(error 'write-token "no token or token context available to save"))
      (preferences:set PREFERENCE-NAME
		       (hash-set (preferences:get PREFERENCE-NAME)
				 oauth-token-context
				 oauth-token)))
    
    (define/public (token-available? c)
      (or (and (equal? oauth-token-context c) oauth-token)
	  (load-token c)))
    
    (define/public (authorize c)
      (unless (token-available? c)
        (unless (and login password)
          (error 'authorize
                 "login name and password required for authorization"))
        (send this get+save-token c))
      (write-token))
    
    (define/public (reauthorize)
      (define c oauth-token-context)
      (delete-token)
      (authorize c))))

(define (make-client context callback)
  (define c (new client%))
  (if (send c load-token context)
      c
      (match (callback)
	[#f #f]
	[(list l p)
	 (set-field! login c l)
	 (set-field! password c p)
	 (send c authorize context)
	 c])))

(define methods
  (trait->mixin
   (trait-sum gh-trait gist-trait issues-trait
	      collab-trait repos-trait orgs-trait teams-trait)))
(define client-methods
  (trait->mixin
   (trait-sum gh-trait gist-trait issues-trait
	      client-trait
	      collab-trait repos-trait orgs-trait teams-trait)))

;; for a real client
(define client%
  (client-methods (http-mixin ((trait->mixin auth-trait) client-state%))))
;; for un-authenticated use
(define octokit%  (methods (http-mixin ((trait->mixin no-auth-trait) github%))))

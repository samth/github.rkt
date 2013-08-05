#lang racket/base
(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         racket/pretty racket/runtime-path
         "main.rkt" "gui.rkt")
(provide tool@)

(define-runtime-path github-logo "GitHub-Mark-16px.png")

(define tool-token-context (token-context "DrRacket Github Tool"
					  (list "gist" "public_repo")))
 
(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
 
 
    (define gist-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
        (inherit register-toolbar-button)
 
        (let ((btn
               (new switchable-button%
                    (label "Post Gist")
                    (callback (λ (button)
                                (post-gist (get-definitions-text))))
                    [bitmap (read-bitmap github-logo)]
                    (parent (get-button-panel)))))
          (register-toolbar-button btn #:number 11)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))
 
    (define (post-gist text)
      (define name (let ([fname (send text get-filename)])
                     (cond [fname
                            (define-values (base n ?) (split-path fname))
                            n]
                           [else "unnamed.rkt"])))
      (define frame (send (send text get-tab) get-frame))
      (define client (gui-auth tool-token-context))
      (define result
        (send client create-gist (hash name (send text get-flattened-text))))
      (define url (hash-ref result 'html_url))
      (send frame set-status-text url))
 
    (define (phase1) (void))
    (define (phase2) (void))
    
    (drracket:get/extend:extend-unit-frame gist-mixin)))
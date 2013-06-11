#lang racket/base
(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         racket/pretty
         "main.rkt" "gui.rkt")
(provide tool@)
 
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
                    [bitmap (make-bitmap 1 1)]
                    (parent (get-button-panel)))))
          (register-toolbar-button btn #:number 11)
          (send (get-button-panel) change-children
                (λ (l)
                  (cons btn (remq btn l)))))))
 
    (define (post-gist text)
      (define name? (send text get-filename))
      (define name (or name "unnamed.rkt"))
      (define client (gui-auth))
      (define result (send client create-gist (hash name (send text get-flattened-text))))
      (printf ">>> result\n")
      (pretty-print result)
      (define url (hash-ref result 'http_url))
      (send text set-status-text url))
 
    (define (phase1) (void))
    (define (phase2) (void))
    
    (drracket:get/extend:extend-unit-frame gist-mixin)))
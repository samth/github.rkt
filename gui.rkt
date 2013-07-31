#lang racket/gui

(require "main.rkt" racket/class)

(define (system-position-ok-before-cancel?)
  (eq? (system-type) 'windows))

(define (ok-cancel mk-ok mk-cancel)
  (if (system-position-ok-before-cancel?)
      (values (mk-ok) (mk-cancel))
      (let ([c (mk-cancel)]
            [o (mk-ok)])
        (values o c))))

(define (get-username+password title 
                               [message "User Name"]
                               [password-message "Password"]
                               [parent #f]
                               [style null]
                               #:validate [validate (λ (x) #t)]
                               #:password-validate [p-validate (λ (x) #t)])
  (define f (make-object dialog% title parent 300))
  (define ok? #f)
  (define (done ?) (set! ok? ?) (send f show #f))
  (define username (new text-field% 
                        [label message]
                        [parent f]
                        [callback 
                         (λ (t e) 
                           (cond
                             [(eq? (send e get-event-type) 'text-field-enter)
                              (done #t)]
                             [else (do-validation username validate)]))]
                        [init-value ""]
                        [style (list* 'single 'vertical-label style)]))
  (define password (new text-field% 
                        [label password-message]
                        [parent f]
                        [callback
                         (λ (t e) 
                           (cond
                             [(eq? (send e get-event-type) 'text-field-enter)
                              (done #t)]
                             [else (do-validation password p-validate)]))]
                        [init-value ""]
                        [style
                         (list* 'password 'single 'vertical-label style)]))
  (define default-background (send username get-field-background))
  (define (do-validation field validate)
    (send field set-field-background 
          (if (validate (send field get-value))
              default-background
              (send the-color-database find-color "pink"))))
  (define p (make-object horizontal-pane% f))
  (send p set-alignment 'right 'center)
  (send f stretchable-height #f)
  (ok-cancel
   (lambda () 
     (make-object button% "Authenticate" p (λ (b e) (done #t)) '(border)))
   (lambda ()
     (make-object button% "Cancel" p (λ (b e) (done #f)))))
  (send (send username get-editor) select-all)
  (send username focus)
  (send f center)
  (send f show #t)
  (and ok? (list (send username get-value) (send password get-value))))

(define (gui-auth)
  (make-client (λ () (get-username+password 
                      "Enter your GitHub login"
                      "GitHub User Name"
                      "GitHub Password"))))

(provide gui-auth
	 get-username+password)

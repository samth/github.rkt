#lang racket/base

(require "main.rkt")

(provide cli-auth)

(define (cli-auth context
		  #:silent [silent #f]
		  #:discard-input [discard (not silent)])
  (make-client context (lambda () (prompt-for-credentials context silent discard))))

(define (discard-input!)
  (define buffer (make-bytes 65536))
  (let loop ()
    (define n (read-bytes-avail!* buffer))
    (when (not (or (eof-object? n) (equal? n 0)))
      (loop))))

(define (prompt-for-credentials context silent discard)
  (when discard
    (discard-input!))
  (when (not silent)
    (printf "======================================================================\n")
    (printf "LOGIN TO GITHUB\n")
    (printf "======================================================================\n")
    (printf "You are creating an authorization token for: ~a\n" (token-context-name context))
    (printf "You are granting the following permissions: ~v\n" (token-context-scopes context))
    (printf "\n")
    (printf "Enter your github username, or just press enter to abort: ")
    (flush-output))
  (define username (read-line (current-input-port) 'any))
  (cond
   [(or (eof-object? username) (equal? username "")) #f]
   [else
    (when (not silent)
      (printf "Enter your github password [WARNING: WILL SHOW ON SCREEN]: ")
      (flush-output))
    (define password (read-line (current-input-port) 'any))
    (list username password)]))

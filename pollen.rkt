#lang racket

(provide (all-defined-out)
         (all-from-out "lib/racket-doc-link.rkt"))

(require "lib/racket-doc-link.rkt")

(define (link href . elements)
  `(a ((href ,href)) ,@elements))

(define author "Lîm Tsú-thuàn")
(define author-mail "dannypsnl@gmail.com")

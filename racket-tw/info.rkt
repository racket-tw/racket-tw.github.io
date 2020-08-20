#lang info
(define collection "racket-tw")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define scribblings '(("src/index.scrbl" (multi-page))))
(define pkg-authors '(dannypsnl))

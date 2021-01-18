#lang scribble/manual

@title{副作用}

TODO

@codeblock{
(define a 1)

(define (foo x)
  (set! x 2)
  x)

(foo a)
a
}
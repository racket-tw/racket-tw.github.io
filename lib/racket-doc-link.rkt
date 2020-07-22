#lang racket

(provide rkt typed/rkt)

(require setup/xref
         scribble/xref
         racket/list
         racket/syntax)

(define xrefs (load-collections-xref))

(define (name->definition-tag modules)
  (let ([tag-cache (make-hash)])
    (lambda (name)
      (define identifier (format-symbol "~a" name))
      (hash-ref! tag-cache
                 identifier
                 (lambda _ (for/or ([module-path (in-list (modules))])
                             (xref-binding->definition-tag xrefs (list module-path identifier) #f)))))))

(define (racket-docs-link modules)
  (lambda (name)
    (define definition-tag ((name->definition-tag modules) name))
    (cond
      [definition-tag
        (define-values (path url-tag)
          (xref-tag->path+anchor xrefs
                                 definition-tag
                                 #:external-root-url "http://docs.racket-lang.org/"))
        `(a [[href ,(format "~a#~a" path url-tag)]
             [class "rkt-docs"]]
            (code ,name))]
      [else `(code ,name)])))

(define rkt (racket-docs-link (make-parameter '(racket txexpr pollen scribble/xref))))
(define typed/rkt (racket-docs-link (make-parameter '(typed/racket))))

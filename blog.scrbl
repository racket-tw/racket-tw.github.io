#lang scribble/manual
@(require racket/string
          racket/list
          racket/file
          racket/path
          racket/runtime-path)

@title[#:tag "blog"]{Blog}

@(define-runtime-path self ".")
@(define-runtime-path posts "post")

@(define post-files (reverse (find-files
                              (lambda (p) (path-has-extension? p #".scrbl"))
                              posts)))

@(define (date-from path)
   (define t (path-replace-suffix (find-relative-path (normalize-path posts) path) #""))
   (define s (string-split (path->string t) "-"))
   (string-join (take s 3) "/"))

@(define (title-from path)
   (define matches (regexp-match #rx"@title{[^}]+}" (open-input-file path)))
   (define s (bytes->string/utf-8 (car matches)))
   (define s2 (string-trim s "@title{"))
   (string-trim s2 "}"))

@(apply itemlist
        (for/list ([f post-files])
          @(item (hyperlink (path-replace-suffix (find-relative-path (normalize-path self) f) #".html")
                            (date-from f)
                            " "
                            (title-from f)))))

#lang scribble/manual
@(require (for-label racket))

@title{gui-easy}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

Recently, a new great racket GUI library calls @code{gui-easy-lib} is out. The most interesting part is it is a declarative framework, which more like SwiftUI or Flutter, makes development obviously easier. Today, I will use a few examples to show the core idea. First, we can get it with command:

@codeblock{
raco pkg install gui-easy-lib
}

Then have our first tried.

@codeblock{
(require racket/gui/easy)

(render
 (window
  (hpanel
   (text "hello"))))
}

The above code shows a @bold{hello} text. Except that, we have observable variable:

@codeblock|{
(require racket/gui/easy
         racket/gui/easy/operator)

(define @counter (@ 0))

(render
 (window
  (hpanel
   (button "-" (lambda () (@counter . <~ . sub1)))
   (text (@counter . ~> . number->string))
   (button "+" (lambda () (@counter . <~ . add1))))))
}|

In this example, @code|{@}| create a new observable with initial state. @code{<~} updates observable with its second argument. @code{~>} apply its second argument with current state. In this simple introduction, you can see a good idea is raising. I hope we will move GUI program on to this, and never go back.

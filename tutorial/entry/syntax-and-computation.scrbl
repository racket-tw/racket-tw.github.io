#lang scribble/manual
@(require (for-label racket)
          scribble/eval)

@title{語法與計算規則}

語法（syntax）和計算規則（computation rule）經常會被混為一談，但他們是不同的，例如一個 Forth 的函數呼叫可能寫成

@codeblock{
1 2 3 foo
}

Racket 寫成

@codeblock{
(foo 1 2 3)
}

但計算規則卻是一樣的：@code{1 2 3} 是 @code{foo} 的參數，結果是由 @code{foo} 的內容決定。於是我們大致可以理解計算規則跟語法為什麼不盡然有關聯了，其實還有很多細節，但對一個 Racket 的入門教學這樣暫時也就夠了。

@section{函數（function）}

函數呼叫（function call）大概是最普遍的計算規則，在各種語言裡面都可以看到，它也經常被稱為 application，因此函數也可以被稱為 applicable。在 Racket 裡，application 就是一個 list，第一個元素被當成函數（所以如果放入不是函數的東西會出現錯誤），剩下的被當成參數。到這裡，Racket 的計算規則暫時可以理解成被 quote 包含的會整塊被當成一個值，剩下的會被當成 application。

有了函數呼叫，自然有定義函數的方式，Racket 提供了 define form 來做這件事

@interaction[
(define (fib n)
  (case n
    [(0) 1]
    [(1) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
]

我們也可以把 @code{fib} 改寫成 define 的變數形式

@racketblock[
(define fib
  (lambda (n)
    (case n
      [(0) 1]
      [(1) 1]
      [else (+ (fib (- n 1)) (fib (- n 2)))])))
]

到這裡，我們知道 Racket 除了 quote 跟 application 的第三種規則：form。內建的 form 有很多，而我們可以用 @code{define-syntax} 等 form 定義更多 form，這就是為什麼 Racket 要讓資料跟程式具有同像性，因為如此一來處理程式就如處理資料一般容易。

接下來，我們來了解一些常見的 form

@section{conditional: if/cond/case/match}

@subsection{if}

@code{if} form 是各語言常見的語法，以下是一個 racket 案例

@codeblock{
(if (= x 1)
  'x-is-one
  'x-is-not-one)
}

我們可以看到 @code{if} 有三大元素：

@itemlist[
    @item{條件式（test-expr）}
    @item{條件式為非假 @code{(not #f)} 的結果（then-expr）}
    @item{條件式為假 @code{#f} 的結果（else-expr）}
]

也就是

@defform[(if test-expr then-expr else-expr)]

@subsection{cond}
@subsection{case}
@subsection{match}

@section{let/let*/letrec}
TODO
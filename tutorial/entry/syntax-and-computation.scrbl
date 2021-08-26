#lang scribble/manual
@(require (for-label racket)
          scribble/eval)

@(begin
   (define match-eval (make-base-eval))
   (interaction-eval #:eval match-eval (require racket/match)))

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

@section{變數與函數（variable and function）}

函數呼叫（function call）大概是最普遍的計算規則，在各種語言裡面都可以看到，它也經常被稱為 application，因此函數也可以被稱為 applicable。在 Racket 裡，application 就是一個 list，第一個元素被當成函數（所以如果放入不是函數的東西會出現錯誤），剩下的被當成參數。到這裡，Racket 的計算規則暫時可以理解成被 quote 包含的會整塊被當成一個值，剩下的會被當成 application。

有了函數呼叫，自然有定義函數的方式，Racket 提供了 @code{define} form 來做這件事

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

我們也可以把 @code{fib} 改寫成 @code{define} 的變數形式

@racketblock[
 (define fib
   (lambda (n)
     (case n
       [(0) 1]
       [(1) 1]
       [else (+ (fib (- n 1)) (fib (- n 2)))])))
 ]

到這裡，我們知道 Racket 除了 quote 跟 application 的第三種規則：form。內建的 form 有很多，而我們可以用 @code{define-syntax} 等 form 定義更多 form，這就是為什麼 Racket 要讓資料跟程式具有同像性，因為如此一來處理程式就如處理資料一般容易。

接下來，我們來了解更多常見的 form 吧！

@subsection{exercise}

下面是關於 @code{define} 的練習

@itemlist[
  @item{@codeblock{(define x 1 1)}？}
  @item{@codeblock{(define (id x) y)}？}
  ]

@section{conditional: if/cond/case/match}

@subsection{if}

@code{if} form 是各語言常見的語法，以下是一個案例

@codeblock{
 (if (= x 1)
   'x-is-one
   'x-is-not-one)
}

@defform[(if test-expr then-expr else-expr)]{
 我們可以看到 @code{if} 有三大元素：

 也就是

 @itemlist[
 @item{條件式（test-expr）}
 @item{條件式為非假 @code{(not #f)} 的結果式（then-expr）}
 @item{條件式為假 @code{#f} 的結果式（else-expr）}
 ]
}

唯一需要特別注意的是 racket 的 @code{if} 不能省略 else-expr（跟某些語言不一樣）。不過如果有這種需要，可以改用 @code{when} 或是 @code{unless} form。

@subsection{cond}

@defform/subs[(cond cond-clause ...)
              ([cond-clause    [test-expr then-body ...+]
                [else then-body ...+]
                [test-expr => proc-expr]
                [test-expr]])
              ]{

 @code{cond} form 可以看作是 @code{if} 的推廣。例子如下:

 @codeblock{
  (cond
    [(= x 1) 'x-is-one]
    [(= x 2) 'x-is-two]
    [(> x 2) 'x-is-more-than-two]
    [else    'something-else]) ; else 是可選的
 }

 它按順序測試任意數量的 test-expr (i.e. @code{(= x 1)}),每個 test-expr 又對應一個表達式 (e.g. @code{'x-is-one}),每組這樣的 sexp 稱為一個 cond-clause (e.g. @code{[(= x 1) 'x-is-one]})。最後一個 cond-clause 中 test-case 可換成 else 來匹配任意情況。

 改寫成 @code{if} form 的形式相當於

 @codeblock{
  (if (= x 1)
    'x-is-one
    (if (= x 2)
      'x-is-two
      (if (> x 2) ; 把 test-case 的 else 去掉的話,這行應該就是 (when (> x 2) 'x-is-more-than-two)
        'x-is-more-than-two
        'something-else)))
 }

 @specform[[test-expr then-body ...+]]{
  一般的 clause 會在 test-expr 為 @code{#true} 時執行
 }
 @specform[[else then-body ...+]]{
  else 是特殊的一個 clause，它表示預設的處理邏輯
 }
 @specform[[test-expr => proc-expr]]{
 }
 @specform[[test-expr]]{
 }

}

@subsection{case}

@defform/subs[(case val-expr case-clause ...)
              ([case-clause [(datum ...) then-body ...+]
                [else then-body ...+]])
              ]{
 @code{case} form 跟 C 語言的 switch case 語法很像,例子如下:

 @codeblock{
  (case x
    [(1)   'x-is-one]
    [(2 3) 'x-is-two-or-three]
    [else  'something-else]) ; else 是可選的
 }

 不妨把 x 位置的表達式稱為 target。方括號的 sexp 稱為 case-clause（e.g. @code{[(2 3) 'x-is-two-or-three]}）,case-clause 中左手邊的則是一個列表（e.g. @code{'(2 3)} 或者 @code{(list 2 3)}）,檢查 x 是否在列表中,返回對應的表達式（e.g. @code{'x-is-two-or-three}）,不考慮性能的情況下改寫成 @code{cond} form 的形式相當於

 @codeblock{
  (cond
    [(or (equal? x 1))              'x-is-one]
    [(or (equal? x 2) (equal? x 3)) 'x-is-two-or-three] ; Racket 會把這行優化成 O(log N)
    [else                           'something-else])
 }

}

@subsection{match}

@defform/subs[(match val-expr clause ...)
              ([clause    [pat body ...+]
                [pat (=> id) body ...+]
                [pat #:when cond-expr body ...+]])
              ]{
 @code{match} form 跟剛剛的 @code{case} form 長很十分像。只是 target 匹配的不是列表中的元素,而是 pattern。例子如下:

 pattern 可以是 constructor 和字面值的組合。
 @interaction[
 #:eval match-eval
 (match 3
   [1 'target-is-one]
   [2 'target-is-two]
   [3 'target-is-three])
 (match '(a b c)
   ['(a a a)     "target is a list of three a"]
   [(list a b)   "target is a list of a and b"]
   ['(a b c)     "target is a list of a, b and c"])
 (match '(a . b)
   [(list a b) 'list]
   [(cons a b) 'pair])]

 pattern 可以是 identifier 或 @code{_}。當 pattern 是 identifier 或 @code{_} 時,匹配任意值。
 @interaction[
 #:eval match-eval
 (define x 0)
 (define y 1)
 (match x
   [10 'x-is-ten]
   [_  'x-is-not-ten])
 (match y
   [x "pattern variable is not the same x as the one we defined at begining"]
   [1 "y is one"])
 (match 2
   [0 "zero"]
   [1 "one"]
   [x (format "mismatch with value ~s" x)])
 ]

 pattern 可以是 constructor 和 pattern 的組合(nested)。
 @interaction[
 #:eval match-eval
 (match '(a b)
   [(list 'a x) (format "the second element is ~s" x)]
   [(list  a b) "match but not reach"])
 (match '(a b)
   [(quote (a x)) "(list 'a 'b) != (list 'a 'x)"]
   [(quote (a b)) "(list 'a 'b) == (list 'a 'b)"])
 (match '(a (b (c d)))
   [(list 'a (list 'b res)) res])
 ]

 pattern 中 某個 sub-pattern 的右方插入 @code{...} 代表該 sub-pattern 可以有任意多個。
 @interaction[
 #:eval match-eval
 (match '(1 1 1)
   [(list 1 ...) 'ones]
   [_ 'other])
 (match '(1 1 2)
   [(list 1 ...) 'ones]
   [_ 'other])
 (match '(1 2 3 4)
   [(list 1 x ... 4) x])
 (match (list (list 'a 23 'b) (list 'c 22 'd))
   [(list (list x y z) ...) (apply + y)])
 ]

}

@section{let/let*/letrec}

@code{let} 是一種綁定語法，如下：

@codeblock{
  (let ([x 1]
        [y 2])
    (+ x y))
}

上面的語法等同於：

@codeblock{
  ((lambda (x y)
    (+ x y))
   1 2)
}

但注意這僅僅是計算上等價，如果牽涉到副作用、類型與 continuation 等更複雜的模型，上面的轉換並非永遠成立。而 @code{let*} 是 @code{let} 的擴充，表示後面的變數可以依賴前面的變數，如下語法：

@codeblock{
  (let* ([x 1]
         [y x])
    y)
}

可以視為

@codeblock{
  (let ([x 1])
    (let ([y x]))
      y)
}

最後則是 @code{letrec}，這次所有變數都可以互相依賴，因此可以寫出遞迴的定義。一般來說前面兩個都建議用 @code{define} 取代，而 @code{letrec} 並不行。

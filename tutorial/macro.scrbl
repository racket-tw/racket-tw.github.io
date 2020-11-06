#lang scribble/manual
@(require (for-label racket)
          scribble/eval
          racket/sandbox)

@title{Macro}

終於來到 macro，racket 的 macro 是給了一個編譯時期運行 racket 的環境，又因為 racket 是 s expression，所以可以直接操作 racket ast(@code{car}、@code{cdr} 等)，因此也稱 @italic{syntax transformers}。那麼趕緊來看怎麼使用吧！

@section{Pattern-Based Macros}

定義 macro 最簡單的方式就是使用 @code{define-syntax-rule}：

@specform[(define-syntax-rule pattern template)]

假設我們定義了一個 @code{swap} macro，它會交換兩個變數中儲存的值，那麼我們可以用 @code{define-syntax-rule} 定義它：

@racketblock[
(define-syntax-rule (swap x y)
  (let ([x1 x])
    (set! x y)
    (set! y x1)))
]

這個定義可以解釋為：
@itemlist[
@item{接受 @code{x}、@code{x} 兩個變數}
@item{回傳
@racketblock[
(let ([x1 x])
  (set! x y)
  (set! y x1))
]
這個 ast}
]

這裏可以看到為什麼要選 s expression(或是任意一種@bold{資料即程式}的表達方式了)，建構 ast(當然是一種資料)跟寫原始程式沒有任何差別。

@racketblock[
(let ([a 1]
      [b 2])
  (swap a b)
  (displayln (list a b)))
]

然而如果我們把 @code{a}、@code{b} 改成 @code{x1}、@code{b} 呢？直覺上我們會因為變數覆蓋而得到錯誤的結果：

@racketblock[
(let ([x1 1]
      [b 2])
  (let ([x1 x1])
    (set! x1 y)
    (set! y x1))
  (displayln (list x1 b)))
]

但 Racket 卻產生了正確的結果：

@def+int[
(define-syntax-rule (swap x y)
  (let ([x1 x])
    (set! x y)
    (set! y x1)))
(let ([tmp 1]
      [d 2])
  (swap tmp d)
  (displayln (list tmp d)))
]

這是因為 Racket 並不是單純的照搬程式碼進來而已，它會保證 @code{x}、@code{y} 不會跟內部定義的變數衝突(當然可以想見這實現起來有多麻煩)，這種不污染 macro 內的概念就叫做 @bold{hygienic macro}。

但用 @code{define-syntax-rule} 我們只能有一種形式，要怎麼做出像是 @code{define} 這樣有多種變化的 form 呢？這時候我們就需要 @code{define-syntax} 跟 @code{syntax-rules} 了！

@specform[
(define-syntax id
  (syntax-rules (literal-id ...)
    [pattern template]
    ...))
]

現在我們建立 @code{my-define}，單純包裝 @code{define}：

@racketblock[
(define-syntax my-define
  (syntax-rules ()
    [(my-define x e)
     (define x e)]
    [(my-define (x p ...) e)
     (define (x p ...) e)]))
]

雖然這很無聊但是這個例子只是要說明我們怎麼讓一個 form 對到不同模式，同時這裏也用到了 @code{...}，這個模式用來一次比對多個，例如我們寫：

@racketblock[
(my-define (add x y)
  (+ x y))
]

那 @code{x}、@code{y} 就會綁定到 @code{p} 變成 @code{'(x y)}，接著底下 macro body 的部分又會把 @code{p ...} 展開。

然而現在的做法還有一些問題，我們可以寫：

@racketblock[
(my-define 1 1)
(swap 'a 'b)
]

而這些程式是荒謬的，雖然這兩個例子裡面轉換後的 form 剛好能抓到問題，然而這一來未必會成立，二來錯誤訊息跟我們定義的 form 毫無關聯非常難讀。使用 @code{syntax-case} 可以解決這個問題：

@racketblock[
(define-syntax my-define
  (λ (stx)
    (syntax-case stx ()
      [(my-define x e)
       (unless (identifier? #'x)
         (error 'my-define "~a should be an identifier" #'x))
       #'(define x e)]
      [(my-define (x p ...) e)
       (unless (identifier? #'x)
         (error 'my-define "~a should be an identifier" #'x))
       #'(define (x p ...) e)])))
]

@code{syntax-case} 在這裡必須被包在 @code{λ} 底下，好接收 @code{stx} 參數。並且這裏需要用 @code{syntax} 明確地把回傳值包裝起來，這樣我們才能區分檢查的程式跟組合出來的結果。

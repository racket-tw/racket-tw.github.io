#lang scribble/manual
@(require (for-label racket))

@title{lazy compute}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

Lazy computation 是函數式裡常見的運算模型，Racket 預設並非使用它，那我們可以自己弄出來嗎？答案是可以的，Racket 提供了各種基礎的運算 form 供人負載，其中就有我們這次的目標 @code{#%app}(application) 和 @code{lambda}。首先我們把 @code{#%app} 蓋掉

@racketblock[
(require (rename-in racket
                    [#%app default-app]))

(define-syntax #%app
  (syntax-rules ()
    [(_ a) (default-app a)]
    [(_ a b) (default-app a b)]
    [(_ a b c ...)
     (#%app (#%app a b) c ...)]))
]

如此一來所有多參數的呼叫都變成單參數的呼叫，這時候你會發現原本的一些函數都不能用了。這是正常的，畢竟這些函數預設自身是 eager 模型。

轉換完 application，接著就是把一般的 lambda form 變成 lazy model 的 lambda form

@racketblock[
(require (rename-in racket
                    [lambda default-λ]))

(provide (rename-out [lambda- lambda]))

(define-syntax-parser lambda-
  [(_ () b ...)
   #'(default-λ () b ...)]
  [(_ (a:id) b ...)
   #'(default-λ (a) b ...)]
  [(_ (p p*:id ...) b ...)
    #'(default-λ (p)
        (lambda (p* ...)
          b ...))])
]

這裏我們把多引數變成單引數，如同把多參數變成單參數，現在我們可以測試一下效果

@codeblock{
> ((λ (a b c)
     a)
    1)
procedure
> ((λ (a b c)
     a)
   1 2)
procedure
> ((λ (a b c)
     a)
   1 2 3)
1
}

as expected，不過要是我們想讓原先的函數運作要怎麼辦？其實就是重包一層把 eager 變 lazy

@racketblock[
(provide (rename-out [lambda- lambda]
                     [cons- cons]))

(define cons-
    (lambda- (a b)
             (default-app cons a b)))
]

歡迎提出問題或是改進建議，我們有 @hyperlink["https://discord.gg/xpwzAcx"]{Discord channel}。到這裡我相信讀者也已經了解 macro 的奇妙運用可以帶來哪些樂趣，下次再見。

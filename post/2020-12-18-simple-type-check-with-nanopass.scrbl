#lang scribble/manual
@(require (for-label (except-in racket
                                extends)
                     nanopass/base))

@title{用 nanopass 做簡單的類型檢查}
@author[(author+email "Lîm Tsú-thuàn" "racket@racket.tw")]

nanopass 是一種編譯器實作的思想，旨在每個 pass 都只做簡單的最佳化，與傳統個位數個 pass 就做完全部事情相比。這種作法雖然需要遍歷程式更多次，總體而言卻比傳統的做法更有效率，並且遠比傳統做法更容易維護。這次則是以簡單運用 nanopass 作為起頭，以後應該還會繼續擴展這個系列下去 XD。

@section{nanopass 入門}

nanopass 除了是一種思想，也是一套框架(由 Andy Keep 實作與維護)，它提供了一個由數個 macro 形成的 DSL 作為開發的核心。@code{define-language} 是我們應該最先認識到的 macro：

@racketblock[
(define (var? x) (symbol? x))
(define (constant? x)
  (or
    (number? x)
    (string? x)
    (char? x)))

(define-language ST
  (entry Stmt)
  (terminals
   (var (x param))
   (constant (c)))
  (Stmt (stmt)
        ;; bind type
        (: x t)
        ;; define
        (:= x e))
  (Expr (e)
        x
        c
        (λ ([param* t*] ...) t e)
        (e e* ...))
  (Typ (t)
       x
       (-> t* ... t)))
]

首先在 @code{terminals} 裡面我們會寫出 @code{(predicate [meta-name* ...])}，@code{var?} 和 @code{constant?} 就是我們用到的 predicate。@code{entry} 指定了進入這個語言的預設語法是什麼，接下來則是 @code{(語法 (語法-meta-name* ...) 語法-clause* ...)} 的宣告。我一直提到 @code{meta-name}，這到底有什麼用呢？這是指我們的語法裡面，除了第一個開頭字符之外，都必須是一個 @code{meta-name}，如果不是 nanopass 會抱怨不存在這個 @code{meta}。這個寫法單純只是偷懶，以 @code{(:= x e)} 為例，其實也可以做成 @code{(:= (name : var) (e : Expr))} 來顯式的標明是哪一種語法(使用哪個 predicate)，不過既然都這樣了就試著習慣它吧！多寫幾種 @code{meta-name} 可以有效的改善語法的可讀性，另外 @code{meta-name} 之後加一個字符是可接受的，而且常常需要這麼做，因為一個語法底下很可能會重複的使用同一個語法。

比起用複雜的說明，還是直接看怎麼處理程式可以更快的理解我們到底做了什麼。nanopass 提供了 @code{define-parser} 讓我們可以產生相應語法的 parser，寫下 @code{(define-parser parse-ST ST)} 之後我們就得到了 @code{parse-ST} 和 @code{unparse-ST}，執行 @code{(parse-ST '(:= a 1))} 就會得到 @code{(ST:Stmt::= a 1)} 這個結構，而 @code{unparse-ST} 正好相反，僅此而已。到這邊 nanopass 已經介紹的差不多了，接下來來看怎麼利用 nanopass 寫 simple type check 吧！

@section{簡單類型檢查}

這裏說的簡單，是指僅有 base 和 arrow type(即 function type)，而我們的頂層語法僅有 type-binding(@code{(: x t)}) 和 value-binding(@code{(:= x e)})，所以我們的流程就是先綁定 type，再檢查 value 是否符合 type，如果沒有 type-binding 則採用 inferred-type 做綁定。為此我們需要一個良好的 environment 實作，這裏我提供一個

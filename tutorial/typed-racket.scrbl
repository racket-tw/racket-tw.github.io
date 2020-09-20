#lang scribble/manual

@title{typed/racket}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

雖說 Racket 之中語法並沒有很重要，但還是需要認識裡面常用的 dialect，這篇就是要介紹 typed/racket 這個 dialect。typed/racket 顧名思義就是標註了 type 的 racket，與 racket/base 的語法基本相通，但加入了一些型別標註(annotation)的語法以及給予某些變體幫助使用者寫出更簡潔的程式(雖然這還蠻看人的那些語法)。

@section{常見的型別}

我們可以先來觀察說到底有哪些常見 type 可以用：

@codeblock{
#lang typed/racket

; 值 : Type
"string" : String
#\a : Char
#t : Boolean [more precisely: True]
#f : False
'f : Symbol [more precisely: 'f]
0 : Integer [more precisely: Zero]
1 : Integer [more precisely: Positive-Byte]
}

可以看到一個奇特現象是 racket 的 type 經常有所謂的 more precisely 的標記，來說明存在更精確的型別存在，實際上怎麼做到的這裡不提，但這麼做的理由是為了支持所謂的 type refinement，可以讓 type checker 根據需要限縮型別。除此之外所有的值都可以是自己的 type: @code{(ann 1 1)} 是合法的標註。

@section{函數類型}

我們用 @code{->} 這個 type constructor 建構函數類型，@code{->} 在數學上的意思是蘊含，A->B 代表 A 蘊含 B，racket 裡照慣例用了前綴表達法 @code{(-> A B)}，只要 @code{A} @code{B} 都是類型,則 @code{(-> A B)} 是類型。
所以下列都是合法的類型：

@codeblock{
(-> Number Number Number)
(-> String String Boolean)
}

對應 @code{define}，typed/racket 提供了 @code{:} 作為宣告型別的語法：

@codeblock{
(: add (-> Number Number Number))
(define (add x y)
  (+ x y))
(: same (-> String String Boolean))
(define (same s1 s2)
  (eqv? s1 s2))
}

然而也提供了修改過的 @code{define}：

@codeblock{
(define (add [x : Number]
             [y : Number])
  : Number
  (+ x y))
(define (same [s1 : String]
              [s2 : String])
  : Boolean
  (eqv? s1 s2))
}

這些語法可以大部分的情況了，但 racket 本身允許可選參數的存在，在 typed/racket 中就對應了 @code{->*} 這個 type constructor：

@codeblock{
(: eval (->* (Term) (Env) Value))
(define (eval term [env '()])
  ;;; ignore
  )
}

p.s. 根據我目前所知，@code{->*} 必須明確的寫成 declare/define 分開的形式，寫成 @code{define} 內涵型別定義的 form 時 type checker 還是會覺得 optional argument 沒被填上導致 type mismatching。

而 racket 本來就支援 @code{case-lambda}(aka function overloading)，所以 typed/racket 也需要處理這個情況：

@codeblock{
(: append (All (a) (case->
                     [(Listof a) a -> (Listof a)]
                     [(Listof a) (Listof a) -> (Listof a)])))
;;; 直接定義
(define append2
  (case-lambda #:forall (a)
    [([l : (Listof a)]
      [x : a])
     (append l (list x))]
    [([l1 : (Listof a)]
      [l2 : (Listof a)])
     (append l1 l2)]))
}

@codeblock{case->} 只有在 @code{require/typed} 的時候能用，不然參數想同的情況下 typed/racket 會把 @code{x} 被推導成 @code{(U (Listof a) a)}。

有趣(麻煩)的最後一點是 keyword argument：

@codeblock{
(: position (->* (#:line Integer #:column Integer #:filename String) (#:msg String) Position))
(define (position #:line line #:column column #:filename filename #:msg [msg ""])
  ;;; ignore
  )
}

一般形式的 keyword argument 應該不是什麼大問題，但 optional keyword argument 要注意 pre-binding 不能把 keyword 自己綁進去，而是要把它的對應變數包進去。

@section{struct}

struct 必定會引入新的型別，並且使用 nominal subtyping，下面提供一個 @code{struct} 的簡單案例：

@codeblock{
(struct point
  ([x : Real]
   [y : Real]))
}

現在表達式 @code{(point 1 2)} 的型別就會是 @code{point}，@code{struct} 可以有 super type：

@codeblock{
(struct dog animal ())
}

這樣 @code{dog} 也可以是 @code{animal}，因為 @code{animal} 是 @code{dog} 的 super type。

@section{union type}

為了讓許多原先存在於 racket 的概念運作，也是為了更複雜的應用，typed/racket 提供了 union type，語法 @code{(U a b c)} 代表 這個型別可能是 @code{a} @code{b} 或 @code{c}：

@codeblock{
(let ([n 10])
  (if (even? n)
    'is-even
    'is-odd))
}

這個表達式的型別就是 @code{Symbol [more precisely: (U 'is-even 'is-odd)]} (值自己一定是自己的型別)

@section{recursive type}

type 之間互相參照就叫做 recursive type，在 typed/racket 裡頭可以用 @code{U} 與 @code{define-type} 來達成：

@codeblock{
(define-type BinaryTree (U Number (Pair BinaryTree BinaryTree)))

(define-type Tree (U leaf node))
(struct leaf ([val : Number]))
(struct node ([left : Tree] [right : Tree]))
}

當然我們不可以直接參照自己：

@codeblock{
(define-type A A)
(define-type B (U Number B))
; 以上都是 invalid type。
}

@section{polymorphism}

polymorphism 或是有些人只聽過 generic，我不打算分清楚他們的差別，以免讀者陷在裡面，這裏主要說的是參數多型，與 @code{struct} 那邊的 super type 不同，
現在先看一個簡單的範例：

@codeblock{
(define-type (Opt a) (U None (Some a)))
(struct None ())
(struct (a) Some ([v : a]))
}

這個型別可以用來表達可能有值也可能沒有值的情況。函數一樣可以接受不定型別作為參數：

@codeblock{
(: list-length (All (A) (-> (Listof A) Integer)))
(define (list-length lst)
  (if (null? lst)
    0
    (+ 1 (list-length (cdr lst)))))
}

@code{All} 對應邏輯裡面的 @code{∀} 符號，意思是對所有 @code{A} 都成立。

@section{inst/ann}

@subsection{ann}

@code{ann} 是 annotation 的縮寫，用來標記表達式 (expression) 的型別是什麼，總計有三種寫法：

@codeblock{
(let ([#{x : Number} 7]) x)
(ann x Number)
#:{x :: Number}
}

由於 typed/racket 有 precisely type，所以用 annotation 有時候是有需要的(笑)。

@subsection{inst}

而 @code{inst} 就更重要了，為了基於 racket 許多內建的 @code{case-lambda} 跟 polymorphism function 上，有時候會遇到 type checker 沒辦法推導出正確型別的情況，這時候就需要 @code{inst} 提供 type argument 實例化 type：

@codeblock{
;;; 這會撞到 Polymorphic function `foldl' could not be applied to arguments
(foldl cons null (list 1 2 3 4))
;;; 解法
(foldl (cons Integer Integer) null (list 1 2 3 4))
}

@section{interaction}

使用 typed/racket，如果函式庫作者沒有提供 type definition 難道就沒救了嗎？這就是最後一塊拼圖，typed/racket 允許使用者提供型別定義：

@codeblock{
(require/typed "point.rkt"
  [#:struct point ([x : Real] [y : Real])]
  [distance (-> point point Real)])
}

這樣一來使用者就不需要綁死在 racket 或是 typed/racket 上，而是能夠按需要選擇適合的語言了。

@section{總結}

希望這些可以讓讀者開始使用 typed/racket 增強需要型別檢查的部分，如果覺得有哪些資訊也應該放進這篇裡面可以寄信告訴我，如果有想了解的內容但還沒有相關教學可以 @link["https://github.com/racket-tw/racket-tw.github.io/issues/new" "開新 issue"]。

#lang pollen

◊(require pollen/unstable/pygments)

◊h1{typed/racket}

雖說 Racket 之中語法並沒有很重要，但還是需要認識裡面常用的 dialect，這篇就是要介紹 typed/racket 這個 dialect。
typed/racket 顧名思義就是標註了 type 的 racket，與 racket/base 的語法基本相通，但加入了一些型別標註(annotation)的語法以及給予某些變體幫助使用者寫出更簡潔的程式(雖然這還蠻看人的那些語法)。

◊h3{常見的型別}

我們可以先來觀察說到底有哪些常見 type 可以用：

◊highlight['racket]{
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

可以看到一個奇特現象是 racket 的 type 經常有所謂的 more precisely 的標記，來說明存在更精確的型別存在，實際上怎麼做到的這裡不提，但這麼做的理由是為了支持所謂的 type refinement，
可以讓 type checker 根據需要限縮型別。除此之外所有的值都可以是自己的 type: ◊highlight['racket]{(ann 1 1)} 是合法的標註。

◊h3{函數類型}

我們用 -> 這個 type constructor 建構函數類型，這在數學上的意思是蘊含，A->B 代表 A 蘊含 B，racket 裡照慣例用了前綴表達法 (-> A B)，只要 A B 都是類型,則 (-> A B) 是類型。
所以下列都是合法的類型：

◊highlight['racket]{
(-> Number Number Number)
(-> String String Boolean)
}

對應 define，typed/racket 提供了 : 作為宣告型別的語法：

◊highlight['racket]{
(: add (-> Number Number Number))
(define (add x y)
  (+ x y))
(: same (-> String String Boolean))
(define (same s1 s2)
  (eqv? s1 s2))
}

然而也提供了修改過的 define：

◊highlight['racket]{
(define (add [x : Number]
             [y : Number])
  : Number
  (+ x y))
(define (same [s1 : String]
              [s2 : String])
  : Boolean
  (eqv? s1 s2))
}

這些語法可以大部分的情況了，但 racket 本身允許可選參數的存在，在 typed/racket 中就對應了 ->* 這個 type constructor：

◊highlight['racket]{
(: eval (->* (Term) (Env) Value))
(define (eval term [env '()])
  ;;; ignore
  )
}

p.s. 根據我目前所知，->* 必須明確的寫成 declare/define 分開的形式，寫成 define 內涵型別定義的 form 時 type checker 還是會覺得 optional argument 沒被填上是 type mismatching。

有趣(麻煩)的最後一點是 keyword argument：

◊highlight['racket]{
(: position (->* (#:line Integer #:column Integer #:filename String) (#:msg String) Position))
(define (position #:line line #:column column #:filename filename #:msg [msg ""])
  ;;; ignore
  )
}

一般形式的 keyword argument 應該不是什麼大問題，但 optional keyword argument 要注意 pre-binding 不能把 keyword 自己綁進去，而是要把它的對應變數包進去。

◊h3{struct}

struct 必定會引入新的型別，並且使用 nominal subtyping，下面提供一個 struct 的簡單案例：

◊highlight['racket]{
(struct point
  ([x : Real]
   [y : Real]))
}

現在表達式 (point 1 2) 的型別就會是 point，struct 可以有 super type：

◊highlight['racket]{
(struct dog animal ())
}

這樣 dog 也可以是 animal，因為 animal 是 dog 的 super type。

◊h3{union type}

為了讓許多原先存在於 racket 的概念運作，也是為了更複雜的應用，typed/racket 提供了 union type，語法 (U a b c) 代表 這個型別可能是 a b 或 c：

◊highlight['racket]{
(let ([n 10])
  (if (even? n)
    'is-even
    'is-odd))
}

這個表達式的型別就是 Symbol [more precisely: (U 'is-even 'is-odd)] (值自己一定是自己的型別)
                                                                                        :w

◊h3{TODO recursive type}
◊h3{TODO polymorphism}
◊h3{TODO interact}

◊p{}

◊link["https://github.com/dannypsnl"]{◊author} 編輯

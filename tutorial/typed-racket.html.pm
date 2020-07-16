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

◊highlight['racket]{
(-> Number Number Number)
(-> String String Boolean)
}

◊p{}

◊link["https://github.com/dannypsnl"]{◊author} 編輯

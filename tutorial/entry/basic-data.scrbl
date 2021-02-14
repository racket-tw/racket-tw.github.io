#lang scribble/manual
@(require (for-label racket)
          scribble/eval)

@title{資料}

@section{Racket 特有的資料}

雖說是特有概念但其實很多概念是繼承自 Lisp/Scheme 的，包含

@itemlist[
    @item{Symbol}
    @item{Keyword}
    @item{Pairs and Lists}
    ]

@subsection{Symbol}

前面加了 @litchar{'} 的 identifier 就會生成 Symbol 的值（所以也可以說 symbol 就是 quoted identifier），例如：

@interaction[
'a
(symbol? 'a)
]

第二句 @code{(symbol? 'a)} 可以讀作：@code{'a} 是 symbol 嗎？

我們還可以詢問兩個 symbol 是否相等：

@interaction[
(eq? 'a 'a)
(eq? 'a 'abc)
(eq? 'a 'ABC)
]

還可以把字串轉換成 symbol：

@interaction[
(string->symbol "a")
]

除了以下字元跟空白符號之外的字元都是合法的 identifier 字元（合法在這裡指可以出現在 identifier 內）：

@litchar{(} @litchar{)} @litchar{[} @litchar{]} @litchar["{"] @litchar["}"] @litchar{"} @litchar{,} @litchar{'} @litchar{`} @litchar{;} @litchar{#} @litchar{|} @litchar{\}

除此之外

@itemlist[
    @item{@litchar{#} 只有在開頭不被允許，但 @litchar{#%} 則允許，所以 @litchar{#a} 不是 identifier 但 @litchar{#%a} 和 @litchar{a#} 是 identifier。}
    @item{@litchar{.} 本身不是 identifier}
    ]

而如果真的想要包含非法字元，可以用 @litchar{|} 包起來或是用 @litchar{\} 前綴逃脫，所以以下都是合法的 symbol

@itemlist[
    @item{@litchar{'\#a}}
    @item{@litchar{'|#a|}}
    @item{@litchar{'\.}}
    @item{@litchar["'|{|"]}
    ]

@subsection{Keyword}

Keyword 跟 Symbol 很像，只是前綴是 @litchar{#:}，例如：

@interaction[
'#:apple
(eq? '#:apple (string->keyword "apple"))
]

keyword 主要是用在命名參數（或是帶名參數）上，讀者暫時可以先不用管這是什麼意思，講到函數（function）時會更深入一點介紹命名參數是什麼。這裏先讓讀者看 @code{with-output-to-file} 怎麼用到 keyword 的：

@codeblock{
(with-output-to-file
  (build-path (current-directory) "stuff.txt")
  (lambda () (printf "example\n"))
  #:mode 'text
  #:exists 'replace)
}

@subsection{Pair 與 List}

Pair 與 List 是 Racket 中的程式、也是資料，這種對應使得我們有能力操作製作程式的程式，但現在我們不需要關心這個進階功能。首先我們看一些 Pair 的實例：

@interaction[
'(1 . 2)
'(a . b)
'(a b . c)
]

事實上 @code{'(a b . c)} 是 @code{'(a . (b . c))} 的簡寫。因此我們可以說 Pair 就是擁有兩個「內容」的結構，內容可以是任何資料。我們可以用 @code{car}（取得左邊）和 @code{cdr}（取得右邊）存取 Pair 的內容，因此：

@interaction[
(car '(a . b))
(cdr '(a . b))
(cdr '(a b . c))
]

List 是特殊的 Pair，當一個 Pair 的右側是空的 Pair @code{()} 時，該 Pair 就是一個 List，並且空 Pair 本身也是 List：

@interaction[
(list? '())
(cdr '(a))
(cddr '(a b))
]

有解構的方式，Pair 也有建構的方式，字面值語法 @code{(1 . 2)} 無法和函數呼叫（之後才會提到）語法區隔，因此我們得寫成 @code{'(1 . 2)}，但我們也可以改用其建構函數 @code{cons}：

@interaction[
(cons 1 2)
(cons 'a 'b)
(cons 'a (cons 'b 'c))
]

到此我們已經對 Pair 和 List 有足夠的了解，接下來是一些相對複雜的資料，只是留作參考，對學習的影響不大，讀者可以跳到計算規則的部分。

@section{更複雜的資料}

@itemlist[
    @item{Vectors}
    @item{Hash Tables}
    @item{Boxes}
    @item{Void and Undefined}
    ]

TODO

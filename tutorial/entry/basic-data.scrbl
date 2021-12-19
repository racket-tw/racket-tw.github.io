#lang scribble/manual
@(require (for-label racket)
          scribble/eval)

@title{資料（Data）}

@section{常見資料}

不管是哪一種程式都會進行計算，而計算就會得到結果，結果以人類可以理解的方式呈現。為此大多數語言都設立了一些常用的資料表達方式，@bold{Racket} 也不例外的支援了

@itemlist[
    @item{整數}
    @item{有理數}
    @item{浮點數（沒有實數，因為不可數集合沒辦法建構出來。同理所以接下來的複數也是有限制的複數）}
    @item{複數}
    @item{布林值（Boolean）}
    @item{字元}
    @item{字串（用來表達文字，但因為程式本身也是文字嘛，所以用 @bold{quote} 包起來避免混淆）}
    ]

下面我們分數字、布林值跟文字來討論。在 terminal 裡面直接輸入 @code{racket} 會出現一個可以輸入運算式（expression）的文字交互環境，我們一般叫它 REPL，下面的程式是以含輸入提示符號（@litchar{>}）排列的。啟動 REPL 的畫面大概會長得像下面那樣（我從印出來的文字裡面拿掉版本資訊，不過執行之後應該就看得出自己有沒有弄對了）。

@codeblock|{
$ racket
Welcome to Racket
>
}|

@subsection{數字}

@subsubsection{整數}

整數一如常見的理解，

@interaction[
1
1232131245
-99230193
]

都是整數

@subsubsection{有理數}

Racket 亦支援有理數如

@interaction[
3/4
32132/414551266
]

，而且它甚至會自動約分（笑）

@subsubsection{浮點數}

就像前面提過的，不可數集合沒辦法建構出來，所以用浮點數這種對於實數的近似值數值表現法來處理，寫成

@interaction[
2.4214
8.34155152
]

等，也有支援常用的特殊值的近似，如

@interaction[
(require racket/math)
pi
]

@subsubsection{複數}

除開實數是採用浮點數近似，複數跟在高中學習到的東西基本上是一樣的

@interaction[
5+2i
10+3.412i
]

@subsubsection{exercise}

現在讓我們實際使用數字看看

@itemlist[
  @item{50 分鐘是幾秒？}
  @item{8 公里是幾公尺？}
  ]

@subsection{布林值（Boolean）}

布林值也是現代人熟知的資料之一了，用於真假運算上，在 Racket 中寫法比較獨特

@interaction{
#t
#f
}

大寫不改變其意

@interaction{
#T
#F
}

@subsection{文字}

@subsubsection{字元}

字元用 @litchar{#\} 作為前綴，在後面接上想要的字元就是該字元

@interaction[
#\c
#\a
#\0
#\\
]

，而 @bold{Racket} 也支援一些控制字元（鍵盤上的特殊功能鍵）如

@interaction[
#\return
#\tab
]

@subsubsection{字串}

現在可以來說明為什麼需要 @bold{quote} 了，假設我們需要一串字叫 @code{hello}，我們的語言就寫

@codeblock{
hello
}

所以列印 @code{hello} 寫成

@codeblock{
(display hello)
}

可是變數也是

@codeblock{
hello
}

那我們怎麼知道

@codeblock{
(display hello)
}

的 @code{hello} 到底是什麼？乃至 @code{(display hello)} 到底是變數還是字串？所以實際上我們用 @litchar{"} 把字串包起來，寫成

@codeblock{
(display "hello")
}

但我們用了 @litchar{"} 之後，要怎麼在字串裡面表示 @litchar{"}？所以有所謂的轉義字符（escape character），例如 @code{"hello"} 就可以寫成

@codeblock{
"\"hello\""
}

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

然而最常見的作法還是用 @code{quote} 去建立 List，我們需要系統性的了解它

@itemlist[
  #:style 'ordered
  @item{Racket 裡面所有東西都是 list 跟 atom，Racket 會去執行它}
  @item{@code{quote}/@literal["'"] 就是叫 Racket 不要執行內容}
  @item{@code{quasiquote}/@literal["`"] 還是 @code{quote}，但預期可能會出現 @code{unquote}/@literal[","]，@code{unquote} 會讓 Racket 執行接續的 s-expression}
  @item{@code{unquote-splicing}/@literal[",@"] 是說，不止要執行，還要展開到上一層}
]

你已經對 Pair 和 List 有足夠的了解，練習看看吧

@subsubsection{exercise}

@itemlist[
  @item{@code{(1 2)}？}
  @item{@code{(+ 1)}？}
  @item{@code{(+)}？}
  @item{@code{(car 1)}？}
  @item{@code{(1 + 2)}？}
  @item{@code{'(1 2 3)}？}
  @item{@code{`(1 ,(add1 1) 3)}？}
  @item{@code{`(1 ,(list 2 3))}？}
  @item{@code["`(1 ,@(list 2 3))"]?}
  ]

@section{更複雜的資料}

這節是講述一些相對複雜的資料，只是留作參考，對學習的影響不大，讀者可以跳到計算規則的部分。

@itemlist[
    @item{Vectors}
    @item{Hash Tables}
    @item{Boxes}
    @item{Void and Undefined}
    ]

@subsection{Vector}

@subsection{Hash Table}

@subsection{Box}

@subsection{Void and Undefined}

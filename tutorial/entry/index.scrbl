#lang scribble/manual
@(require (for-label racket)
          scribble/eval)

@title{程式入門教學}
@author[(author+email "Lîm Tsú-thuàn" "racket@racket.tw")]

以下教學假設您已經成功安裝 Racket，並理解簡單的數學，擁有相當的生活常識。教學在第一次提到名詞時會中英並陳，之後則根據需要使用中或英，在翻譯會造成理解上的困擾時不翻。

@section{何謂程式（program）？}

程式就是一串說明如何執行運算的指令，那麼無可避面的我們會有進一步的問題：何謂運算？

@section{何謂運算（computation）？}

運算可以是各種各樣的東西，1+1 是運算嗎？是的，1+1 當然是一種運算，並且我們知道它的結果是 2。事實上，判斷也是一種運算，1+1 = 2 的等於生成真（true）或假（false）作為結果。根據長年來學習數學的經驗，我相信大多數人可以回答 2+3 = 5 為真。但為什麼 2+3=5 呢？因為我們約定好了 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 等「符號」組成的東西叫做「數字」，且說好了 + 這個符號應該做出什麼樣的「運算」。換句話說，雖然運算中間發生了什麼我們不關心，但我們可以通過約定好的「運算規則」知道其結果。

根據我們選定的抽象層面，我們也有可能可以關注到本來無法觀察的運算過程。舉個例子，方才我們描述的東西叫做自然數加法，皮亞諾公理（Peano axioms）可以幫助我們觀察某方面的真實。

皮亞諾公理規定了自然數可以由兩個方式得到

@itemlist[
    @item{0 是自然數}
    @item{如果 n 是自然數，則 succ(n) 是自然數(succ 也經常寫成 s 或 suc，是 successor 的縮寫)}
    ]

且

@itemlist[
    @item{succ n = succ(m) 當且僅當 n = m}
    @item{對任何 n 來說 succ(n) 都不可能為 0}
    ]

剩下的 axioms 是關於 equality 的

@itemlist[
    @item{reflexive}
    @item{symmetric}
    @item{transitive}
    @item{closed under equality}
    ]

上面的形式化裡面可以只讀懂前兩條就好，剩下的公理都是為了保證這跟我們直覺理解到的自然數真的是同一個東西而定義的，對接下來要說明的事物沒有關係。自然數常見的爭論是 0 算不算自然數，不過為了 0+n = n 的特性（稱為 additive identity），我們選擇了 0 是自然數的這邊。歸納公理被省略，不過你可以認為這條公理的重點是為了說明我們在討論合法的函數（對所有輸入皆有輸出，又稱 totality）。

於是我們可以開始討論為何這些允許我們更仔細的觀察加法，我們把加法定義為

@itemlist[
    @item{n + 0 = n}
    @item{n + succ(m) = succ(n + m)}
    ]

這是遞迴定義，但我們知道無論 m 為何，最終運算都會結束（因為 m 會歸零）。不過我們可以觀察到此運算的性質為「加法的結果是兩數中其中一個的減一加另一數再加一」或「兩數之一為零則結果為另一數」。

作為練習，你可以試著手動一步步展開 succ(succ(0)) + succ(succ(0)) 並檢查結果是否是 succ(succ(succ(succ(0))))。

這是個重要的觀察，重點不是加法的另一種定義方式，而是通過轉換觀點可以用各種角度觀察運算的不同「真實」。而這些取決於我們要站在什麼地方理解他們，因此我們也可以說「交易」是一種運算，由可以收錢跟可以付錢的客體完成，其重點是付款金額與收款金額應該相同，我們可以基於剛才的數字加減法完成金額的計算。或是選擇某金額的所有權由付款方移到收款方這樣的方式定義我們的運算。第二種方式看似有點多餘，但其實引入了我們可以在未來追蹤這筆交易的機會，種種計算方式往往沒有優劣之分。重點在衡量什麼是「合適」的層級，這往往需要知道我們要完成什麼以及我們有哪些限制需要遵守，並不僅僅只是運算問題。

現在讓我們面對抽象是什麼這個問題。

@section{何謂抽象（abstraction）？}

抽象是我們整天都在做的事情，例如我現在不擔心我現在使用的編輯軟體為什麼可以動，因為我的目的是要把文章寫出來（除非我的編輯軟體一直當掉）。而我們也不需要擔心為什麼超市買得到食物（除非買不到了）等等，因為我們注意力有限，因而一次只能注意少數事務來進行工作，把關心的部分拉出來的行為就是抽象。

@section{寫程式這份工作}

在說明運算時，我說問題有時不只是運算，這是因為程式涉及到解決問題，因此我們需要

@itemlist[
    @item{系統化地理解/闡述問題}
    @item{創意思考（像是在下雪的日子裡穿拖鞋騎腳踏車上山）}
    @item{正確的表達解法}
    ]

，偶爾我們也需要進行一些科學實驗：

@itemlist[
    @item{觀察複雜系統}
    @item{建立假說}
    @item{預測行為並驗證}
    ]

這些是我們設計程式的種種過程與不同面向，經常涉及到關於人的問題，例如老闆覺得重寫系統不夠好玩，怎麼不順便放點新功能呢？不幸的是即便現實世界無比複雜我們還是得找出方式解決問題，萬幸的是即便解決不了問題，我們通常也能通過解決人來收尾（誤）。

在瞎扯這麼多之後，我們可以來看程式到底應該有些什麼。

@section{程式}

很多人誤以為寫程式很難，或是「寫」程式非常重要，這些其實都是錯誤的觀念。不過事實上，「寫」程式是非常簡單的，只要我們不求這些程式可以解決我們關心的問題的話，例如下面這段程式（看不懂也沒有關係，之後會再做介紹）

@codeblock|{
#lang racket/base

(displayln "hello, world")
}|

隨便建立一個檔案，像是 @bold{a.rkt} 把程式貼進去，用 @bold{racket a.rkt} 執行就可以看到印出了 @bold{hello, world} 這串字。而接下來也是程式

@codeblock|{
#lang racket/base

(displayln "hello, world")
(displayln "hello, world")
(displayln "hello, world")
}|

事實上我們要重複幾行 @code{(displayln "hello, world")} 都無所謂，這些都是程式，只是沒用而已，所以真正困難的是解決問題而不是編寫程式。寫程式事實上也不重要，我們關心的是問題有沒有被解決，而寫程式只是其中一種辦法而已。然而程式具備的重要特性之一，並不只是可以執行，最重要的其實是可以被閱讀。一個方法可以通過被閱讀理解後傳承，只需要做修改就能解決新的問題，或是在這之上解決更複雜的問題，不需要再手動執行一次流程，這才是程式的價值所在。

既然程式可以被書寫閱讀（雖然是通過鍵盤與螢幕），那麼它就需要有符號、文法與語義，各種語言的細節常常在此愚弄初來乍到者，誤以為程式語言俱是龐然大物，然而，程式語言該有的核心其實亦常簡單：

@itemlist[
    @item{輸入輸出}
    @item{簡單數學運算}
    @item{條件執行}
    @item{重複}
    ]

輸入輸出是很容易理解的，無論如何最終程式都是為了人們製作的，所以會有各式各樣的輸入輸出裝置存在，例如鍵盤、滑鼠、VR 眼鏡、螢幕、開關等等。數學運算很不幸的沒有好的解釋，不過簡單數學在很多問題裡面都會用到，或許是個理解它為何存在的方向。條件執行是因為我們經常需要根據狀況做出不同的行為，例如掃地機器人撞到東西時會轉向等。重複執行的使用方式變化多端，不過大抵上來說是因為我們希望可以描述「一直做到完成」這類型的抽象，否則我們就得苦哈哈的坐在那邊不斷重新執行程式直到滿足某個條件為止。

現在你應該已經對程式能夠與不能夠完成的事情有了一點概念，也了解到運算跟抽象定義的威力，然而我們終究還是要學習一門實際的語言來進行寫程式這個行為，因此接下來轉入介紹 @bold{Racket} 這個程式語言。

@section{資料（Data）}

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

到此我們已經對 Pair 和 List 有足夠的了解，接下來是一些相對複雜的資料，只是留作參考，對學習的影響不大，讀者可以跳到計算規則的部分。

@section{更複雜的資料}

TODO

@itemlist[
    @item{Vectors}
    @item{Hash Tables}
    @item{Boxes}
    @item{Void and Undefined}
    ]

TODO

@section{語法與計算規則}
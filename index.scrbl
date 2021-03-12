#lang scribble/manual

@title{Racket Taiwan}
@author[(author+email "Lîm Tsú-thuàn" "racket@racket.tw")]

嗨，這裡是 racket.tw，目標是在台灣推廣 racket 這個大家應該不怎麼熟悉的程式語言。

@section{源頭}

racket 源於 PLT Scheme 這個專案，至今也仍然與 scheme 互相影響，某種程度上說他們是同一個語言也還行XD。

@section{教學}

目前的組織方式是分成從其他語言過來需要快速掌握開發方式的快速導覽路線跟完全的程式入門者教學路線，最後合流到進階學習給想了解各種無所謂的細節的學習者 :p。

@itemlist[
  @item{@hyperlink["./tutorial/entry/index.html"]{程式入門者}
        以還不會寫程式的讀者為目標}
  @item{@hyperlink["./tutorial/quick/index.html"]{快速導覽}
        已經會寫其他程式語言可以從這裡開始}
  @item{@hyperlink["./tutorial/advanced/index.html"]{進階學習者}
        想要進一步了解 racket 無所謂的細節的人可以看這裡 :p}
  ]

@subsection{其他學習資訊}

@subsubsection{書籍}

@itemlist[
  @item{@hyperlink["https://mitpress.mit.edu/sites/default/files/sicp/index.html"]{Structure and Interpretation of Computer Programs(SICP)}}
  @item{@hyperlink["https://mitpress.mit.edu/books/little-schemer-fourth-edition"]{The Little Schemer}}
  @item{@hyperlink["https://nostarch.com/realmofracket.htm"]{Realm of Racket}}
  @item{@hyperlink["https://nostarch.com/racket-programming-fun-way"]{Racket Programming the Fun Way}}
  ]

@subsubsection{線上練習環境}

@itemlist[
  @item{@hyperlink["https://exercism.io/tracks/racket"]{exercism: Racket}}
  ]

@section{Blog}

@itemlist[
  @item{@hyperlink["./post/2021-03-13-lazy-compute.html"]{2021/03/13 lazy compute}}
  @item{@hyperlink["./post/2020-12-26-frontend-to-nanopass.html"]{2020/12/26 從 frontend(compiler) 到 nanopass}}
  @item{@hyperlink["./post/2020-12-18-simple-type-check-with-nanopass.html"]{2020/12/18 用 nanopass 做簡單的類型檢查}}
  @item{@hyperlink["./post/2020-11-20-riposte.html"]{2020/11/20 Riposte}}
  @item{@hyperlink["./post/2020-11-15-raco-run.html"]{2020/11/15 raco-run}}
  @item{@hyperlink["./post/2020-10-22-pattern-matching-value.html"]{2020/10/22 Pattern matching value}}
  @item{@hyperlink["./post/2020-10-15-semilit.html"]{2020/10/15 semilit}}
  @item{@hyperlink["./post/2020-10-07-from-template.html"]{2020/10/07 from-template}}
  ]

@section{Q&A}

@subsection{如何安裝}

到 @hyperlink["https://download.racket-lang.org/"]{https://download.racket-lang.org/} 這個頁面可以下載官方發布的 Racket。作業系統的選擇不必多言，但 Distribution 是否選擇 Minimal Racket 則需要視情況而定，如果你需要包含 DrRacket 等內建開發軟體，則最好不要選擇 Minimal 的發行版(選擇這個方式之後也可以另外安裝 DrRacket，所以不須過多的擔心)，但如果只是在部署機器或是 CI 執行的話，那 Minimal Racket 便足夠了。最後則是 Variant 的選擇，BC 指 Before CS，而 CS 則指 Racket on Chez Scheme 的版本，CS 這個版本在 Racket 8.0 正式成為預設版本。不過遇到問題的話不仿也試試 BC，可以用 @hyperlink["https://nixos.org/download.html"]{nix} 寫 script 快速切換不同環境。

@subsection{開發環境}

編輯器推薦直接用 DrRacket 就好，但也有其他選擇如

@itemlist[
  @item{@hyperlink["https://marketplace.visualstudio.com/items?itemName=karyfoundation.racket"]{VSCode Plugin}}
  @item{@hyperlink["https://github.com/greghendershott/racket-mode"]{Emacs: racket-mode}}
  @item{@hyperlink["https://github.com/wlangstroth/vim-racket"]{Vim: vim-racket}}
  ]

但我認為整體來看還是 DrRacket 最為完整，也可以試試看 racket.tw 開發的 @hyperlink["https://github.com/racket-tw/sauron"]{sauron}。

@section{專案}

@itemlist[
  @item{@hyperlink["https://github.com/racket-tw/sauron"]{sauron}：A Racket IDE}
  @item{@hyperlink["https://github.com/racket-tw/reporter"]{reporter}：compiler 用的錯誤報告的 pretty printer/collector}
  @item{@hyperlink["https://github.com/racket-tw/cc"]{cc}：用 racket 寫的 c to x86/64 compiler}
  ]

@section{近期活動}

@itemlist[
  @item{@hyperlink["https://racketfest.com/"]{Racket Fest 2021}
        2021/03/26 CET 8:00 pm to 11:59 pm
        2021/03/27 CET 8:00 pm to 11:59 pm}
  ]

@section{其他資源}

@itemlist[
  @item{@hyperlink["https://racket-lang.org/"]{官網}}
  @item{@hyperlink["https://docs.racket-lang.org/"]{Racket 官方文件}
        這裡的搜尋也可以搜到第三方程式庫}
  @item{@hyperlink["https://pkgs.racket-lang.org/"]{Racket 程式庫}
        這是發布第三方程式庫的標準管道}
  @item{@hyperlink["https://try-racket.defn.io/"]{線上運行環境}}
  ]

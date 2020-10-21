#lang scribble/manual

@title{Racket Taiwan}
@author[(author+email "Lîm Tsú-thuàn" "racket@racket.tw")]

嗨，這裡是 racket.tw，我的目標就是在台灣推廣 racket 這個大家應該不怎麼熟悉的程式語言。
目前教學暫時會著重在寫給已經有程式經驗的讀者，對初學者就比較抱歉了QQ，畢竟目前只有我一個人弄。
但初學者可以讀 @(link "https://mitpress.mit.edu/sites/default/files/sicp/index.html" "Structure and Interpretation of Computer Programs(SICP)") 這本書
，或是 @(link "https://mitpress.mit.edu/books/little-schemer-fourth-edition" "The Little Schemer")，兩本都非常適合學習 scheme/racket 家族。

@section{源頭}

racket 源於 PLT Scheme 這個專案，至今也仍然與 scheme 互相影響，某種程度上說他們是同一個語言也還行XD。

@section{連結}

@subsection{教學連結}

@itemlist[
  @item{@link["/tutorial/quick-start.html" "Quick Start"]}
  @item{@(link "/tutorial/module.html" "Module")}
  @item{@(link "/tutorial/typed-racket.html" "typed/racket")}
  @item{預定：macro}
  @item{預定：collection}
  @item{預定：continuation}
  ]

@subsection{Blog}

@itemlist[
  @item{@link["/post/2020-10-19-semilit.html" "2020/10/19 semilit"]}
  @item{@link["/post/2020-10-10-from-template.html" "2020/10/10 from-template"]}
  ]

@subsection{其他}

@itemlist[
  @item{@link["https://try-racket.defn.io/" "線上運行環境"]}
  @item{@link["https://racket-lang.org/" "官網"]}
  @item{@link["https://docs.racket-lang.org/" "Racket Document"]}
  @item{@link["https://pkgs.racket-lang.org/" "Racket Packages"]}
  ]

@section{專案}

@itemlist[
  @item{@(link "https://github.com/racket-tw/sauron" "sauron")：A Racket IDE}
  @item{@(link "https://github.com/racket-tw/cc" "cc")：用 racket 寫的 c to x86/64 compiler}
  ]

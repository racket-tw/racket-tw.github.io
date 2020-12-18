#lang scribble/manual

@title{Racket Taiwan}
@author[(author+email "Lîm Tsú-thuàn" "racket@racket.tw")]

嗨，這裡是 racket.tw，我的目標就是在台灣推廣 racket 這個大家應該不怎麼熟悉的程式語言。

@section{源頭}

racket 源於 PLT Scheme 這個專案，至今也仍然與 scheme 互相影響，某種程度上說他們是同一個語言也還行XD。

@section{教學}

@link["/tutorial/index.html" "tutorial"]

@section{Blog}

@itemlist[
  @item{@link["/post/2020-12-18-simple-type-check-with-nanopass.html" "2020/12/18 用 nanopass 做簡單的類型檢查"]}
  @item{@link["/post/2020-11-20-riposte.html" "2020/11/20 Riposte"]}
  @item{@link["/post/2020-11-15-raco-run.html" "2020/11/15 raco-run"]}
  @item{@link["/post/2020-10-22-pattern-matching-value.html" "2020/10/22 Pattern matching value"]}
  @item{@link["/post/2020-10-15-semilit.html" "2020/10/15 semilit"]}
  @item{@link["/post/2020-10-07-from-template.html" "2020/10/07 from-template"]}
  ]

@section{Q&A}

@subsection{如何安裝}

到 @link["https://download.racket-lang.org/" "https://download.racket-lang.org/"] 這個頁面可以下載官方發布的 Racket。作業系統的選擇不必多言，但 Distribution 是否選擇 Minimal Racket 則需要視情況而定，如果你需要包含 DrRacket 等內建開發軟體，則最好不要選擇 Minimal 的發行版(選擇這個方式之後也可以另外安裝 DrRacket，所以不須過多的擔心)，但如果只是在部署機器或是 CI 執行的話，那 Minimal Racket 便足夠了。最後則是 Variant 的選擇，Regular 又稱 BC，指 Before CS，而 CS 則指 Racket on Chez Scheme 的版本，這個版本目前暫且還在開發當中，但 performance 優於 BC 版本不少，穩定度也尚可，可以進行一些早期試用了。仍要注意畢竟 CS 還在積極開發階段，遇到問題的話不仿也試試 BC，可以用 @link["https://nixos.org/download.html" "nix"] 寫 script 快速切換不同環境。

@section{專案}

@itemlist[
  @item{@(link "https://github.com/racket-tw/sauron" "sauron")：A Racket IDE}
  @item{@(link "https://github.com/racket-tw/cc" "cc")：用 racket 寫的 c to x86/64 compiler}
  ]

@section{其他資源}

@itemlist[
  @item{@link["https://try-racket.defn.io/" "線上運行環境"]}
  @item{@link["https://racket-lang.org/" "官網"]}
  @item{@link["https://docs.racket-lang.org/" "Racket Document"]}
  @item{@link["https://pkgs.racket-lang.org/" "Racket Packages"]}
  ]

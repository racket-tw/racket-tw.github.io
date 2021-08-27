#lang scribble/manual

@title[#:tag "Q&A"]{Q&A}

@section{如何安裝}

到 @hyperlink["https://download.racket-lang.org/"]{https://download.racket-lang.org/} 這個頁面可以下載官方發布的 Racket。作業系統的選擇不必多言，但 Distribution 是否選擇 Minimal Racket 則需要視情況而定，如果你需要包含 DrRacket 等內建開發軟體，則最好不要選擇 Minimal 的發行版(選擇這個方式之後也可以另外安裝 DrRacket，所以不須過多的擔心)，但如果只是在部署機器或是 CI 執行的話，那 Minimal Racket 便足夠了。最後則是 Variant 的選擇，BC 指 Before CS，而 CS 則指 Racket on Chez Scheme 的版本，CS 在 Racket 8.0 成為預設版本。不過遇到問題的話不仿也試試 BC，用 @hyperlink["https://nixos.org/download.html"]{nix} 可以寫出快速切換不同環境的 script。

@section{開發環境}

編輯器推薦直接用 DrRacket 就好，但也有其他選擇如

@itemlist[
  @item{@hyperlink["https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket"]{VSCode Plugin: Magic Racket}}
  @item{@hyperlink["https://github.com/greghendershott/racket-mode"]{Emacs: racket-mode}}
  @item{@hyperlink["https://github.com/wlangstroth/vim-racket"]{Vim: vim-racket}}
  ]

但我認為整體來看還是 DrRacket 最為完整，也可以試試看 racket.tw 開發的 @hyperlink["https://github.com/racket-tw/sauron"]{sauron}。

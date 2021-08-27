#lang scribble/manual

@title[#:tag "Q&A"]{Q&A}

@section{如何安裝}

到 @hyperlink["https://download.racket-lang.org/"]{https://download.racket-lang.org/} 這個頁面可以下載官方發布的 Racket。作業系統的選擇不必多言，但 Distribution 是否選擇 Minimal Racket 則需要視情況而定，如果你需要包含 DrRacket 等內建開發軟體，則最好不要選擇 Minimal 的發行版(選擇這個方式之後也可以另外安裝 DrRacket，所以不須過多的擔心)，但如果只是在部署機器或是 CI 執行的話，那 Minimal Racket 便足夠了。最後則是 Variant 的選擇，BC 指 Before CS，而 CS 則指 Racket on Chez Scheme 的版本，CS 在 Racket 8.0 成為預設版本。不過遇到問題的話不仿也試試 BC，用 @hyperlink["https://nixos.org/download.html"]{nix} 可以寫出快速切換不同環境的 script。

@section{開發環境}

編輯器推薦用 DrRacket 加上 sauron 以及 drcomplete 這兩個 plugin。或是使用 magic racket 加上 racket-langserver 的搭配（反正現在這兩個都是我們在維護）。

@itemlist[
  @item{@hyperlink["https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket"]{VSCode Plugin: Magic Racket}}
  @item{@hyperlink["https://github.com/greghendershott/racket-mode"]{Emacs: racket-mode}}
  @item{@hyperlink["https://github.com/wlangstroth/vim-racket"]{Vim: vim-racket}}
  @item{@hyperlink["https://github.com/racket-tw/sauron"]{sauron}}
  ]

目前整體來看 VSCode 的開發環境最完整，也可以遠端 pair programming（DrRacket 不能），但 magic racket 有點常 crash。

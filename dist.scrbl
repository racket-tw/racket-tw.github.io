#lang scribble/manual

@title{Racket Taiwan}
@author[(author+email "Lîm Tsú-thuàn" "racket@racket.tw")]

嗨，這裡是 racket.tw，目標是在台灣推廣 racket 這個大家應該不怎麼熟悉的程式語言。racket 源於 PLT Scheme 這個專案，至今也仍與 scheme 互相影響，某種程度上說他們是同一個語言也還行 XD。

@include-section["tutorial.scrbl"]
@include-section["blog.scrbl"]
@include-section["q-and-a.scrbl"]

@section[#:tag "project"]{專案}

@itemlist[
  @item{@hyperlink["https://github.com/racket-tw/sauron"]{sauron}：A Racket IDE}
  @item{@hyperlink["https://github.com/racket-tw/k"]{k}：一個實驗中的定理證明器}
  @item{@hyperlink["https://github.com/racket-tw/reporter"]{reporter}：compiler 用的錯誤報告的 pretty printer/collector}
  @item{@hyperlink["https://github.com/racket-tw/cc"]{cc}：用 racket 寫的 c to x86/64 compiler}
  ]

@section[#:tag "resource"]{其他資源}

@itemlist[
  @item{@hyperlink["https://racket-lang.org/"]{官網}}
  @item{@hyperlink["https://docs.racket-lang.org/"]{Racket 官方文件}
        這裡的搜尋也可以搜到第三方程式庫}
  @item{@hyperlink["https://pkgs.racket-lang.org/"]{Racket 程式庫}
        這是發布第三方程式庫的標準管道}
  @item{@hyperlink["https://try-racket.defn.io/"]{線上運行環境}}
  ]

@subsection[#:tag "online"]{線上題庫}

@itemlist[
  @item{@hyperlink["https://exercism.io/tracks/racket"]{exercism: Racket}}
  ]

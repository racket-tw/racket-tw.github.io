#lang scribble/manual
@(require (for-label racket))

@title{from-template}
@author[(author+email "Lîm Tsú-thuàn" "racket@racket.tw")]

from-template 是一個項目模板產生器，這裏簡單介紹它怎麼安裝及使用。

@section{安裝}

指令很簡單

@codeblock|{
raco pkg install from-template
}|

@section{使用}

例如我們想要產生一個 GUI 專案，那就打

@codeblock|{
raco new gui-app <project-name>
}|

或是一個 raco CLI extension 專案

@codeblock|{
raco new raco-command <project-name>
}|

全部的 template 可以在 @url["https://github.com/racket-templates"] 裡面找到

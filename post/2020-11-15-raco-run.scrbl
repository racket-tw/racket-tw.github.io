#lang scribble/manual
@(require (for-label racket))

@title{raco-run}
@author[(author+email "Lîm Tsú-thuàn" "racket@racket.tw")]

raco-run 是一個 raco 指令介面擴展，可以直接通過 raco 執行相應模組內的程式

@section{安裝}

指令

@codeblock|{
raco pkg install raco-run
}|

@section{使用}

這個指令需要模組與子模組名稱

@codeblock|{
raco run <module> <submodule>
}|

例如執行一個檔案(每個檔案都是一個模組)下的測試

@codeblock|{
raco run main.rkt test
}|

或是執行安裝的程式(以 sauron 為案例)：

@codeblock|{
raco run sauron main
}|

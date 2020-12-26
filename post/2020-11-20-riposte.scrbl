#lang scribble/manual
@(require (for-label racket))

@title{Riposte}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

Riposte 可以像 postman 一樣幫助我們建立可以反覆測試的 HTTP Request，但寫出來的腳本可以提交進 repository 的同時又不需要依賴 postman 這樣的操作介面。可以通過下面的指令安裝：

@codeblock|{
raco pkg install riposte
}|

使用時是建立副檔名為 @code{rip} 的程式

@codeblock|{
#lang riposte

^Content-Type := "application/json"
^Cache-Control := "no-cache"
%base := https://api.examplebookstore.com:8888/
%timeout := 30

$id := @ID with fallback "1234567890"
GET book/{id} responds with 2XX
}|

這是發送 @bold{GET} 的範例，我們也可以把一些常用的程式拆分出去：

@codeblock|{
;;; headers.rip
#lang riposte

^Content-Type := "application/json"
^Cache-Control := "no-cache"
}|

然後用 @code{import headers.rip} 引用。更多資訊可以直接去看 @link["Riposte 的文件" "https://docs.racket-lang.org/riposte/"]

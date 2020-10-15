#lang scribble/manual
@(require (for-label racket
                     semilit))

@title{semilit}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

semilit 是一個文學編程用的語言模組，可以通過以下指令安裝

@codeblock|{
raco pkg install from-template
}|

使用的時候，把第一行插入 @code{semilit}，像這樣

@codeblock|{
#lang semilit racket
}|

接著就只有開頭是 @code{>} 的程式會被執行了，因此可以寫出文學編程式的文章

@codeblock|{
#lang semilit racket

The following code would brbrbr...
> (define (id x) x)
}|

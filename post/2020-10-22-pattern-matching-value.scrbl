#lang scribble/manual
@(require (for-label racket))

@title{Pattern matching value}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

pattern matching 是 Lisp/ML 家族很常見也很好用的功能，但因為語法衝突，我們總是只能選擇以變數名稱做綁定或是取值比對兩種行為。舉例來說：

@racketblock[
(match (list 1 2 3)
  [{list 1 x y} (list x y)])
]

這段程式應該取得 @code{(list 2 3)}，這裏 @code{x} 跟 @code{y} 是模式綁定的語義。但對另一個案例來說這可能就不是那麼符合我們的需求了：

@racketblock[
(match (list string? boolean?)
  [{list boolean? x} (x 1)]
  [{list string? x} (x #t)])
]

這乍看之下會進入第二個分支，但其實不會，因為 @code{boolean?} 就只是變數綁定而已。編譯器不可能在語義一致的情況下幫你分析是哪個情況，而且也不應該引入不一致的語義，也就是不應該隨著上層是否有名稱綁定決定現在是取值或是綁定，這會造成非常微妙的後果：

@racketblock[
(let ([x 1]
      [y 3])
  (match (list 1 2 3)
    [{list 1 x y} (list x y)]))
]

假設編譯器根據上下文知道有 @code{x = 1} 且 @code{y = 3} 進而決定這個模式其實是比對 @code{{list 1 1 3}}，結果就會是沒有進這個分支。而這種行為非常麻煩，因為修改外層隨時都有可能讓模式比對出錯，並且不容易檢查到，因此腦子還正常的人都會選擇一致性。但難道我們就沒有任何方式可以比對被放在變數後的值了嗎？畢竟我們不可能去重複函數體來比對函數，也不可能引入不一致語義自爆。其實還是有辦法的，其中一種是 @code{#:when} 子句，這個子句在 @code{match} 中可以存在。

@racketblock[
(match (list string? boolean?)
  [{list y? x} #:when (eq? y? string?)
   (x 1)])
]

但這在這個情況下不是很好用，另一個方式是@bold{相等模式}。

@racketblock[
(match (list string? boolean?)
  [{list (== string?) x} (x 1)])
]

這樣就可以直接比對變數後的值了，這結論也回到一開始我提到的語法問題，簡而言之就是變數綁定模式跟變數取值模式需要用語法區隔其行為。

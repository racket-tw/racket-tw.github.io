#lang scribble/manual
@(require (for-label (except-in racket
                                extends)
                     nanopass/base))

@title{從 frontend(compiler) 到 nanopass}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

採用 nanopass 內建的 parser 最大的問題就是只能處理內建的 s-expression（用 @code{quote} 包住的那些表達式），然而這樣一來就失去了在錯誤發生時回報精確位置的能力，是我們不樂見的情況。為了解決這個問題，我們需要手寫把 frontend 的節點翻譯到 nanopass 的 language 部分。首先我們定義一個簡單的 language。

@codeblock|{
(require nanopass/base)

(define-language L0
  (terminals
   (syntax (stx))
   (identifier (name param))
   (stx-number (num)))
  (Expr (expr)
        num
        name
        ; let binding
        (let stx name expr) => (let name expr)
        ; abstraction
        (λ stx (param* ...) expr) => (λ (param* ...) expr)
        ; application
        (stx expr expr* ...) => (expr expr* ...)))
}|

@code{stx-number} 顯然是個新酷玩意兒，沒錯，它的定義是：

@codeblock|{
(define (stx-number? stx)
  (if (syntax? stx)
      (number? (syntax-e stx))
      #f))
}|

因為我們需要儲存任意表達式的位置，最簡單的方式就是儲存原始的 @code{syntax} 物件，我們在每一層物件裡面都確保了它儲存了最上層的原始物件。@code{stx-number?} 跟 @code{identifier?} 都首先是一個 @code{syntax?}，然後才檢查其內容，因此一定有做到儲存原始最上層物件的結果。至於 @bold{let}、@bold{abstraction}、@bold{application} 都自行持有 @code{stx} 欄位來達成這點。然而如果每次我們都列印原始的物件，這一定很煩人，所以用 @code{=>} 簡化輸出結果。

現在我們可以專注在最後的核心：@code{parse}，這個 pass 把原始 @code{syntax} 物件轉換成 nanopass 的結構以利於後續採用 nanopass 的方式開發。

@codeblock|{
(require syntax/parse)

(define-pass parse : * (stx) -> L0 ()
  (Expr : * (stx) -> Expr (expr)
        (syntax-parse stx
          #:literals (let λ)
          ; let form
          [(let name:id expr)
           `(let ,stx ,#'name ,(parse #'expr))]
          ; lambda form
          [(λ (param*:id ...) expr)
           `(λ ,stx (,(syntax->list #'(param* ...)) ...) ,(parse #'expr))]
          [(f arg* ...)
           `(,stx ,(parse #'f) ,(map parse (syntax->list #'(arg* ...))) ...)]
          ; literal expression
          [x #:when (ormap (λ (pred?) (pred? stx)) (list identifier? stx-number?))
             #'x]
          [else (error 'syntax "unknown form: ~a" stx)]))
  (Expr stx))
}|

這個 @code{define-pass} 接收了不知道是什麼的輸入 @code{*}，並輸出到 @code{L0}。內部只定義了 @code{Expr} 函數，把 @code{* (stx)} 翻譯成 @code{Expr (expr)}。主要的展開可以分成

@itemlist[
  @item{let form}
  @item{lambda form}
  @item{application form}
  @item{literal}
  @item{error}
]

我們可以簡單解釋 literal 跟 error，當展開的內容是 @code{identifier?} 或 @code{stx-number?} 其中之一，我們只需要直接回傳，因為這兩者都是和法的 @code{L0 Expr}。當其他語法處理部分都沒有捕捉成功，就報告語法錯誤。

let 是剩下來裡面最好讀懂的東西，它幾乎直接對照著翻譯到 @code{L0 Expr}，只是多給原始的 @code{syntax} 物件，並且需要再展開 @code{expr} 才傳給 @code{L0 Expr}。

在 lambda 還有 application 裡面我們都需要 @code{(syntax->list #'(x* ...))} 的部分，這是為了把一個 @code{syntax} 最外層的 @code{list} 解開變成一個含有多個 @code{syntax} 的 @code{list}。而接下來 @code{arg*} 需要全部拿去再 @code{parse} 一遍而 @code{param*} 只需要直接傳（已經是我們想要的內容：@code{identifier?}）就好了。接著我們來測試成果。

@codeblock|{
(parse #'(let a 1))
(parse #'(let id (λ (x) x)))
(parse #'(let a-id (id a)))
(parse #'(let no-arg (foo)))
}|

這些產出雖然看起來十分複雜，但在使用 nanopass 時卻不需要關心這些細節，只需要用 meta name 就可以了。歡迎提出問題或是改進建議，我們有 @link["https://discord.gg/xpwzAcx" "Discord channel"]，這篇的程式也可以在@link["https://github.com/racket-tw/compiler-frontend-sample" "這個專案"]裡面找到。到這裡我相信讀者也已經了解怎麼把各種輸入傳進 nanopass 並美化輸出避免干擾的技巧，剩下的只有多加練習掌握而已，下次再見。

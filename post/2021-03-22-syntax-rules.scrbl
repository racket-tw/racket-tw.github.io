#lang scribble/manual
@(require (for-label racket))

@title{[Scheme] define-syntax 佮 syntax-rules 的使用例}
@author[(author+email "Yoxem" "yoxem.tem98@nctu.edu.tw")]

佇 Scheme 中，巨集 (macro) 的使用是一个特點，巨集佮函數真成，毋過會當處理 syntax （句法）做輸入的 argumemt（引數）。巨集的定義干涉著 syntax。佇伊的囝語言 Racket 中閣有 syntax-case 等語法，提來定義 syntax 用的。

若定義巨集，用 syntax-rules 通定義巨集當處理幾个 argument 的時愛按怎執行。比論以下的例：

@codeblock{
;; #lang r5rs -> scheme R5Rs
;; scheme　系 (define-syntax) (syntax-rule) 的用法舉隅（有關巨集 macro）

;;; 求算平方和，sqrt-num 是巨集名稱
;; 期望輸出結果
; (sqrt-sum 2) => 4 （回傳值，= 2^2）
; (sqrt-sum 1 3) => 10 (=1^2+3^2)
; (sqrt-sum 1 3 5) => 35 (=1^2+3^2+5^2)
(define-syntax sqrt-sum
  (syntax-rules ()
    ((_ x) ; 一元引數時。底線_代表 sqrt-num 這個函數，當然也可以直接寫成 sqrt-num。下同
     (* x x)) ; 處理方法

    ((_ x y ...) ; 二元以上引數
     (+ (_ x)(_ y ...))) ;　處理方法，可遞迴表示
  )
)

;; 另一種寫法，利用尾遞迴來避免堆棧耗用過度
(define-syntax sqrt-sum
  (syntax-rules ()
    ((_ x) ; 一元引數時。底線_代表 sqrt-num 這個函數,當然也可以直接寫成 sqrt-num。下同
     (* x x)) ; 處理方法

    ((_ x y ...) ; 二元以上引數
     (begin ; 執行多個指令
       ; 定義尾遞迴表示
       (define (sqrt-sum-iter a arg-list)
       (if (eq? arg-list '()) a ; 若 arg-list 為空,傳回值
           ;; else 去 arg-list 頭,將頭的平方和加到 a
           (sqrt-sum-iter
            (+ a (* (car arg-list) (car arg-list)))
            (cdr arg-list))
       )
      )
     (sqrt-sum-iter 0 (list x y ...))) ;　處理方法，可用尾遞迴表示，避免堆棧使用過多
     )
  )
)

;; syntax-case 的使用，在 racket 可以用，但 Drracket 附隨的 scheme r5rs不行
;(define-syntax sqrt-sum
;   (lambda (stx) ;; 要加
;     (syntax-case stx ()
;       ((_ x)
;        (syntax (* x x))) ;; 執行的動作，要包 syntax
;       ((_ x y ...)
;        (syntax (+ (sqrt-sum x) (sqrt-sum y ...)))) ;; syntax 內不能使用 _ 表示原本函數
;       )))

; syntax-case 的使用，在 racket 可以用，但 Drracket 附隨的 scheme r5rs不行
; 另一種寫法，stx 表示 sqrt-num 的參數
;(define-syntax (sqrt-sum stx)
;   (syntax-case stx () (
;       ((_ x) ; lambda (stx) 等不要加
;       (syntax (* x x))) ;; 執行的動作，要包 syntax
;       ((_ x y ...)
;       (syntax (+ (sqrt-sum x) (sqrt-sum y ...)))) ;; syntax 內不能使用 _ 表示原本函數
;       ))

;; 其他關於 macro 可以參考：https://artyom.me/learning-racket-2
}

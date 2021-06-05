#lang scribble/manual
@(require (for-label racket))

@title{軟體開發}

這一篇章的重點不在語言，而在學習如同 racket 熟手一般的真正開發的技巧

@section{撰寫好程式}

這個章節討論 racket 常見的慣例、風格、傳統與陷阱，學習 racket 程式的最佳實踐。

@subsection{格式}

@subsubsection{避免過長的單行}

@codeblock{
(define (filter pred? lst)
  (cond
    [(null? lst) '()]
    [(pred? (car lst)) (cons (car lst) (filter pred? (cdr lst)))]
    [else (filter pred? (cdr lst))]))
}

適當的折行有助於閱讀

@codeblock{
(define (filter pred? lst)
  (cond
    [(null? lst)
     '()]
    [(pred? (car lst))
     (cons (car lst)
           (filter pred? (cdr lst)))]
    [else
     (filter pred? (cdr lst))]))
}

@subsubsection{不要拆散結尾括號}

以下的程式是非常好的 racket 風格

@codeblock{
(define (factorial n)
    (if (zero? n)
        1
        (* n (factorial (- n 1)))))
}

不要寫成

@codeblock{
(define (factorial n)
    (if (zero? n)
        1
        (* n (factorial (- n 1)))
    )
)
}

@subsubsection{子表達式的縮排要一致}

以下是好的案例

@codeblock{
(list (foo)
      (bar)
      (baz))
}

以下是例外，當子表達式是 body 時，我們給它兩個空格做縮排

@codeblock{
(let ((pi 3.14)
      (r 120))
  (* pi r r))
}

@subsubsection{折一個就全部折}

假設你有

@codeblock{
(+ 1 foo bar baz)
}

折行時就應該寫

@codeblock{
(+ 1
   foo
   bar
   baz)
}

@subsection{共通的最佳實踐}

@subsubsection{慎重的命名}
@subsubsection{為程式加上註解}
@subsubsection{保持函數簡短}
@subsubsection{避免重複的程式}
@subsubsection{避免重複的計算}
@subsubsection{使用高階函數}

@subsection{模組}

@section{閱讀好程式}

帶你看各種優秀的 racket 專案，學習自己發掘程式的設計。

@section{發佈好程式}

討論如何發佈寫好的程式庫。

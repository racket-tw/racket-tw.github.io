#lang scribble/manual
@(require (for-label metapict))

@title{metapict}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

metapict 是幫助大家更輕鬆的用程式繪圖的套件，可以通過以下指令安裝

@codeblock|{
raco pkg install metapict
}|

使用的方式是 @code{(require metapict)}

@codeblock|{
(require metapict)

(def p1 (pt   0 100))
(def p2 (pt 100 100))
(def p3 (pt 200 100))
(def p4 (pt   0   0))
(def p5 (pt 100   0))
(def p6 (pt 200   0))

(with-window (window -10 210 -5 105)
  (draw
   (curve p1 .. p6)
   (curve p2 .. p5)
   (curve p3 .. p4)))
}|

@code{window} 限制框框，@code{draw} 內部描述怎麼畫線，@code{curve} 中把點連起來，可以用 @code{--}（直線）或是 @code{..}（曲線）連結語法。以下是可以繪出曲線的連法

@codeblock|{
(with-window (window -10 210 -5 105)
  (draw (curve p1 .. p2 .. p6)))
}|

官方文件還有繪出字母的例子：

@codeblock|{
(define (A w h α)
  (set-curve-pict-size w h)
  (def p1 (pt    0    0))
  (def p2 (pt (/ w 2) h))
  (def p3 (pt    w    0))
  (def p4 (med α p1 p2))
  (def p5 (med α p3 p2))
  (with-window (window 0 w 0 h)
    (draw (curve p1 .. p2)
          (curve p2 .. p3)
          (curve p4 .. p5))))
(list (A 10 20 0.3)
      (A 10 20 0.4)
      (A 10 20 0.5)
      (A 10 20 0.6))
}|

還有更多使用方式，可以在@hyperlink["https://docs.racket-lang.org/metapict/index.html"]{官方文件}查看更多的用法。

#lang pollen

◊(require pollen/unstable/pygments)

◊h1{Module}

◊h3{檔案}

racket 的每一個檔案本身都會是一個模組(module)，例如：

◊highlight['racket]{
;;; hello.rkt
#lang racket

(provide print-hello)

(define (print-hello)
  (printf "Hello~n"))
}

對同一個目錄的檔案來說就可以用

◊highlight['racket]{
(require "hello.rkt")
}

引用這個模組中的定義。racket 將 require 後的字串解析為相對路徑，所以也可以用 "../xxx.rkt" 等方式引用。

◊h3{Collection}

◊em{Collection} 是已安裝的一群模組，引用 ◊em{Collection} 是用沒有檔案類型類後綴的路徑，下面引用了在 ◊em{Collection} "racket" 中的 module "date.rkt"

◊highlight['racket]{
(require racket/date)

(printf "Today is ~s\n"
        (date->string (seconds->date (current-seconds))))
}

除了內建的 ◊em{Collection}，可以用 raco pkg 指令取得第三方程式庫：

◊highlight['bash]{
pkg install cur
}

◊h3{專案}

可以用以下指令生成新的專案(開發新的 Collection)：

◊highlight['bash]{
raco pkg new <collection-name>
raco pkg install --auto
}

注意 install --auto 那行不能省略，不然會看到 test 指令瘋狂失敗 www。

在任意一個專案中的檔案都可以寫測試：

◊highlight['racket]{
#lang racket

(module+ test
  (require rackunit))

(define x 1)

(module+ test
  (check-equal? 1 x))
}

用以下指令執行測試：

◊highlight['bash]{
raco test .
}

如果最後刪除這個目錄，也要記得刪除 raco 中的紀錄：

◊highlight['bash]{
raco pkg remove <collection-name>
}

不然就會造成 raco 一直找不到該 collection 而沒辦法正常運作。

如果要開發 executable，在 main.rkt(生成的專案裡會有) 裡的 module+ main 裡面寫的程式就會是 executable 執行的東西，而仍然可以引用這個 collection 不會執行到這些程式。

◊h3{#lang}

#lang 是 racket 的語言核心，這是 racket 裡面唯一不可以拿掉的部分，事實上 #lang xxx 代表的是以 xxx 包裹這個模組的意思。
語法僅僅是表象這個說法在 racket 中發揮的淋漓盡致，最常見的 module language 就是 racket、racket/base。
例如說我們可以在 REPL 裡面打：

◊highlight['racket]{
> (module f racket
    (provide (except-out (all-from-out racket) lambda)
             (rename-out [lambda function])))
> (module use 'f
    ((function (x) x) 1))
> (require 'use)
1
}

暫時不用理解 provide 裡面 except-out rename-out 等等奇怪的東西。
我們只關心 module 語法，它接受 name 以及一個可選參數 module。這個 module provide 的 form 會成為該 module 的語言基礎。
而 #lang 也只是 module 的語法糖。

◊highlight['racket]{
#lang s-exp "html.rkt"
 
(title "Queen of Diamonds")
(p "Updated: " ,(now))
}

如果有 html.rkt 這個 module，s-exp "html.rkt" 就是 (module <file-name> "html.rkt") 而已。

◊h3{總結}

希望這段教學可以讓讀者看懂怎麼用 module 了，如果覺得有哪些資訊也應該放進這篇裡面可以寄信(◊author-mail)告訴我，
如果有想了解的內容但還沒有相關教學可以 ◊link["https://github.com/racket-tw/racket-tw.github.io/issues/new"]{開新 issue}。

◊p{}

◊link["https://github.com/dannypsnl"]{◊author} 編輯

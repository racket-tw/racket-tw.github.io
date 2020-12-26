#lang scribble/manual
@(require (for-label (except-in racket
                                extends)
                     nanopass/base))

@title{用 nanopass 做簡單的類型檢查}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

nanopass 是一種編譯器實作的思想，旨在每個 pass 都只做簡單的最佳化，與傳統個位數個 pass 就做完全部事情相比。這種作法雖然需要遍歷程式更多次，總體而言卻比傳統的做法更有效率，並且遠比傳統做法更容易維護。這次則是以簡單運用 nanopass 作為起頭，以後應該還會繼續擴展這個系列下去 XD。

@section{nanopass 入門}

nanopass 除了是一種思想，也是一套框架(由 Andy Keep 實作與維護)，它提供了一個由數個 macro 形成的 DSL 作為開發的核心。@code{define-language} 是我們應該最先認識到的 macro：

@racketblock[
(define (var? x) (symbol? x))
(define (constant? x)
  (or
    (number? x)
    (string? x)
    (char? x)))

(define-language ST
  (entry Stmt)
  (terminals
   (var (x param))
   (constant (c)))
  (Stmt (stmt)
        (: x t)
        (:= x e))
  (Expr (e)
        x
        c
        (λ ([param* t*] ...) t e)
        (e e* ...))
  (Typ (t)
       x
       (-> t* ... t)))
]

首先在 @code{terminals} 裡面我們會寫出 @code{(predicate [meta-name* ...])}，@code{var?} 和 @code{constant?} 就是我們用到的 predicate。@code{entry} 指定了進入這個語言的預設語法是什麼，接下來則是 @code{(語法 (語法-meta-name* ...) 語法-clause* ...)} 的宣告。我一直提到 @code{meta-name}，這到底有什麼用呢？這是指我們的語法裡面，除了第一個開頭字符之外，都必須是一個 @code{meta-name}，如果不是 nanopass 會抱怨不存在這個 @code{meta}。這個寫法單純只是偷懶，以 @code{(:= x e)} 為例，其實也可以做成 @code{(:= (name : var) (e : Expr))} 來顯式的標明是哪一種語法(使用哪個 predicate)，不過既然都這樣了就試著習慣它吧！多寫幾種 @code{meta-name} 可以有效的改善語法的可讀性，另外 @code{meta-name} 之後加一個字符是可接受的，而且常常需要這麼做，因為一個語法底下很可能會重複的使用同一個語法。另外 @code{...} 是處理零到多個的語法，需要一到多的話要寫成 @code{e* ... e}。

比起用複雜的說明，還是直接看怎麼處理程式可以更快的理解我們到底做了什麼。nanopass 提供了 @code{define-parser} 讓我們可以產生相應語法的 parser，寫下 @code{(define-parser parse-ST ST)} 之後我們就得到了 @code{parse-ST} 和 @code{unparse-ST}，執行 @code{(parse-ST '(:= a 1))} 就會得到 @code{(ST:Stmt::= a 1)} 這個結構，而 @code{unparse-ST} 正好相反，僅此而已。到這邊 nanopass 已經介紹的差不多了，接下來來看怎麼利用 nanopass 寫 simple type check 吧！

@section{簡單類型檢查}

這裏說的簡單，是指僅有 base 和 arrow type(即 function type)，而我們的頂層語法僅有 type-binding(@code{(: x t)}) 和 value-binding(@code{(:= x e)})，所以我們的流程就是先綁定 type，再檢查 value 是否符合 type，如果沒有 type-binding 則採用 inferred-type 做綁定。為此我們需要一個良好的 environment 實作，這裏我提供一個版本：

@racketblock[
(struct env (cur parent) #:transparent)
(define (make-env)
  (env (make-hash) (cur-env)))
(define cur-env (make-parameter (env (make-hash) #f)))

(define (bind id typ)
  (let ([cur-binding (env-cur (cur-env))])
    (when (hash-ref cur-binding id #f)
      (error 'semantic "cannot rebind: `~a`" id))
    (hash-set! cur-binding id typ)))
(define (lookup id)
  (let ([parent? (env-parent (cur-env))]
        [cur-binding (env-cur (cur-env))])
    (hash-ref cur-binding id
              (if parent?
                  (parameterize ([cur-env parent?])
                    (lookup id))
                  #f))))
]

如此一來我們只要靠 @code{(parameterize ([cur-env (make-env)]) ...)} 就可以自動階層化環境囉！由於我們沒有 polymorphism 這種需要實例化的麻煩東西，所以類型等全非常簡單：

@racketblock[
(define (ty-eq? t1 t2)
  (unless (equal? t1 t2)
    (error 'semantic "expected: ~a got: ~a" t1 t2)))
]

接下來就是核心部分了，首先我們綁定頂層類型(注意 @code{(unparse-ST t)}，這是必須的，要把 nanopass 生成的結構轉成普通的 s-expression)：

@racketblock[
(define-pass bind-type* : ST (s) -> ST ()
  (Stmt : Stmt (s) -> Stmt ()
        [(: ,x ,t)
         (bind x (unparse-ST t))]))
]

其他的程式會由 nanopass 自動生成，因為 @code{Stmt} 到 @code{Stmt} 對 nanopass 來說是已知的，不過這裡我們加入新的語言修改一下可以得到更多：

@racketblock[
(define-language L1
  (extends ST)
  (Stmt (stmt)
    (- (: x t))))
(define-pass bind-type* : ST (s) -> L1 ()
  (Stmt : Stmt (s) -> Stmt ()
        [(: ,x ,t)
         (bind x (unparse-ST t))]))
]

我們加入新的一層，並把 @code{(: x t)} 這個語法移除，現在你會發現 @code{bind-type*} 不再能夠自動生成，因為 @code{L1} 沒有 @code{(: x t)}，nanopass 不知道怎麼轉換這個東西，於是我們得繼續修改：

@racketblock[
(define-pass bind-type* : ST (s) -> * ()
  (Stmt : Stmt (s) -> * ()
        [(: ,x ,t)
         (bind x t)
         #f]
        [else #t]))
]

一旦給出 @code{* ()} 這個寫法，nanopass 就會要求這個 pass 必須完全覆蓋所有語法，因此這裏我們多了個 else clause，我們的回傳值表示了是否要繼續這樣的意思。因此我們會有以下函數：

@racketblock[
(define (all x)
  (let ([s (parse-ST x)])
    (when (bind-type* s)
      'continue)))
]

我們接著可以加上 guard，確保沒有意外的寫出錯誤的轉換(其實沒寫是沒差，只是 nanopass 本身報錯太難找位置了，加這個比較好定位)：

@racketblock[
(define-pass ST->L1 : ST (s) -> L1 ()
  (Stmt : Stmt (s) -> Stmt ()
    [(: ,x ,t) (error 'unreachable)]))

(define (all x)
  (let ([s (parse-ST x)])
    (when (bind-type* s)
      ((compose 'other-passes
                ST->L1) s))))
]

下一步我們需要「檢查 value 是否符合 type，如果沒有 type-binding 則採用 inferred-type 做綁定」，因此給出實作：

@racketblock[
(define-pass check-type* : L1 (s) -> L1 ()
  (Stmt : Stmt (s) -> Stmt ()
        [(:= ,x ,e)
         (if (lookup x)
             (ty-eq? (lookup x) (infer e))
             (bind x (infer e)))
         s]))

(define (all x)
  (let ([s (parse-ST x)])
    (when (bind-type* s)
      ((compose check-type*
                ST->L1) s))))
]

這沒什麼難以理解的地方，就是照字面寫而已，最後則是型別推導部分：

@racketblock[
(define-pass infer : L1 (e) -> * ()
  (Expr : Expr (e) -> * ()
        [(λ ([,param* ,t*] ...) ,t ,e)
         (parameterize ([cur-env (make-env)])
           (for ([p param*]
                 [t t*])
             (bind p t))
           (ty-eq? t (infer e)))
         `(-> ,@t* ,t)]
        [(,e ,e* ...)
         (match (infer e)
           [`(-> ,t* ... ,t)
            (for-each (λ (t e) (ty-eq? t (infer e)))
                      t* e*)
            t]
           [else (error 'semantic "not a funciton: ~a" e)])]
        [,x (lookup x)]
        [,c (cond
              [(number? c) 'number]
              [(string? c) 'string]
              [(char? c) 'char])]))
]

這裡是這篇最後一個 pass，可以看到採用了 @code{(parameterize ([cur-env (make-env)]) ...)} 的作法來隔開環境，回傳也是 @code{* ()}，而 application(function call) 需要檢查是不是 arrow type，其他的都頗為直覺。最後我們來看一下方才的成果：

@racketblock[
(all '(: a number))
(all '(:= a 1))
(all '(: id-number (-> number number)))
(all '(:= id-number (λ ([n number]) number n)))
(all '(:= result (id-number a)))

(lookup 'result)
]

結果應該會是 @code{'number}。歡迎提出問題或是改進建議，我們有 @link["https://discord.gg/xpwzAcx" "Discord channel"]，這篇的程式也可以在@link["https://github.com/racket-tw/simple-typer" "這個專案"]裡面找到喔！到這裡我相信讀者也已經對 nanopass 為我們省下了什麼有所感受，且可以用來編寫更多有趣的程式 XD，下次再見。

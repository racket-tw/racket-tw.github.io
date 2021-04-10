#lang scribble/manual
@(require (for-label racket))

@title{[Scheme] 安裝 Racket-llvm}
@author[(author+email "Yoxem" "yoxem.tem98@nctu.edu.tw")]

@itemlist[
    @item{Clone @hyperlink["https://github.com/bubba/racket-llvm"]{git repo}：
        @racketblock[
        git clone git@github.com:bubba/racket-llvm.git
        ]}
    @item{包做 .zip：
        @codeblock{
        raco pkg create --binary racket-llvm
        }}
    @item{安裝：
        @codeblock{
        raco pkg install racket-llvm.zip
        }}
]

看安裝 ê 結果，親像：
@codeblock{
    Installation-wide:
     Package            Checksum     Source
    ....
    User-specific for installation "6.3":
     Package      Checksum     Source
     racket-llvm  52ebbb73...  file /tmp/a/racket-llvm.zip
}

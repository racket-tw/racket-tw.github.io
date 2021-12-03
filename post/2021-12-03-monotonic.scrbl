#lang scribble/manual
@(require (for-label racket
                     monotonic))

@title{How to measure more percisely than milliseconds}
@author[(author+email "Lîm Tsú-thuàn" "dannypsnl@gmail.com")]

A common problem in Racket is that we can use @code{time} function to measure a codeblock's spend time, but it only provides milliseconds, what if we want more percise result? Library @code{monotonic} stands for this problem, by using @code{(current-monotonic-nanoseconds)}, we get nanoseconds.

@codeblock{
raco pkg install monotonic
}

And we can get microseconds by @code{current-inexact-milliseconds}, it will return value like @code{1289513737015.418}, where @code{1289513737015} is in milliseconds and @code{418} is in microseconds.

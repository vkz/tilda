#lang scribble/manual

@(require (for-label racket/base
                     racket/math
                     prelude/tilda)
          scribble/eval)

@(define eval~>
   (make-eval-factory '(prelude/tilda
                        racket/function
                        racket/list
                        racket/math)))

@title{Threading with tildas}

@defmodule[prelude/tilda]

@defform[#:literals (~ ~foo ~id)
         (~> expr clause ...)
         #:grammar
         ([clause thread-option
                  (expr ...)
                  (pre-expr ... hole post-expr ...)]
          [hole ~
                ~id]
          [thread-option (code:line #:with pat expr/hole)
                         (code:line #:do (expr/hole ...))]
          [expr/hole (pre-expr ... hole post-expr ...)
                     (expr ...)
                     expr])]{

@italic{Threads} the @racket[expr] through the next @racket[clause], then that
@racket[clause] through the one after it and so on. "Threads" here means that the
@racket[expr] will replace a @racket[hole], if there is one, in the
@racket[clause]. Clause with such replacement becomes the new @racket[expr] to be
threaded through the next clause. @racket[hole] is any unbound identifier that
starts with @racket[~].

 @(examples
   #:eval (eval~>)
   (~> '(1 2 3)
       (map add1 ~)
       (second ~)
       (* 2 ~))
   (~> "foo"
       (string->bytes/utf-8 ~)
       (bytes->list ~)
       (map (curry * 2) ~)
       (list->bytes ~)))

 @(examples
   #:eval (eval~>)
   (~> 6
       (range 1 ~upto)
       (filter odd?  ~)
       (findf even? ~)
       #:do ((unless ~num (<~ #f)))
       (* 2 ~))

   (~> '(1 6)
       #:with (list from upto) ~
       (range from upto)
       (filter odd?  ~)
       (findf even? ~)
       #:do ((unless ~num (<~ #f)))
       (* 2 ~)))
}
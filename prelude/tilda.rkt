#lang racket

(provide ~>)

(require (for-syntax syntax/parse
                     syntax/keyword
                     racket/match))

(define-syntax-rule (comment . any) (void))
(define-syntax-rule (example . any) (void))
(begin-for-syntax
  (define-syntax-rule (comment . any) (void))
  (define-syntax-rule (example . any) (void)))

;; TODO Split ~> into separate package either the same prelude collection, so it
;; is required as prelude/tilda and also reprovide ~> from prelude or maybe a
;; separate tilda collection? Document it with scribble. Pull it into prelude
;; proper and tables as needed.

;; TODO Ask mailing list to review and improve.

;; TODO easy to implement standard ~> and ~>> in terms of my ~> below, not sure I
;; really need them, though. Only decent syntactic solution I can think of is to
;; have three macros: ~ ~> ~>>, but then how do I tell ~ as a hole from everything
;; else? Use _ instead maybe?

;; TODO make ~ available in #:with and #:do. Here's my current idea. I can't
;; merily "temprarily" bind ~ say in rhs of #:with or #:do body, because, well
;; they could be using ~> inside, too. But more importantly, think what we're
;; doing here. ~ and friends are nothing but markers to be replaced during macro
;; expansion as needed. So, it isn't right to just treat ~ as nothing but a marker
;; sometimes but as a binding other times. I say we need to be consistent:
;;
;; #:do treat its body "as if" it's just a sequence of ~> clauses, replacing ~
;;      with its current expr (val) only inside outer most clause parens, just
;;      like we do in normal ~> clause. One nice pattern emerges: if you want to
;;      use current ~ value in the rest of the body simply bind it at the top of
;;      the body e.g. ((define cur ~) . body)
;;
;; #:with treat its rhs exactly like ~> clause similarly only replace ~ at the top
;;        level. Allow one special case when ~ or ~id is the entire rhs. Naturally
;;        I am thinking about allowing lhs to be a match pattern, with the whole
;;        thing transformed into a (match-define lhs rhs).
;;
;; I think the following example should now work as expected:
(example
 (~> 0
     (sub1 ~)
     (define val ex)
     #:with foo (pre .. ~ post ..)
     (add1 ~)
     #:do ((define bar ~))
     (list foo bar ~))
 ;; example
 )


(define-for-syntax (fix-outer/ctx ctx stx [loc #f])
  (datum->syntax ctx (syntax-e stx) loc))


(define-syntax ~>
  (syntax-parser
    ((_ clauses ...)
     #:with <~ (datum->syntax this-syntax '<~)
     #:with body (fix-outer/ctx this-syntax #'(impl~> clauses ...) this-syntax)
     #'(let/ec <~ body))))


(define-syntax (impl~> stx)

  (define (unbound? stx)
    (define top-level-or-unbound (not (identifier-binding stx)))
    (define not-top-level-bound (not (identifier-binding stx (syntax-local-phase-level) #t)))
    (and top-level-or-unbound
         not-top-level-bound))

  (define (~id? stx)
    (regexp-match #px"^~" (symbol->string (syntax-e stx))))

  (define-syntax-class ~
    (pattern id:id #:when (and (~id? #'id) (unbound? #'id))))

  (define-syntax-class clause
    #:attributes ((pre 1) hole (post 1))
    (pattern (pre ... hole:~ post ...))
    (pattern (pre ...)
             #:with (post ...) #'()
             #:attr hole #f))

  (define kw-table
    (list (list '#:guard check-expression)
          (list '#:do check-expression)
          (list '#:with check-identifier check-expression)))

  (define (options->syntaxes prev-clause value options)
    (for/list ((opt (in-list options)))
      (match opt
        ((list #:with ctx id e)
         (with-syntax ((id id) (e e))
           (fix-outer/ctx ctx #'(define id e) ctx)))

        ((list #:do ctx body)
         (define/syntax-parse (e:expr ...) body)
         (fix-outer/ctx ctx #'(begin e ...) ctx))

        ;; TODO I'm really hating this, either make error message helpful e.g. by
        ;; installing a contract boundary between clauses or ditch this thing. See
        ;; with-contract, invariant-assertion or maybe define/contract. I'd have
        ;; to fix blame object somehow. Are guards even useful here?
        ((list #:guard ctx guard)
         (with-syntax ((error (fix-outer/ctx ctx #`(error "guard failed") prev-clause)))
           (fix-outer/ctx guard #`(unless (#,guard #,value) error) guard)))

        ((list-rest kw ctx _)
         (raise-syntax-error #f (format "unexpected keyword ~a" kw) ctx ctx)))))

  (syntax-parse stx

    ;; no more clauses
    ((_ e:expr) #'e)

    ;; keyword options before the next clause
    ((_ e:expr (~peek _:keyword) . rest)
     #:do ((define-values (options clauses)
             (parse-keyword-options #'rest kw-table
                                    #:context this-syntax)))
     #:with (clause ...) clauses
     #:with body (fix-outer/ctx this-syntax #'(impl~> val clause ...) this-syntax)
     #:with (options ...) (datum->syntax this-syntax (options->syntaxes #'e #'val options) this-syntax)
     (fix-outer/ctx this-syntax
                    #'(begin (define val e) options ... body)
                    this-syntax))

    ;; clause with hole
    ((_ e:expr c:clause rest ...)
     #:when (attribute c.hole)
     #:with clause/e (fix-outer/ctx this-syntax #'(c.pre ... e c.post ...) #'c)
     (fix-outer/ctx this-syntax #'(impl~> clause/e rest ...) this-syntax))

    ;; clause with no hole
    ((_ e:expr c:clause rest ...)
     #:when (not (attribute c.hole))
     #:with clause (fix-outer/ctx this-syntax #'(c.pre ... c.post ...) #'c)
     (fix-outer/ctx this-syntax
                    #'(begin e (impl~> clause rest ...))
                    this-syntax))))


(module+ test

  (require rackunit)

  (check-eq? (~> 'foo
                 (symbol->string ~)
                 (format ":~a" ~str)
                 (string->symbol ~))
             ':foo)

  (check-eq? (~> 'foo
                 (symbol->string ~)
                 (format ":~a" ~str)
                 ;; threading can be split by expr that ignores the result
                 (list 42)
                 (car ~))
             42)

  ;; exn: symbol->string: contract violation
  (check-exn exn:fail:contract?
             (thunk
              (~> '42
                  (symbol->string ~)
                  (format ":~a" ~str)
                  (string->symbol ~))))

  ;; ensure macro introduced val doesn't capture outside val
  (check-eq? (let ((val 0))
               (~> val
                   (+ 1 ~)
                   #:do ()
                   ;; val must still be 0
                   (+ val ~)))
             1)

  (check-equal? (~> 'foo
                    (symbol->string ~)
                    #:with bar "-bar"
                    #:with baz "-baz"
                    (string-append ~foo bar baz)
                    (format ":~a" ~str)
                    (string->symbol ~)
                    #:do ((define l (list 1 2))
                          (set! l (cons 0 l)))
                    (cons ~sym l))
                '(:foo-bar-baz 0 1 2))

  (check-eq? (~> 0
                 (add1 ~)
                 #:do ((define bar 42)
                       (<~ bar))
                 (list bar ~))
             42))

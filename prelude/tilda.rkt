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


(begin-for-syntax
  (require (for-syntax racket/base))
  (define-syntax (fix-outer/ctx stx)
    (syntax-case stx ()
      ((_ ctx stxe srcloc) #'(datum->syntax ctx (syntax-e stxe) srcloc))
      ((_ ctx stxe)        #'(fix-outer/ctx ctx stxe ctx))
      ;; HACK assumes being called from inside syntax-parse
      ((_ stxe)            (with-syntax ((ctx (datum->syntax stx #'this-syntax)))
                             #'(fix-outer/ctx ctx stxe ctx))))))


(define-syntax ~>
  (syntax-parser
    ((_ clauses ...)
     #:with <~ (datum->syntax this-syntax '<~)
     #:with body (fix-outer/ctx #'(impl~> clauses ...))
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

  (define-syntax-class (do-expr val)
    (pattern c:clause #:when (attribute c.hole)
             #:with val val
             #:with subst (fix-outer/ctx #'c #'(c.pre ... val c.post ...)))
    (pattern e
             #:with subst #'e))

  (define-syntax-class (with-rhs val)
    (pattern id:~ #:with subst (fix-outer/ctx val val #'id))
    (pattern (~var e (do-expr val)) #:with subst #'e.subst))

  (define kw-table
    (list (list '#:guard check-expression)
          (list '#:do check-expression)
          (list '#:with check-expression check-expression)
          (list '#:as check-expression)))

  (define (options->syntaxes prev-clause value options)
    (for/list ((opt (in-list options)))
      (match opt

        ((list #:as ctx p)
         (define/syntax-parse pat p)
         (define/syntax-parse rhs value)
         (fix-outer/ctx ctx #'(match-define pat rhs)))

        ((list #:with ctx p e)
         (define/syntax-parse pat p)
         (define/syntax-parse (~var rhs (with-rhs value)) e)
         (fix-outer/ctx ctx #'(match-define pat rhs.subst)))

        ((list #:do ctx body)
         (define/syntax-parse ((~var e (do-expr value)) ...) body)
         (fix-outer/ctx ctx #'(begin e.subst ...)))

        ;; TODO I'm really hating this, either make error message helpful e.g. by
        ;; installing a contract boundary between clauses or ditch this thing. See
        ;; with-contract, invariant-assertion or maybe define/contract. I'd have
        ;; to fix blame object somehow. Are guards even useful here?
        ((list #:guard ctx guard)
         (with-syntax ((error (fix-outer/ctx ctx #`(error "guard failed") prev-clause)))
           (fix-outer/ctx guard #`(unless (#,guard #,value) error))))

        ((list-rest kw ctx _)
         (raise-syntax-error #f (format "unexpected keyword ~a" kw) ctx ctx)))))

  (syntax-parse stx

    ;; no more clauses
    ((_ e:expr) #'e)

    ;; keyword options before the next clause
    ((_ e:expr (~peek _:keyword) . rest)
     #:do ((define-values (options clauses) (parse-keyword-options
                                             #'rest kw-table
                                             ;; report errors in terms of ~>
                                             #:context #'(~> e . rest))))
     #:with (clause ...) clauses
     #:with body (fix-outer/ctx #'(impl~> val clause ...))
     #:with (options ...) (datum->syntax this-syntax (options->syntaxes #'e #'val options)
                                         this-syntax)
     (fix-outer/ctx #'(begin (define val e) options ... body)))

    ;; clause with a hole
    ((_ e:expr c:clause rest ...)
     #:when (attribute c.hole)
     #:with clause/e (fix-outer/ctx this-syntax #'(c.pre ... e c.post ...) #'c)
     (fix-outer/ctx #'(impl~> clause/e rest ...)))

    ;; clause with no holes
    ((_ e:expr c:clause rest ...)
     #:when (not (attribute c.hole))
     #:with clause (fix-outer/ctx this-syntax #'(c.pre ... c.post ...) #'c)
     (fix-outer/ctx #'(begin e (impl~> clause rest ...))))))


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

  (check-eq? 0 (~> 0))
  (check-eq? 0 (~> 0 #:do ()))

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
             42)

  (check-equal? '(0 1 2) (~> 0
                             #:do ((define foo ~))
                             (add1 ~)
                             #:do ((define bar ~))
                             (add1 ~)
                             (list foo bar ~)))

  (check-equal? '(1 1 2 1) (~> 0
                               (add1 ~)
                               #:do ((define foo ~)
                                     42
                                     (define bar ~)
                                     (define baz (add1 foo)))
                               (list foo bar baz ~)))

  (check-equal? '(0 0 2) (~> 0
                             #:with foo ~
                             (add1 ~)
                             #:with bar (sub1 ~)
                             (add1 ~)
                             (list foo bar ~)))

  (check-true (~> '(0 1 2)
                  #:with (list a b c) ~
                  (equal? ~ (list a b c))))

  ;; bound ~id is not treated as a hole so isn't replaced
  (check-equal? '(6 1) (let ((~foo 1))
                         (list (~> 2
                                   #:with (list a b) (list ~foo ~)
                                   (+ ~foo ~ a b))
                               ~foo)))

  ;; scoping rules work as expected
  (check-equal? '(42 1) (let ((~foo 1))
                          (list (~> 0
                                    #:with ~foo 42
                                    (+ ~foo ~))
                                ~foo)))

  ;; #:as and short-circuit with or
  (check-equal? (list 6 (range 1 6)) (~> 6
                                         #:as upper-limit
                                         (range 1 ~)
                                         #:as seq
                                         (filter odd?  ~)
                                         (findf even? ~)
                                         (or ~num (<~ (list upper-limit seq)))
                                         (* 2 ~))))


;; TODO define~> and λ~> idea
(example
 (define~> (even*2 from ~upto)
   (range from ~)
   (filter odd?  ~)
   (findf even? ~)
   #:do ((unless ~num (<~ #f)))
   (* 2 ~))

 (define even*2 (λ (from . ~rest)
                  (car ~)
                  (range from ~upto)
                  (filter odd?  ~)
                  (findf even? ~)
                  #:do ((unless ~num (<~ #f)))
                  (* 2 ~)))
 ;; example
 )

;; TODO easy to implement standard ~> and ~>> in terms of my ~>, not sure I really
;; need them, though. Only decent syntactic solution I can think of is to have
;; three macros: ~ ~> ~>>, but then how do I tell ~ as a hole from everything
;; else? Use _ instead maybe?

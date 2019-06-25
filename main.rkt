#lang racket


;;* Provides -------------------------------------------------------- *;;


(provide ~> define~> lambda~>
         (rename-out [lambda~> λ~>]))


(require (for-syntax syntax/parse
                     syntax/keyword
                     syntax/parse/lib/function-header
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
                             #'(fix-outer/ctx ctx stxe ctx)))))

  (define (unbound? stx)
    (define top-level-or-unbound (not (identifier-binding stx)))
    (define not-top-level-bound (not (identifier-binding stx (syntax-local-phase-level) #t)))
    (and top-level-or-unbound
         not-top-level-bound))

  (define (~pred? stx)
    (match (symbol->string (syntax-e stx))
      ((regexp #px"^~(.*[?])$" (list _ pred)) (datum->syntax stx (string->symbol pred) stx))
      (else #f)))

  (define (~id? stx)
    (and (regexp-match #px"^~" (symbol->string (syntax-e stx)))
         (not (~pred? stx))))

  (define-syntax-class hole
    (pattern hole:id
             #:attr pred (~pred? #'hole)
             #:when (and (attribute pred) (unbound? #'hole)))
    (pattern hole:id
             #:when (and (~id? #'hole) (unbound? #'hole))
             #:attr pred #f)))


;;* ~> -------------------------------------------------------------- *;;


(require racket/stxparam)


(define-syntax-parameter return #f)


(define-syntax ~>
  (syntax-parser
    ((_ clauses ...)
     #:with <~ (datum->syntax this-syntax '<~)
     #:with body (fix-outer/ctx #'(impl~> clauses ...))
     #'(let/ec <~
         (syntax-parameterize ((return (make-rename-transformer #'<~)))
           body)))))


(define-syntax (impl~> stx)

  (define-syntax-class clause
    #:attributes ((pre 1) hole pred (post 1))
    (pattern (pre ... hole:hole post ...)
             #:attr pred (attribute hole.pred))
    (pattern (pre ...)
             #:with (post ...) #'()
             #:attr hole #f
             #:attr pred #f))

  (define-syntax-class (do-expr val)
    (pattern c:clause #:when (attribute c.hole)
             #:with val val
             #:with subst (fix-outer/ctx #'c #'(c.pre ... val c.post ...)))
    (pattern e
             #:with subst #'e))

  (define-syntax-class (with-rhs val)
    (pattern id:hole #:with subst (fix-outer/ctx val val #'id))
    (pattern (~var e (do-expr val)) #:with subst #'e.subst))

  (define kw-table
    (list (list '#:do check-expression)
          (list '#:when check-expression check-expression)
          (list '#:unless check-expression check-expression)
          (list '#:with check-expression check-expression)
          (list '#:as check-expression)))

  (define (options->syntaxes prev-clause value options)

    (define opt->stx
      (match-lambda
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

        ((list #:when ctx lhs rhs)
         (define/syntax-parse val value)
         (define/syntax-parse pred lhs)
         (define/syntax-parse (~var consequent (with-rhs value)) rhs)
         (fix-outer/ctx ctx #'(when (pred val) (return consequent.subst))))

        ((list #:unless ctx lhs rhs)
         (define/syntax-parse val value)
         (define/syntax-parse pred lhs)
         (define/syntax-parse (~var consequent (with-rhs value)) rhs)
         (fix-outer/ctx ctx #'(unless (pred val) (return consequent.subst))))

        ((list-rest kw ctx _)
         (raise-syntax-error #f (format "unexpected keyword ~a" kw) ctx ctx))))

    (for/list ((opt (in-list options)))
      (opt->stx opt)))

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

    ;; clause with ~pred? hole
    ((_ e:expr c:clause rest ...)
     #:when (attribute c.pred)
     #:with val #'val
     #:with clause/e (fix-outer/ctx this-syntax #'(c.pre ... val c.post ...) #'c)
     (fix-outer/ctx #'(begin (define val e)
                             (unless (c.pred val) (return #f))
                             (impl~> clause/e rest ...))))

    ;; clause with ~id hole
    ((_ e:expr c:clause rest ...)
     #:when (and (attribute c.hole) (not (attribute c.pred)))
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
                                         (* 2 ~)))

  (check-eq? 6 (~> 6
                   #:unless odd? ~
                   (range 1 ~)))

  (check-equal? '(5 6) (~> 6
                           #:as num
                           #:when even? (list 5 ~)
                           (list num ~)))

  ;; these test two things:
  ;;   (a) nested ~>
  ;;   (b) nested escapes with <~
  (check-equal? '(3 1) (~> '()
                           (cons 1 ~)
                           (~> ~
                               #:as a
                               #:when list? a
                               (cons 2 ~))
                           (cons 3 ~)))

  ;; TODO consider replacing all occurrences of ~ in a clause. Until then we have
  ;; to bind with #:as as in the test case above instead of writing:
  #;(~> '()
        (cons 1 ~)
        (~> ~
            #:when list? ~
            (cons 2 ~))
        (cons 3 ~))

  (check-equal? '(3 2 1) (~> '()
                             (cons 1 ~)
                             (~> ~
                                 (cons 2 ~))
                             (cons 3 ~)))

  (check-equal? '(3 1) (~> '()
                           (cons 1 ~)
                           (~> ~
                               #:as foo
                               (cons 2 (<~ foo)))
                           (cons 3 ~)))

  ;; ~pred?
  (check-eq? (~> 42 (+ 1 ~number?)) 43)
  (check-false (~> 'foo (+ 1 ~number?)))
  (check-exn #rx"foo\\?: unbound identifier" (thunk
                                              (convert-compile-time-error
                                               (~> 42 (+ 1 ~foo?))))))

;;* define~> and lambda~> ------------------------------------------- *;;


(define-for-syntax (hole-bound-error name ctx)
  (raise-syntax-error
   name (format "~a, ~a" "attempt to bind hole-marker ~ "
                "consider using a meaningful ~id as parameter instead")
   ctx ctx))


(define-syntax define~>
  (syntax-parser
    ;; TODO Unsatisfied by this solution. IMO the right approach would be to make
    ;; ~ unbound so it can be used as a marker in the body, sadly I don't yet know
    ;; how to manipulate scopes appropriately.
    ((_ header:function-header clause ...)
     #:with (_ ... param:hole _ ...) #'header.params
     #:when (eq? (syntax-e #'param) '~)
     (hole-bound-error 'define~> #'param))

    ((_ header:function-header clause ...)
     #:with (_ ... param:hole _ ...) #'header.params
     #:with thread (fix-outer/ctx #'(~> param clause ...))
     #'(define header thread))))


(define-syntax lambda~>
  (syntax-parser
    ((_ header:formals clause ...)
     #:with (_ ... param:hole _ ...) #'header.params
     #:when (eq? (syntax-e #'param) '~)
     (hole-bound-error 'lambda~> #'param))

    ((_ header:formals clause ...)
     #:with (_ ... param:hole _ ...) #'header.params
     #:with thread (fix-outer/ctx #'(~> param clause ...))
     (fix-outer/ctx #'(lambda header thread)))))


(module+ test
  (require syntax/macro-testing)

  (define~> ((foo~> . ~arg) b #:c [c 3])
    (list* b c ~)
    #:as all
    (last ~)
    #:when even? 'even
    (+ ~ (car all)))

  (check-eq? ((foo~> 0 1) 2 #:c 3) 3)
  (check-eq? ((foo~> 0 1) 2) 3)
  (check-eq? ((foo~> 0 2) 3) 'even)

  (check-exn #rx"attempt to bind hole-marker"
             (thunk
              (convert-compile-time-error
               (define~> ((foo . ~) b c)
                 (list* b c ~)
                 (car ~)
                 (add1 ~)))))

  (check-exn #rx"attempt to bind hole-marker"
             (thunk
              (convert-compile-time-error
               (lambda~> ~ (car ~)))))

  (check-eq? ((lambda~> (a b . ~rest) (map add1 ~) (list* ~) (last ~)) 1 2 3 4) 5)
  (check-eq? ((lambda~> ~args (cdr ~) (last ~)) 1 2 3) 3))


(module+ test
  (test-case "Use #%app from macro invocation context"
    (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (~> 1 (2 ~) (3 ~)))
                  '(3 (2 1)))

    (let ([->proc (λ (x) (if (symbol? x) (λ (hsh) (hash-ref hsh x)) x))]
          [h (hasheq 'x (hasheq 'y 1))])
      (let-syntax ([#%app (syntax-rules () [(_ x . rest) (#%app (->proc x) . rest)])])
        (define~> (getxy ~h) ('x ~) ('y ~))

        (check-equal? (~> h ('x ~) ('y ~)) 1)
        (check-equal? ((lambda~> (~h) ('x ~) ('y ~)) h) 1)
        (check-equal? (getxy h) 1)))))


;; TODO easy to implement standard ~> and ~>> in terms of my ~>, not sure I really
;; need them, though. Only decent syntactic solution I can think of is to have
;; three macros: ~ ~> ~>>, but then how do I tell ~ as a hole from everything
;; else? Use _ instead maybe?

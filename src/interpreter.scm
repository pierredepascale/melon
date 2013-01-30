;;; interpreter.scm -- interpreter for the Melon language

(define (filter pred? ls)
  (cond ((null? ls) ls)
	((pred? (car ls)) (cons (car ls) (filter pred? (cdr ls))))
	(else (filter pred? (cdr ls)))))

(define (make-class name parent) (vector 'class name parent))
(define (class? obj) (and (vector? obj) (eq? (vector-ref obj 0) 'class)))

(define (instance? obj type) (subtype? (type-of obj) type))
(define (subtype? t1 t2)
  (or (eq? t1 t2)
      (let check ((parents (vector-ref t1 2)))
        (if (null? parents)
            #f
            (or (subtype? (car parents) t2)
                (check (cdr parents)))))))

(define (type-of obj)
  (cond ((string? obj) <str>)
        ((number? obj) <num>)
        ((or (eq? obj #t) (eq? obj #f)) <bool>)
        (else (error "Unknown object"))))

(define (make-method specializers proc) (vector 'method specializers proc))
(define (method? obj) (and (vector? obj) (eq? (vector-ref obj 0) 'method)))

(define (apply-method method args) (apply (vector-ref method 2) args))
(define (apply-methods methods args)
  (if (null? methods)
      (error "No more methods")
      (apply-method (car methods) (cons (lambda () (apply-methods (cdr methods) args))
                                        args))))

(define (method-applicable? method args-type)
  (let check ((t1 (vector-ref method 1))
              (t2 args-type))
    (or (null? t1)
        (and (subtype? (car t2) (car t1))
             (check (cdr t1) (cdr t2))))))

(define (method-more-specific? m1 m2)
  (let check ((t1 (vector-ref m1 1))
              (t2 (vector-ref m2 1)))
    (or (null? t1)
        (and (subtype? (car t1) (car t2))
             (check (cdr t1) (cdr t2))))))

(define (sort-applicable-methods methods less?)
  (let sort ((ms methods)
             (smallest #f)
             (rest '()))
    (if (null? ms)
        (if smallest
            (cons smallest (sort-applicable-methods rest less?))
            '())
        (if smallest 
            (if (less? smallest (car ms))
                (sort (cdr ms) smallest (cons (car ms) rest))
                (sort (cdr ms) (car ms) (cons smallest rest)))
            (sort (cdr ms) (car ms) rest)))))

(define (make-generic name methods) (vector 'generic name methods))
(define (generic? obj) (and (vector? obj) (eq? 'generic (vector-ref obj 0))))

(define (apply-generic generic args)
  (let* ((args-type (map type-of args))
         (methods (filter (lambda (m) (method-applicable? m args-type))
                          (vector-ref generic 2)))
         (sorted-methods (sort-applicable-methods methods method-more-specific?)))
    (apply-methods sorted-methods args)))

(define <any> (make-class "any" '()))
(define <str> (make-class "string" (list <any>)))
(define <num> (make-class "number" (list <any>)))
(define <bool> (make-class "boolean" (list <any>)))

(define print-any (make-method (list <any>) (lambda (next obj) (display "<any>"))))
(define print-str (make-method (list <str>) (lambda (next obj) (next) (display "<str>"))))
(define print-num (make-method (list <num>) (lambda (next obj) (display "<num>"))))
(define print-bool (make-method (list <bool>) (lambda (next obj) (display "<bool>"))))
(define (print obj)
  (apply-generic (make-generic "print" (list print-any print-str print-num print-bool))
                 (list obj)))

;;; INTERPRETER ==================================================================

(define *global* '())
(define (bind-env name value env) (cons (cons name value) env))
(define (bind-global! name value) 
  (let ((probe (assq name *global*)))
    (if probe
        (set-cdr! probe value)
        (set! *global* (bind-env name value *global*)))))
(define (is? form exp) (and (pair? exp) (eq? (car exp) form)))

(bind-global! 'print (make-generic "print" (list print-any print-str print-num print-bool)))
(bind-global! 'Any <any>)
(bind-global! 'String <str>)
(bind-global! 'Num <num>)
(bind-global! 'Bool <bool>)

(define (ev-if exp env)
  (let ((t (ev (cadr exp) env))) 
    (if t (ev (caddr exp) env) (ev (cadddr exp) env))))

(define (add-method gen method)
  (cond ((method? gen) (make-generic "<anonymous-generic>" (list gen method)))
        ((generic? gen) (vector-set! gen 2 (cons method (vector-ref gen 2))))
        (else method)))

(define (ev-define exp env)
  (let ((var (cadr exp))
	(val (ev (caddr exp) env)))
    (set! *global* (cons (cons var val) *global*))))

(define (ev-def exp env)
  (let* ((var (cadr exp))
	 (old (assq var env))
	 (new (ev `(lambda ,(caddr exp) ,(cadddr exp)) env)))
    (if old
	(set-cdr! old (add-method (cdr old) new))
	(set! *global* (cons (cons var new) *global*)))))

(define (ev-application exp env)
  (let* ((app (map (lambda (e) (ev e env)) exp))
         (op (car app)))
    (cond ((method? op)
           (apply-method op (cons (lambda () (error "no more next method")) (cdr app))))
          ((generic? op) (apply-generic op (cdr app)))
          (else (error "calling a non function ~a" op)))))

(define (bind-arguments params args env)
  (if (null? params)
      env
      (cons (cons (car params) (car args))
            (bind-arguments (cdr params) (cdr args) env))))

(define (ev-method exp env)
  (let* ((formals (cadr exp))
         (params (map (lambda (a) (if (pair? a) (car a) a)) formals))
         (types (map (lambda (e) (ev e env))
                     (map (lambda (a) (if (pair? a) (cadr a) <any>)) formals))))
    (make-method types
                 (lambda args 
                   (ev (caddr exp)
		       (bind-arguments (cons 'next params) args env))))))

(define (ev-let exp env)
  (let* ((binding (cadr exp))
	 (name (car binding))
	 (value (ev (cadr binding) env)))
    (ev (caddr exp) (bind-env name value env))))

(define (ev-letrec exp env)
  (let* ((binding (cadr exp))
	 (name (car binding))
	 (env* (bind-env name #f env))
	 (value (ev (cadr binding) env*)))
    (set-cdr! (car env*) value)
    (ev (caddr exp) env*)))

(define (ev-set! exp env)
  (let ((binding (assq (cadr exp) env)))
    (if binding
	(set-cdr! binding (ev (caddr exp) env))
	(bind-global! (cadr exp) (ev (caddr exp) env)))))

(define (ev-quote exp env) (cadr exp))
(define (ev exp env)
  (cond ((is? 'if exp) (ev-if exp env))
        ((is? 'define exp) (ev-define exp env))
	((is? 'def exp) (ev-def exp env))
        ((is? 'let exp) (ev-let exp env))
        ((is? 'letrec exp) (ev-letrec exp env))
        ((is? 'begin exp) (ev* (cdr exp) env))
        ((is? 'lambda exp) (ev-method exp env))
        ((is? 'set! exp) (ev-set! exp env))
        ((is? 'quote exp) (ev-quote exp env))
        ((is? 'fragments exp) (ev-fragments (cadr exp) env))
        ((is? 'syntax-call exp)
         (bind-syntax-call! (cadr exp) (lambda (f e)
                                         (ev (cadddr exp) (bind-env 'e (list f) env)))))
        ((is? 'syntax-stmt exp)
         (bind-syntax-stmt! (cadr exp) (lambda (f e)
                                         (ev (cadddr exp) (bind-env 'e (list f) env)))))
        ((is? 'syntax-definer exp)
         (bind-syntax-define! (cadr exp) (lambda (f e)
                                            (ev (cadddr exp) (bind-env 'e (list f) env)))))
        ((is? 'syntax-case exp)
         (ev-syntax-case (ev (cadr exp) env) (caddr exp) env))
        ((pair? exp) (ev-application exp env))
        ((symbol? exp) 
         (let ((probe (or (assq exp env)
                          (assq exp *global*))))
           (if probe 
               (cdr probe) 
               (error "unbound variable ~a in ~a" exp env))))
        (else exp)))

(define (ev* exps env)
  (if (null? exps)
      #f
      (let ((val (ev (car exps) env)))
        (if (null? (cdr exps)) val (ev* (cdr exps) env)))))

(define (ev-fragments f env)
  (cond ((define? f) (list (make-define (ev-fragments (define-body f) env))))
        ((name? f)
         (if (var? (lex-keyword-name (name-id f)))
             (let ((var-name (var-name (lex-keyword-name (name-id f)))))
               (if (var? var-name)
                   (list (make-name (make-lex-keyword var-name)))
                   (let ((val (assq (string->symbol var-name) env)))
                     (if val
                         (cdr val)
                         (error "unknown fragment binding ~a in ~a" f env)))))
             (list f)))
        ((immediate? f) (list f))
        ((nested? f) (list (make-nested (nested-open f)
                                        (nested-close f)
                                        (ev-fragments (nested-body f) env))))
        ((syntax-call? f) (list (make-syntax-call (syntax-call-name f)
                                                  (ev-fragments (syntax-call-body f) env))))
        ((syntax-stmt? f) (list (make-syntax-stmt (syntax-stmt-name f)
                                                  (ev-fragments (syntax-stmt-body f) env))))
        ((punctuation? f) (list f))
;        ((and (pair? f) 
;              (pair? p) 
;              (name? (car p)) 
;              (var? (lex-keyword-name (name-id (car p)))))
;         (let* ((type (var-type (lex-keyword-name (name-id (car p)))))
;                (expander (assoc type *pattern-types*)))
;           (if expander
;               ((cdr expander) f p env)
;               (error "unknown pattern variable type ~a in ~a"
;                      type
;                      (name-id (car p))))))
        ((pair? f) (append (ev-fragments (car f) env) (ev-fragments (cdr f) env)))
        ((null? f) f)
        (else (error "Unknown code fragment ~a" f))))         


(define (ev-syntax-case val cases env)
  (display "syn-case:") (write val) (newline) (write cases) (newline)
  (if (null? cases)
    #f
    (let* ((rule (car cases))
	   (pattern (car rule))
	   (expression (cdr rule)))
        (let ((binding (pattern-match val pattern '() env)))
;	  (display "Matching:") (write pattern) (newline) (write val) (newline)
;	  (display "result:") (write binding) (newline)
          (if binding
              (ev expression (append (map (lambda (b) (cons (string->symbol (car b))
                                                            (cdr b))) binding) 
                                     env))
              (ev-syntax-case val (cdr cases) env))))))

(define (repeat n obj) (if (= n 0) '() (cons obj (repeat (- n 1) obj))))

(define (prim arity name impl)
  (bind-global! name (make-method (repeat arity <any>)
				  (lambda (next . args) (apply impl args)))))

;; ;;; number

(prim 1 '%puts display)
(prim 2 'instance? instance?)
(prim 2 'subtype? subtype?)

(prim 2 '+ +)
(prim 2 '- -)
(prim 2 '* *)
(prim 2 '/ /)

(prim 2 'display display)

;; (prim 'mod modulo)
;; (prim 'rem remainder)
;; (prim 'min min) (prim 'max max) (prim 'abs abs)
;; (prim 'numerator numerator) (prim 'denominator denominator)
;; (prim 'gcd gcd) (prim 'lcm lcm)
;; (prim '/ /)
(prim 2 '< <)
(prim 2 '<= <=)
;; (prim '= equal?) (prim '~= (lambda (a b) (not (equal? a b))))
;; (prim '== eq?) (prim '~== (lambda (a b) (not (eq? a b))))
;; (prim '> >) (prim '>= >=)
;; (prim 'number? number?) (prim 'complex? complex?)
;; (prim 'real? real?) (prim 'rational? rational?)
;; (prim 'integer? integer?)
;; (prim 'zero? zero?) (prim 'positive? positive?) (prim 'negative? negative?)
;; (prim 'odd? odd?) (prim 'even? even?)
;; (prim 'exact->inexact exact->inexact) (prim 'inexact->exact inexact->exact)

;; (prim 'number->string number->string) (prim 'string->number string->number)

;; ;;; boolean

;; (prim '~ not)
;; (prim 'boolean? boolean?)

;; ;;; pairs

;; (prim 'pair? pair?) (prim 'make-pair cons) (prim 'head car) (prim 'tail cdr)
;; (prim 'list list) (prim 'set-car! set-car!) (prim 'set-cdr! set-cdr!)
;; (prim 'list? list?) (prim 'null? null?) (prim 'length length)
;; (prim 'append append) (prim 'reverse reverse) (prim 'list-ref list-ref)

;; ;;; symbols

;; (prim 'symbol? symbol?)
;; (prim 'symbol->string symbol->string)
;; (prim 'string->symbol string->symbol)

;; ;;; vector

;; (prim 'vector? vector?) (prim 'make-vector make-vector)
;; (prim 'aref vector-ref) (prim 'aset! vector-set!) 
;; (prim 'vector-length vector-length)
;; (prim 'vector->list vector->list) (prim 'list->vector list->vector)

;; ;;; procedure

;; (prim 'apply apply) (prim 'procedure? procedure?)
;; (prim 'map map) (prim 'for-each for-each)

;; ;;; char

;; (prim 'char? char?)
;; (prim 'char=? char=?) (prim 'char<? char<?) (prim 'char>? char>?)
;; (prim 'char>=? char>=?) (prim 'char<=? char<=?) 
;; (prim 'char-ci=? char-ci=?) (prim 'char-ci<? char-ci<?) (prim 'char-ci>? char-ci>?)
;; (prim 'char-ci>=? char-ci>=?) (prim 'char-ci<=? char-ci<=?) 
;; (prim 'char->number char->integer) (prim 'number->char integer->char)
;; (prim 'char-upcase char-upcase) (prim 'char-downcase char-downcase)

;; ;;; string

;; (prim 'string? string?) (prim 'make-string make-string) (prim 'string string)
;; (prim 'string-ref string-ref) (prim 'string-set! string-set!)
;; (prim 'string-length string-length)
;; (prim 'string=? string=?) (prim 'string-ci=? string-ci=?)
;; (prim 'string<? string<?) (prim 'string>? string>?)
;; (prim 'string<=? string<=?) (prim 'string>=? string>=?)
;; (prim 'string-append string-append)
;; (prim 'string->list string->list) (prim list->string list->string)
;; (prim 'string-copy string-copy)

;; ;;; io

;; (prim 'eof-object? eof-object?)
;; (prim 'input-port? input-port?) (prim 'output-port? output-port?)  
;; (prim 'read-char read-char) (prim 'write-char write-char)
;; (prim 'display display) (prim 'newline newline)

;; (prim 'open-input-file open-input-file) 
;; (prim 'open-output-file open-output-file)

;; (prim 'close-input-port close-input-port) 
;; (prim 'close-output-port close-output-port)

;; ;; eta expanded because the procedure melon is defined later
;; (prim 'require (lambda (fn) (melon fn)))

;; ;;; gambit os primitives
;; (prim 'getenv getenv) (prim 'real-time real-time)
;; (prim 'cpu-time cpu-time) (prim 'runtime cpu-time)

;; (define-structure instance slots)

;; (prim '%alloc (lambda (n) (make-instance (make-vector n #f))))
;; (prim '%ref (lambda (u i) 
;;                (let ((slots (instance-slots u))) (vector-ref slots i))))
;; (prim '%set! (lambda (u i o)
;;                 (let ((slots (instance-slots u))) (vector-set! slots i o))))
;; (prim '%length (lambda (u) (vector-length (instance-slots u))))

;(prim '%imm %imm)
;(prim '%type %type)
;(prim '%alloc alloc) (prim '%ref %ref) (prim '%set! %set!) 
;(prim '%length %length)


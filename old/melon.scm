;;; dylan.scm -- Dylan Macros Implementation

;; Author: Pierre De Pascale
;; Date: 17 Jul 2004


;;; Lexer ======================================================================
;; Lexeme definition
;; keyword     ::= [a-zA-Z-_:+@*/%=?!<>]
;; literal     ::= (integer | string | char | boolean | vector | list)
;; punctuation ::= [;,()[]]*
;; 
(define (lex-predicate sym) 
  (lambda (exp) (and (pair? exp) (eq? (car exp) sym))))

(define lex-punctuation? (lex-predicate 'punct))
(define (make-lex-punctuation punct) (list 'punct punct))
(define lex-punctuation-char cadr)
(define (lex-punctuation=? lex ch) (and (lex-punctuation? lex)
                                        (char=? (lex-punctuation-char lex) ch)))

(define lex-keyword? (lex-predicate 'keyword))
(define (make-lex-keyword str) (list 'keyword str))
(define lex-keyword-name cadr)
(define (lex-keyword=? lex key)
  (and (lex-keyword? lex) (string=? (lex-keyword-name lex) key)))

(define (lex-literal? lex)
  (or (lex-number? lex) (lex-string? lex) (lex-char? lex)))
  
(define lex-number? (lex-predicate 'num))
(define (make-lex-number num) (list 'num num))
(define lex-number cadr)

(define lex-string? (lex-predicate 'string))
(define (make-lex-string str) (list 'string str))
(define lex-string cadr)

(define lex-char? (lex-predicate 'char))
(define (make-lex-char char) (list 'char char))
(define lex-char cadr)

(define lex-symbol? (lex-predicate 'symbol))
(define (make-lex-symbol sym) (list 'symbol sym))
(define lex-symbol-name cadr)

(define lex-bool? (lex-predicate 'bool))
(define (make-lex-bool b) (list 'bool b))
(define lex-bool-value cadr)

(define *punctuation* '(#\, #\; #\( #\) #\[ #\] #\. #\{ #\}))

(define *lex* #f)

(define (skip-spaces port)
  (let loop ((ch (peek-char port)))
    (if (eof-object? ch)
        'done
        (if (char-whitespace? ch)
            (begin (read-char port) (loop (peek-char port)))))))

(define (read-lex port) 
  (if *lex* 
      (let ((lex *lex*))
	(set! *lex* #f)
	lex)
      (really-read-lex port)))

(define (peek-lex port)
  (or *lex*
      (let ((lex (really-read-lex port)))
	(set! *lex* lex)
	lex)))

(define (really-read-lex port)
  (skip-spaces port)
  (let* ((ch (peek-char port)))
    (cond ((eof-object? ch) ch)
          ((member ch *punctuation*) (read-char port) (make-lex-punctuation ch))
          ((char-numeric? ch) (read-num port))
          ((char-keyword? ch) (read-keyword port))
          ((char=? ch #\") (read-str port))
          ((char=? ch #\') (read-ch port))
          ((char=? ch #\#) (read-sharp port))
          (else (error "unknown token beginning with ~a" ch)))))

(define (read-sharp port)
  (read-char port)
  (let ((next (peek-char port)))
    (cond ((char=? next #\") (make-lex-symbol 
                              (string->symbol (lex-string (read-str port)))))
          ((char=? next #\f) (read-char port) (make-lex-bool #f))
          ((char=? next #\t) (read-char port) (make-lex-bool #t))
          (else (error "unknown # reader ~a" next)))))

(define (read-num port)
  (make-lex-number
   (let loop ((ch (peek-char port))
              (n 0))
     (if (and (char? ch) (char-numeric? ch))
         (let ((ch* (read-char port)))
           (loop (peek-char port) 
                 (+ (* 10 n) (- (char->integer ch) (char->integer #\0)))))
         n))))

(define (read-str port)
  (read-char port)
  (make-lex-string
   (list->string 
    (let loop ((ch (peek-char port)))
      (if (eof-object? ch)
          (error "End of file encountered while reading a string literal")
          (if (char=? ch #\")
              (begin (read-char port) '())
              (let ((ch* (read-char port)))
                (cond ((char=? ch #\\)
                       (let ((next (read-char port)))
                         (if (eof-object? next)
                             (error "End of file while reading string escapes")
                             (cond ((char=? next #\n)
                                    (cons #\newline (loop (peek-char port))))
                                   ((char=? next #\\)
                                    (cons #\\ (loop (peek-char port))))
                                   ((char=? next #\")
                                    (cons #\" (loop (peek-char port))))
                                   (else 
                                    (error "Unknown string escape ~a" next))))))
                      (else
                       (cons ch* (loop (peek-char port))))))))))))

(define (read-ch port)
  (let* ((open (read-char port))
         (char (read-char port))
         (close (read-char port)))
    (if (char=? close #\')
        (make-lex-char char)
        (error "A character syntax needs a closing '" close))))

(define (char-keyword? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (member ch '(#\_ #\~ #\- #\% #\& #\: #\+ #\= #\* #\/ #\< #\> #\? #\!))))
      
(define (skip-comments port)
  (let ((ch (read-char port)))
    (if (and (char? ch) (char=? ch #\newline))
      (really-read-lex port)
      (skip-comments port))))

(define (read-keyword port)
  (let ((first (read-char port))
	(next (peek-char port)))
    (if (and (char=? first #\/) (char=? next #\/))
        (skip-comments port)
        (let ((key (read-name first port)))
          (if (char=? #\: (string-ref key (- (string-length key) 1)))
              (make-lex-symbol 
               (string->symbol (substring key 0 (- (string-length key) 1))))
              (make-lex-keyword key))))))

(define (read-name first port)
  (list->string 
   (cons first 
         (let loop ((ch (peek-char port)))
           (if (and (char? ch) (char-keyword? ch))
               (let ((ch* (read-char port)))
                 (cons ch* (loop (peek-char port))))
               '())))))

;;; TESTS PROCEDURES ==========================================================

(define (test-lex fn)
  (set! *lex* #f)
  (call-with-input-file fn
      (lambda (p) 
        (let loop ((lex (read-lex p)))
          (if (eof-object? lex)
              '()
              (cons lex (loop (read-lex p))))))))

(define (test-lex-2)
  (call-with-input-file "/home/dpa/tmp/essai.dyl"
    (lambda (p)
      (list (peek-lex p) (peek-lex p) (read-lex p) (peek-lex p) (read-lex p)))))

;;; SKELETAL SYNTAX TREE =======================================================

;; Form* ::= Form ...
;; Form  ::= define what EL ... end     Definition
;;           id
;;           literal
;;           ( EL ...)
;;           [ EL ...]
;;           syntax EL ... end          Macro statement
;;           syntax (EL ...)            Macro call
;; EL ::= punctuation | form
;; which gives types:
;; SST ::= Define(what, el) | Name(id) | Immediate(lit) | Nested(kind,el) |
;;         SyntaxStmt(name,el) | SyntaxCall(name,el) | Punctuation(char)

(define (make-define body) (list 'define body))
(define (define? exp) (and (pair? exp) (eq? (car exp) 'define)))
(define (define-body exp) (cadr exp)) 

(define (make-name name) (list 'name name))
(define (name? exp) (and (pair? exp) (eq? (car exp) 'name)))
(define (name-id exp) (cadr exp))
(define (name=? exp str) 
  (and (name? exp) (string=? (lex-keyword-name (name-id exp)) str)))

(define (make-immediate literal) (list 'immediate literal))
(define (immediate? exp) (and (pair? exp) (eq? (car exp) 'immediate)))
(define (immediate-literal exp) (cadr exp))

(define (make-nested open close body) (list 'nested open close body))
(define (nested? exp) (and (pair? exp) (eq? (car exp) 'nested)))
(define (nested-open exp) (cadr exp))
(define (nested-close exp) (caddr exp))
(define (nested-body exp) (cadddr exp))

(define (make-syntax-stmt name body) (list 'syntax-stmt name body))
(define (syntax-stmt? exp) (and (pair? exp) (eq? (car exp) 'syntax-stmt)))
(define (syntax-stmt-name exp) (cadr exp))
(define (syntax-stmt-body exp) (caddr exp))
(define (syntax-stmt=? exp str) 
  (and (syntax-stmt? exp) (lex-keyword=? (syntax-stmt-name exp) str)))

(define (make-syntax-call name body) (list 'syntax-call name body))
(define (syntax-call? exp) (and (pair? exp) (eq? (car exp) 'syntax-call)))
(define (syntax-call-name exp) (cadr exp))
(define (syntax-call-body exp) (caddr exp))

(define (make-punctuation punct) (list 'punct punct))
(define (punctuation? exp) (and (pair? exp) (eq? 'punct (car exp))))
(define (punctuation-char exp) (cadr exp))
(define (punctuation=? exp char) (and (punctuation? exp)
                                      (char=? (lex-punctuation-char (punctuation-char exp))
                                              char)))

;; Syntax environment

(define (make-empty-syntax-env) '())

(define (bind-syntax name kind expander senv)
  (cons (list name kind expander) senv))
    
(define (syntax-env-expander name senv)
  (let ((probe (assoc name senv)))
    (if probe
        (caddr probe)
        (error "unbound syntax" name))))

(define (syntax-env-statement? name senv)
  (let ((probe (assoc name senv)))
    (if probe
        (eq? (cadr probe) 'statement)
        #f)))
        
(define (syntax-env-call? name senv)
  (let ((probe (assoc name senv)))
    (if probe
        (eq? (cadr probe) 'call)
        #f)))

(define (syntax-env-define? name senv)
  (let ((probe (assoc name senv)))
    (if probe
        (eq? (cadr probe) 'define)
        #f)))
        
; FORM* -> EL ...
; FORM ->
;   define EL ... end 
;   ID
;   LITERAL
;   ( EL ... )
;   [ EL ... ] 
;   syntax-stmt EL ... end
;   syntax-call (EL ...) 

; EL -> [ FORM | PUNCT ] 

(define (read-form* senv port)
  (let ((lex (peek-lex port)))
    (cond ((eof-object? lex) lex)
          ((lex-keyword=? lex "define") (read-element senv port))
          (else (read-element* senv port)))))
    
(define (read-element* senv port)
  (let* ((el (read-element senv port))
         (lex (peek-lex port)))
    (cond ((eof-object? lex) (list el))
          ((lex-keyword=? lex "define") (list el))
          (else (cons el (read-element* senv port))))))
    
(define (read-form senv port)
  (let ((lex (peek-lex port)))
    (if (eof-object? lex)
	lex
	(cond ((or (lex-number? lex) (lex-string? lex) (lex-char? lex)
                   (lex-symbol? lex) (lex-bool? lex))
	       (let ((lex* (read-lex port)))
                 (make-immediate lex)))
              ((lex-punctuation=? lex #\() (read-nested lex senv port))
              ((lex-punctuation=? lex #\[) (read-nested lex senv port))
              ((lex-punctuation=? lex #\{) (read-nested lex senv port))
	      ((lex-keyword=? lex "define") (read-define senv port))
	      ((lex-keyword? lex)
               (let* ((name (lex-keyword-name lex)))
                 (cond ((syntax-env-statement? name senv) 
			(read-statement lex senv port))
                       ((syntax-env-call? name senv) 
			(read-call lex senv port))
                       (else (read-lex port) (make-name lex)))))
	      (else (error "Unknown lex ~a" lex))))))

(define (read-element senv port)
  (let ((lex (peek-lex port)))
    (if (or (lex-punctuation=? lex #\;)
            (lex-punctuation=? lex #\,))
        (let ((lex (read-lex port)))
          (make-punctuation lex))
        (read-form senv port))))

(define (nested-close? lex open)
  (and (lex-punctuation? lex)
       (or (and (char=? (lex-punctuation-char lex) #\))
		(char=? (lex-punctuation-char open) #\())
           (and (char=? (lex-punctuation-char lex) #\})
		(char=? (lex-punctuation-char open) #\{))
	   (and (char=? (lex-punctuation-char lex) #\])
		(char=? (lex-punctuation-char open) #\[)))))
               
(define (read-nested open senv port)
  (let ((open (read-lex port)))
    (let loop ((lex (peek-lex port))
               (inbetweens '()))
;      (display lex) (newline)
      (if (eof-object? lex)
	  (error "End of file inside nested" open)
	  (if (nested-close? lex open)
	      (let ((lex (read-lex port)))
		(make-nested open lex (reverse inbetweens)))
	      (let* ((element (read-element senv port)))
					;            (display element) (newline)
		(loop (peek-lex port) (cons element inbetweens))))))))

(define (read-statement lex senv port)
  (let ((lex (read-lex port)))
    (make-syntax-stmt lex (read-body senv port))))
  
(define (read-call lex senv port)
  (let* ((lex (read-lex port))
         (open (peek-lex port)))
    (if (and (lex-punctuation? open) (lex-punctuation=? open #\())
        (make-syntax-call lex (read-nested open senv port))
        (error "expected '(' after syntax call, got" open))))
              
(define (read-body senv port)
  (let ((lex (peek-lex port)))
    (if (eof-object? lex)
	(error "End of file seen inside block")
	(if (lex-keyword=? lex "end")
	    (begin (read-lex port) '())
	    (let ((el (read-element senv port)))
	      (cons el (read-body senv port)))))))
          
(define (read-define senv port)
  (read-lex port)
  (make-define (read-define-body senv port)))

(define (read-define-body senv port)
  (let ((lex (peek-lex port)))
    (cond ((lex-keyword=? lex "syntax-stmt") 
           (cons (make-name lex) (read-syntax-body 'statement senv port)))
          ((lex-keyword=? lex "syntax-call") 
           (cons (make-name lex) (read-syntax-body 'call senv port)))
          ((lex-keyword=? lex "syntax-definer") 
           (cons (make-name lex) (read-syntax-body 'define senv port)))
          (else (read-body senv port)))))

(define (read-syntax-body kind senv port)
  (read-lex port)
  (let ((lex (read-lex port)))
    (if (lex-keyword? lex)
        (cons (make-name lex)
              (read-body (bind-syntax (lex-keyword-name lex) 
                                      kind 
                                      #f
                                      senv)
                         port))
        (error "DEFINE SYNTAX requires a keyword after, got ~a" lex))))

;; TEST =====================================================================

(define (test-form fn)
  (set! *lex* #f)
  (call-with-input-file fn
      (lambda (p) 
	(read-form* (make-empty-syntax-env) p))))

(define (test-form-1) (test-form "/home/dpa/tmp/essai.dyl"))

(define (t f p)
  (set! *lex* #f)
  (call-with-input-file f p))

;;; PATTERN MATCHING & TEMPLATES ==================================================

;; PATTERN is define(body), name(id), immediate(literal), nested(open,close,body), 
;; syntax-call(name, body), punctuation(char)

(define (pattern-match f p env rt-env)
  (cond ((define? p)
         (if (define? f)
             (pattern-match (define-body f) (define-body p) env rt-env)
             #f))
        ((name? p)
         (if (and (name? f)
                  (string=? (lex-keyword-name (name-id f)) 
                            (lex-keyword-name (name-id p))))
             env
             #f))
        ((immediate? p)
         (if (and (immediate? f)
                  (equal? (immediate-literal f) (immediate-literal p)))
             env
             #f))
        ((nested? p)
         (if (and (nested? f)
                  (char=? (lex-punctuation-char (nested-open f)) 
                          (lex-punctuation-char (nested-open p)))
                  (char=? (lex-punctuation-char (nested-close f))
                          (lex-punctuation-char (nested-close p))))
             (pattern-match (nested-body f) (nested-body p) env rt-env)
             #f))
        ((syntax-call? p)
         (if (and (syntax-call? f)
                  (string=? (lex-keyword-name (syntax-call-name f)) 
                            (lex-keyword-name (syntax-call-name p))))
             (pattern-match (syntax-call-body f) (syntax-call-body p) env rt-env)
             #f))
        ((syntax-stmt? p)
         (if (and (syntax-stmt? f)
                  (string=? (lex-keyword-name (syntax-stmt-name f))
                            (lex-keyword-name (syntax-stmt-name p))))
             (pattern-match (syntax-stmt-body f) (syntax-stmt-body p) env rt-env)
             #f))
        ((punctuation? p)
         (if (and (punctuation? f)
                  (char=? (lex-punctuation-char (punctuation-char f))
                          (lex-punctuation-char (punctuation-char p))))
             env
             #f))
        ((and (pair? p) 
              (name? (car p)) 
              (var? (lex-keyword-name (name-id (car p)))))
         (let* ((type (var-type (lex-keyword-name (name-id (car p)))))
                (expander (assoc type *pattern-types*)))
           (if expander
               ((cdr expander) f p env rt-env)
               (default-expander f p env rt-env))))
        ((pair? p)
         (if (pair? f)
             (let ((env* (pattern-match (car f) (car p) env rt-env)))
               (if env*
                   (pattern-match (cdr f) (cdr p) env* rt-env)
                   #f))
             #f))
        ((null? p) (if (null? f) env #f))
        (else (error "Unknown pattern expression: ~a" p))))

;; Variables: ?toto:* ?toto ?toto:expression ?toto:name

(define *pattern-types* '())
(define (bind-pattern-type! type proc)
  (let ((probe (assoc type *pattern-types*)))
    (if probe
        (set-cdr! probe proc)
        (set! *pattern-types* (cons (cons type proc) *pattern-types*)))))

(bind-pattern-type! ""
  (lambda (f p env rt-env)
    (let ((env* (cons (cons (var-name (lex-keyword-name (name-id (car p))))
                            (list (car f)))
                      env)))
      (pattern-match (cdr f) (cdr p) env* rt-env))))

(bind-pattern-type! "*"
  (lambda (f p env rt-env)
    (let loop ((f '()) (rest f))
      (let ((env* (cons (cons (var-name (lex-keyword-name (name-id (car p)))) f) env)))
        (or (pattern-match rest (cdr p) env* rt-env)
            (if (null? rest)
                #f
                (loop (append f (list (car rest))) (cdr rest))))))))

(define (default-expander f p env rt-env)
  (let* ((var (lex-keyword-name (name-id (car p))))
         (name (var-name var))
         (type (var-type var)))
    (let ((expander (assq (string->symbol type) rt-env)))
      (if expander
          ((pattern-constraint (cdr expander)) f p env rt-env)
          (error "unknown variable type ~a in ~a" type name)))))

(define (pattern-constraint pattern?)
  (lambda (f p env rt-env)
    (let loop ((f '()) (rest f))
      (let ((env* (cons (cons (var-name (lex-keyword-name (name-id (car p)))) f)
                        env)))
        (or (and (pattern? f) (pattern-match rest (cdr p) env* rt-env))
            (if (null? rest)
                #f
                (loop (append f (list (car rest))) (cdr rest))))))))
  
(bind-pattern-type! "name" (pattern-constraint 
                            (lambda (f) (and (pair? f) (name? (car f))))))

(define (var? str) (char=? (string-ref str 0) #\?))

(define (var-name str)
  (let loop ((i 1))
    (if (< i (string-length str))
        (if (char=? #\: (string-ref str i))
            (substring str 1 i)
            (loop (+ i 1)))
        (substring str 1 i))))

(define (var-type str)
  (let loop ((i 1))
    (if (< i (string-length str))
        (if (char=? #\: (string-ref str i))
            (substring str (+ i 1) (string-length str))
            (loop (+ i 1)))
        "")))

;;; SYNTAX EXPANSION =============================================================

(define *syntaxes* (make-empty-syntax-env))

(define (bind-syntax-call! name expander)
  (set! *syntaxes* 
	(bind-syntax name 'call expander *syntaxes*)))

(define (bind-syntax-stmt! name expander)
  (set! *syntaxes* 
	(bind-syntax name 'statement expander *syntaxes*)))

(define (bind-syntax-define! name expander)
  (set! *syntaxes* 
	(bind-syntax name 'define expander *syntaxes*)))

(bind-syntax-call! "meaning-of-life"
		   (lambda (f e) (list (make-immediate (make-lex-number 42)))))
(bind-syntax-call! "sens-de-la-vie" 
                   (lambda (f e) 
                     (list (make-name (make-lex-keyword "meaning-of-life")))))
(bind-syntax-stmt! "if"
                   (lambda (f e)
;		     (display "expanding body =>") (display (syntax-stmt-body f)) 
		     (newline)
		     (list (make-syntax-stmt (syntax-stmt-name f)
					     (expand-syntax* (syntax-stmt-body f) e)))))

(bind-syntax-stmt! "lambda" 
                   (lambda (f e)
                     (list (make-syntax-stmt (syntax-stmt-name f)
                                             (expand-syntax* (syntax-stmt-body f) e)))))

(bind-syntax-stmt! "syntax-case"
                   (lambda (f e)
                     (list (make-syntax-stmt (syntax-stmt-name f)
                                             (expand-syntax* (syntax-stmt-body f) e)))))
(bind-syntax-define! "syntax-stmt" 
                     (lambda (f e)
                       (let* ((body (define-body f))
                              (syn-stmt (car body))
                              (name (cadr body))
                              (args (caddr body)))
                         (list (make-define (append (list syn-stmt name args)
                                                    (expand-syntax* (cdddr body) e)))))))

(bind-syntax-define! "syntax-call" 
                     (lambda (f e)
                       (let* ((body (define-body f))
                              (syn-stmt (car body))
                              (name (cadr body))
                              (args (caddr body)))
                         (list (make-define (append (list syn-stmt name args)
                                                    (expand-syntax* (cdddr body) e)))))))

(bind-syntax-define! "syntax-definer"
                     (lambda (f e)
                       (let* ((body (define-body f))
                              (syn-stmt (car body))
                              (name (cadr body))
                              (args (caddr body)))
                         (list (make-define (append (list syn-stmt name args)
                                                    (expand-syntax* (cdddr body) e)))))))

(bind-syntax-define! "syntax-rules" (lambda (f e) (list f)))

(define (expand-syntax-stmt f senv)
  (let ((name (lex-keyword-name (syntax-stmt-name f))))
    (if (syntax-env-statement? name senv)
        (let ((expander (syntax-env-expander name senv)))
          (expander f senv))
	(error "syntax is not a syntax statement even though it has been read so" 
	       f senv))))

(define (expand-basic-define f senv)
  (let* ((body (define-body f))
         (name (car body))
         (params (cadr body))
         (rest (cddr body)))
    (list (make-define (cons name (cons params (expand-syntax* rest senv)))))))

(define (expand-syntax-define f senv)
  (let* ((body (define-body f))
         (head (if (null? body) (error "body is empty!") (car body))))
    (if (name? head)
        (let ((name (name-id head)))
          (if (syntax-env-define? name senv)
              (let ((expander (syntax-env-expander name senv)))
                (expand-syntax* (expander f senv) senv))
              (expand-basic-define f senv)))
        (error "a name should appear after a definition" f))))
    
(define (expand-syntax-call f senv)
  (let ((name (lex-keyword-name (syntax-call-name f))))
    (if (syntax-env-call? name senv)
	(let ((expander (syntax-env-expander name senv)))
	  (expand-syntax* (expander f senv) senv))
	(error "syntax is not a syntax call even though it has been read so" 
	       f senv))))
  
(define (expand-nested f senv)
  (if (char=? (lex-punctuation-char (nested-open f)) #\{)
      (list f)
      (list (make-nested (nested-open f)
                         (nested-close f)
                         (expand-syntax* (nested-body f) senv)))))

(define (expand-syntax f senv)
  (cond ((immediate? f) (list f))
	((define? f) (expand-syntax-define f senv))
	((name? f) (list f))
	((nested? f) (expand-nested f senv))
	((punctuation? f) (list f))
	((syntax-stmt? f) (expand-syntax-stmt f senv))
	((syntax-call? f) (expand-syntax-call f senv))
	(else (error "unknown fragment type ~a" f))))

(define (expand-syntax* fs senv)
  (if (null? fs)
      fs
      (append (expand-syntax (car fs) senv)
              (expand-syntax* (cdr fs) senv))))

(define (test-syn-1)
  (let ((form (call-with-input-file "/home/dpa/tmp/essai.dyl"
		(lambda (p) 
		  (read-form *syntaxes* p)))))
    (display "input:") (newline)
    (display form) (newline)
    (expand-syntax form *syntaxes*)))

;;; TRANSLATION ===================================================================

;; form        ::= DEFINE ID (ID ...) body END 
;;                 exp ;
;; body        ::= exp ; body
;;                 exp ;
;; exp         ::= LET ID = exp ; body
;;                 LETREC ID = exp ; body
;;                 syntax-case
;;                 IF (exp) body ELSE body END
;;                 exp := exp
;;                 bool-exp
;; syntax-case ::= SYNTAX-CASE (exp) cases END
;; cases       ::= FRAGMENTS => body ; cases
;;                 FRAGMENTS => body ;
;; bool-exp    ::= rel-exp AND bool-exp
;;                 rel-exp OR bool-exp
;; rel-exp     ::= term < term
;;                 term <= term
;;                 term = term
;;                 term == term
;;                 term ~= term
;;                 term ~== term
;;                 term > term
;;                 term >= term
;;          
;; term        ::= factor + term
;;                 factor - term
;; factor      ::= path * factor
;;                 path / factor
;;                 path MOD factor
;;                 path DIV factor
;; path        ::= path [ exp , exp ...]
;;                 path ( exp , exp ...)
;;                 path . NAME
;;                 basic
;; basic       ::= IMMEDIATE
;;                 LAMBDA (ID , ID ...) body END
;;                 NAME
;;                 ( exp )
;;                 FRAGMENTS
;;                 ~ basic
;;                 - basic

(define (popt p1 p2)
  (lambda (fs k)
    (p1 fs (lambda (v1 rest)
             (if v1
                 (p2 rest 
                     (lambda (v2 rest*) 
                       (if v2 
                           (k (cons v1 v2) rest*) 
                           (k (cons v1 v2) rest))))
                 (k #f fs))))))

(define (por . ps)
  (lambda (fs k)
    (if (null? ps)
        (error "bad POR expression")
        ((car ps) fs (lambda (v rest)
                       (if v
                           (k v rest)
                           (if (null? (cdr ps))
                               (k #f fs)
                               ((apply por (cdr ps)) fs k))))))))

(define (pand . ps)
  (lambda (fs k)
    (if (null? ps)
        (error "bad PAND expression")
        ((car ps) fs (lambda (v1 rest)
             (if v1
                 (if (null? (cdr ps))
                     (k (list v1) rest)
                     ((apply pand (cdr ps))
                      rest 
                      (lambda (v2 rest*) 
                        (if v2
                            (k (cons v1 v2) rest*)
                            (k #f  fs)))))                    
                 (k #f fs)))))))

(define (pnest char p)
  (lambda (fs k)
    (if (null? fs)
        (k #f fs)
        (let ((h (car fs)))
          (if (and (nested? h) (lex-punctuation=? (nested-open h) char))
              (p (nested-body h) 
                 (lambda (v1 rest)
                   (if (and (null? rest) v1)
                       (k v1 (cdr fs))
                       (k #f fs))))
              (k #f fs))))))

(define (pstmt str p)
  (lambda (fs k)
    (if (null? fs)
        (k #f fs)
        (let ((h (car fs)))
          (if (and (syntax-stmt? h) (syntax-stmt=? h str))
              (p (syntax-stmt-body h) 
                 (lambda (v1 rest)
                   (if (and (null? rest) v1)
                       (k v1 (cdr fs))
                       (k #f fs))))
              (k #f fs))))))

(define (pdef p)
  (lambda (fs k)
    (if (null? fs)
        (k #f fs)
        (let ((h (car fs)))
          (if (define? h)
              (p (define-body h) 
                 (lambda (v1 rest)
                   (if (and (null? rest) v1)
                       (k v1 (cdr fs))
                       (k #f fs))))
              (k #f fs))))))

(define (perror msg)
  (lambda (fs k)
    (error msg fs)))

(define pname
  (lambda (fs k)
    (if (and (pair? fs)
             (name? (car fs)))
        (k (string->symbol (lex-keyword-name (name-id (car fs)))) (cdr fs))
        (k #f fs))))

(define (pkey key)
  (lambda (fs k)
    (if (and (pair? fs)
             (name=? (car fs) key))
        (k #t (cdr fs))
        (k #f fs))))

(define (ppunct char)
  (lambda (fs k)
    (if (and (pair? fs)
             (punctuation=? (car fs) char))
        (k #t (cdr fs))
        (k #f fs))))

(define (pbetween p sep)
  (lambda (fs k)
    (p fs (lambda (v1 rest)
            (if v1
                (sep rest (lambda (ign rest*)
                            (if ign
                                ((pbetween p sep) rest*
                                 (lambda (v2 rest*)
                                   (if v2
                                       (k (cons v1 v2) rest*)
                                       (k (list v1) rest*))))
                                (k (list v1) rest*))))
                (k #f fs))))))

(define (ptrans p f)
  (lambda (fs k)
    (p fs (lambda (v* rest)
            (if v*
                (k (f v*) rest)
                (k #f fs))))))

(define (ptrans* p f)
  (lambda (fs k)
    (p fs (lambda (v* rest)
            (if v*
                (k (apply f v*) rest)
                (k #f fs))))))

(define *debug-level* 0)

(define (pdebug msg p)
  (lambda (fs k)
    (newline) (display *debug-level*) (display ">trying ") (display msg) (display fs)
    (set! *debug-level* (+ *debug-level* 1))
    (p fs (lambda (v rest)
            (if v
                (begin (set! *debug-level* (- *debug-level* 1))
                       (display " successfully") (display *debug-level*)
                       (newline) 
                       (k v rest))
                (begin (set! *debug-level* (- *debug-level* 1))
                       (display " no success:-") (display *debug-level*)
                       (display rest) (newline) 
                       (k v fs)))))))

(define (immediate->scheme f)
  (let ((lex (immediate-literal f)))
    (cond ((lex-number? lex) (lex-number lex))
          ((lex-string? lex) (lex-string lex))
          ((lex-char? lex) (lex-char lex))
          ((lex-symbol? lex) (list 'quote (lex-symbol-name lex)))
          ((lex-bool? lex) (list 'quote (lex-bool-value lex)))
          (else (error "unknown immediate type ~a" lex)))))

(define pimm
  (lambda (fs k)
    (if (and (pair? fs) (immediate? (car fs)))
        (k (immediate->scheme (car fs)) (cdr fs))
        (k #f fs))))
    
(define let-syn
  (lambda (fs k)
    ((ptrans* (pand (pkey "let") pname (pkey "=") exp-syn (ppunct #\;) body-syn)
              (lambda (i1 name i2 expr i3 body)
                `(let (,name ,expr) ,body)))
     fs k)))

(define letrec-syn
  (lambda (fs k)
    ((ptrans* (pand (pkey "letrec") pname (pkey "=") exp-syn (ppunct #\;) body-syn)
              (lambda (i1 name i2 expr i3 body)
                `(letrec (,name ,expr) ,body)))
     fs k)))

(define body-syn
  (lambda (fs k)
    ((ptrans (pbetween exp-syn (ppunct #\;))
             (lambda (exps) (cons 'begin exps)))
     fs k)))

(define if-syn
  (lambda (fs k)
    ((por (ptrans* (pstmt "if" (pand (pnest #\( exp-syn) body-syn (pkey "else") body-syn))
                   (lambda (exp con else* alt) (list 'if exp con alt)))
          (ptrans* (pstmt "if" (pand (pnest #\( exp-syn) body-syn))
                   (lambda (exp con) (list 'if exp con #f))))
     fs k)))

(define syntax-case-syn
  (lambda (fs k)
    ((ptrans* (pstmt "syntax-case" (pand (pnest #\( exp-syn) syntax-case-cases))
              (lambda (e cases) (list 'syntax-case e cases)))
     fs k)))

(define syntax-case-cases
  (lambda (fs k)
    ((por (ptrans* (pand syntax-case-case syntax-case-cases)
                   (lambda (one two) (cons one two)))
          (ptrans syntax-case-case (lambda (v) (list v))))
     fs k)))

(define syntax-case-case
  (lambda (fs k)
    ((ptrans* (pand (pnest #\{ as-is*) (pkey "=>") body-syn)
              (lambda (left => right)
                (cons left right)))
     fs k)))
                     
(define define-syn
  (lambda (fs k)  
    ((por (ptrans* (pdef (pand pname (pkey "=") exp-syn))
                   (lambda (name equal exp) (list 'define name exp)))
          (ptrans* (pdef (pand (pkey "syntax-stmt") pname
                               (pnest #\( (pbetween pname (ppunct #\,)))
                               body-syn))
                   (lambda (stmt name args exp)
                     (list 'syntax-stmt (symbol->string name) args exp)))
          (ptrans* (pdef (pand (pkey "syntax-call") pname
                               (pnest #\( (pbetween pname (ppunct #\,)))
                               body-syn))
                   (lambda (stmt name args exp)
                     (list 'syntax-call (symbol->string name) args exp)))
          (ptrans* (pdef (pand (pkey "syntax-definer") pname
                               (pnest #\( (pbetween pname (ppunct #\,)))
                               body-syn))
                   (lambda (stmt name args exp)
                     (list 'syntax-definer (symbol->string name) args exp)))
          (ptrans* (pdef (pand pname (pnest #\( (pbetween pname (ppunct #\,))) 
                               body-syn))
                   (lambda (name args exp) (list 'define name (list 'lambda args exp)))))
     fs k)))

(define exp-syn
  (lambda (fs k)
    ((por define-syn
          if-syn
          let-syn
          letrec-syn
          syntax-case-syn
          assn-syn
          (perror "unknown expression starting with ~a")) 
     fs k)))

(define assn-syn
  (lambda (fs k)
    ((por (ptrans* (pand bool-syn (pkey ":=") bool-syn)
                   (lambda (left := right)
                     (cond ((and (pair? left) (eq? (car left) 'aref))
                            (list 'aset! (cadr left) (caddr left) right))
                           ((and (pair? left) (eq? (car left) 'slot-ref))
                            (list 'slot-set! (cadr left)
                                  (caddr left) right))
                           ((symbol? left) (list 'set! left right))
                           (else (error "invalid assignement expression: ~a ~a" left right)))))
          bool-syn)
     fs k)))

(define bool-syn
  (lambda (fs k)
    ((por (ptrans* (pand rel-syn bool-op bool-syn)
                   (lambda (left op right) (list op left right)))
          rel-syn)
     fs k)))

(define bool-op
  (por (ptrans (pkey "&") (lambda (_) 'and))
       (ptrans (pkey "|") (lambda (_) 'or))))

(define rel-syn
  (lambda (fs k)
    ((ptrans (popt term-syn (pand rel-op term-syn))
             (lambda (parse)
               (let ((left (car parse))
                     (op? (cdr parse)))
                 (if op?
                     (list (car op?) left (cadr op?))
                     left))))
     fs k)))

(define rel-op
  (por (ptrans (pkey "<") (lambda _ '<)) (ptrans (pkey "<=") (lambda _ '<=))
       (ptrans (pkey "=") (lambda _ '=)) (ptrans (pkey "~=") (lambda _ '~=))
       (ptrans (pkey "==") (lambda _ '==)) (ptrans (pkey "~==") (lambda _ '~==))
       (ptrans (pkey ">") (lambda _ '>)) (ptrans (pkey ">=") (lambda _ '>=))))

(define term-syn
  (lambda (fs k)
    ((ptrans (popt factor-syn (pand plus-minus-op term-syn))
             (lambda (parse)
               (let ((left (car parse))
                     (op? (cdr parse)))
                 (if op?
                     (list (car op?) left (cadr op?))
                     left))))
     fs k)))

(define plus-minus-op 
  (por (ptrans (pkey "+") (lambda (_) '+))
       (ptrans (pkey "-") (lambda (_) '-))))

(define factor-syn
  (lambda (fs k)
    ((ptrans (popt path-syn (pand mult-div-op factor-syn))
             (lambda (parse) 
               (let ((left (car parse))
                     (op? (cdr parse)))
                 (if op?
                     (list (car op?) left (cadr op?))
                     left))))
     fs k)))

(define mult-div-op 
  (por (ptrans (pkey "*") (lambda (_) '*))
       (ptrans (pkey "/") (lambda (_) '/))
       (ptrans (pkey "mod") (lambda (_) 'modulo))
       (ptrans (pkey "div") (lambda (_) 'div))))


(define path-syn
  (lambda (fs k)
    (basic-syn fs 
               (lambda (val fs*)
                 (if val
                     (let loop ((val val)
                                (fs* fs*))
                       (if (and val (pair? fs*))
                           ((por (ptrans (pnest #\[ (pbetween exp-syn (ppunct #\,)))
                                         (lambda (exps) `(aref ,val ,@exps)))
                                 (ptrans (pnest #\( (pbetween exp-syn (ppunct #\,)))
                                         (lambda (exps) (cons val exps))))
                            fs* (lambda (new fs*) 
                                  (if new
                                      (loop new fs*)
                                      (k val fs*))))
                           (k val fs*)))
                     (k #f fs))))))

(define basic-syn
  (lambda (fs k)
    ((por pimm
          (ptrans* (pstmt "lambda" (pand (pnest #\( (pbetween pname (ppunct #\,))) body-syn))
                   (lambda (args body) `(lambda ,args ,body)))
          (ptrans* (pand pname (pnest #\( (pbetween exp-syn (ppunct #\,))))
                   (lambda (name args) `(,name ,@args)))          
          (pnest #\( exp-syn)
          (ptrans (pnest #\{ as-is*) (lambda (v) (list 'fragments v)))
          (ptrans* (pand (pkey "~") basic-syn)
                   (lambda (not exp) (list '~ exp)))
          (ptrans* (pand (pkey "-") basic-syn)
                   (lambda (min exp) (list '- exp)))
          pname)
     fs k)))

(define as-is*
  (lambda (fs k) (k fs '())))

(define (form-syn fs) (body-syn fs (lambda (v rest) v)))

;   (if (null? fs)
;       '()
;       (expression->scheme fs 
;                           (lambda (head rest) 
;                             (cons head (fragments->scheme rest))))))

(define (test-parsing)
  (let ((fs '((name (keyword "hello")) (name (keyword "world")))))
    ((pand (pkey "hello")
           pname) fs list)))

(define (test-parsing) 
  (let ((fs 
         '((immediate (num 1)) (name (keyword "+")) (immediate (num 2)))))
    (body-syn fs list)))

;;; INTERPRETER ==================================================================

(define *global* '())
(define (bind-env name value env) (cons (cons name value) env))
(define (bind-global! name value) 
  (let ((probe (assq name *global*)))
    (if probe
        (set-cdr! probe value)
        (set! *global* (bind-env name value *global*)))))
(define (is? form exp) (and (pair? exp) (eq? (car exp) form)))

(define (ev exp env)
  (cond ((is? 'if exp) 
         (let ((t (ev (cadr exp) env))) 
           (if t (ev (caddr exp) env) (ev (cadddr exp) env))))
        ((is? 'define exp)
         (let ((var (cadr exp))
               (val (ev (caddr exp) env)))
           (bind-global! var val)
           val))
        ((is? 'let exp) (let* ((binding (cadr exp))
                               (name (car binding))
                               (value (ev (cadr binding) env)))
                          (ev (caddr exp) (bind-env name value env))))
        ((is? 'letrec exp) (let* ((binding (cadr exp))
                                  (name (car binding))
                                  (env* (bind-env name #f env))
                                  (value (ev (cadr binding) env*)))
                             (set-cdr! (car env*) value)
                             (ev (caddr exp) env*)))
        ((is? 'begin exp) (ev* (cdr exp) env))
        ((is? 'lambda exp)
         (lambda args
;           (display "eval: with ") (display args) 
;           (display " in ") (display env) (newline)
           (if (= (length args) (length (cadr exp)))
               (ev (caddr exp) (append (map cons (cadr exp) args) env))
               (error "arity error in calling ~a with ~a" exp args))))
        ((is? 'set! exp)
         (let ((binding (assq (cadr exp) env)))
           (if binding
               (set-cdr! binding (ev (caddr exp) env))
               (bind-global! (cadr exp) (ev (caddr exp) env)))))
        ((is? 'quote exp) (cadr exp))
        ((is? 'fragments exp) (ev-fragments (cadr exp) env))
        ((is? 'syntax-call exp)
         (bind-syntax-call! (cadr exp) (lambda (f e)
                                         (ev (cadddr exp) (bind-env 'e (list f) env)))))
        ((is? 'syntax-stmt exp)
         (bind-syntax-stmt! (cadr exp) (lambda (f e)
                                         (ev (cadddr exp) (bind-env 'e (list f) env)))))
        ((is? 'syntax-definer exp)
         (bind-syntax-call! (cadr exp) (lambda (f e)
                                         (ev (cadddr exp) (bind-env 'e (list f) env)))))
        ((is? 'syntax-case exp)
         (ev-syntax-case (ev (cadr exp) env) (caddr exp) env))
        ((pair? exp) 
         (let ((es (map (lambda (e) (ev e env)) exp)))
           (apply (car es) (cdr es))))
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
             (let ((val (assq (string->symbol (var-name (lex-keyword-name (name-id f))))
                              env)))
               (if val
                   (cdr val)
                   (error "unknown fragment binding ~a in ~a" f env)))
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
;  (display "syn-case:") (display val) (display cases) (newline)
  (if (null? cases)
      (error "syntax-case fallthrough ~a" val)
      (let* ((rule (car cases))
             (pattern (car rule))
             (expression (cdr rule)))
        (let ((binding (pattern-match val pattern '() env)))
          (if binding
              (ev expression (append (map (lambda (b) (cons (string->symbol (car b))
                                                            (cdr b))) binding) 
                                     env))
              (ev-syntax-case val (cdr cases) env))))))

(define (prim name impl)
  (bind-global! name impl))

(prim '+ +)
(prim '- -)
(prim '* *)
(prim 'div quotient)
(prim 'mod modulo)
(prim '/ /)
(prim '< <) (prim '<= <=)
(prim '= =) (prim '~= (lambda (a b) (not (= a b))))
(prim '== eq?) (prim '~== (lambda (a b) (not (eq? a b))))
(prim '> >) (prim '>= >=)
(prim '~ not)
(prim 'number? number?)
(prim 'pair? pair?) (prim 'make-pair cons) (prim 'head car) (prim 'tail cdr)
(prim 'list list)
(prim 'vector? vector?) (prim 'make-vector make-vector)
  (prim 'aref vector-ref) (prim 'aset! vector-set!) 
  (prim 'vector-length vector-length)
(prim 'string? string?) (prim 'make-string make-string) 
  (prim 'string-ref string-ref) (prim 'string-set! string-set!)
  (prim 'string-length string-length)
  
(prim 'read-char read-char) (prim 'write-char write-char)
(prim 'display display)

(prim 'open-input-file open-input-file) 
(prim 'open-output-file open-output-file)

(prim 'close-input-port close-input-port) 
(prim 'close-output-port close-output-port)

;(prim '%imm %imm)
;(prim '%type %type)
;(prim '%alloc alloc) (prim '%ref %ref) (prim '%set! %set!) 
;(prim '%length %length)

;;; DYLAN REPL ================================================================

(define (dylan fn)
  (set! *lex* #f)
  (let* ((form (call-with-input-file fn 
                 (lambda (port) 
                   (read-form *syntaxes* port)))))
    (display "read: ") (display form) (newline)
    (let ((expanded (expand-syntax form *syntaxes*)))
      (display "expa: ") (display expanded) (newline)
      (let ((translated (form-syn expanded)))
        translated))))

(define (dylan* fn)
  (set! *lex* #f)
  (let* ((form (call-with-input-file fn 
                 (lambda (port) 
                   (read-form* *syntaxes* port)))))
    (display "read: ") (display form) (newline)
    (let ((expanded (expand-syntax* form *syntaxes*)))
      (display "expa: ") (display expanded) (newline)
      (let ((translated (form-syn expanded)))
        (display "trans: ") (display translated) (newline)
        (map (lambda (e) (ev e *global*)) translated)))))
    
(define (melon fn)  
  (set! *lex* #f)
  (call-with-input-file fn
    (lambda (port)
      (let loop ((form (read-form* *syntaxes* port)))
        (format #t "eval: ~a~%" form) 
        (if (not (null? form))
            (let ((expanded (expand-syntax* form *syntaxes*)))
              (let ((translated (form-syn expanded)))
                (display "executing:") (display translated)
                (ev translated *global*)
                (loop (read-form* *syntaxes* port)))))))))

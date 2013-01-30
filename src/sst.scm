;;; sst.scm -- skeletal syntax tree procedures

;;; SKELETAL SYNTAX TREE =======================================================

;; Form* ::= Form ...
;; Form  ::= def what EL ... end     Definition
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
          ((lex-keyword=? lex "def") (list (read-element senv port)))
          (else (read-element* senv port)))))

; (define (read-element* senv port)
;   (let* ((el (read-element senv port))
;          (lex (peek-lex port)))
;     (cond ((eof-object? lex) (list el))
;           ((lex-keyword=? lex "def") (list el))
;           (else (cons el (read-element* senv port))))))

(define (read-element* senv port)
  (let* ((lex (read-element senv port)))
    (cond ((eof-object? lex) '())
          ((punctuation=? lex #\;) '())
          (else (cons lex (read-element* senv port))))))

(set! read-form* read-element*)

(define (read-form senv port)
  (let ((lex (peek-lex port)))
    (if (eof-object? lex)
	lex
	(cond ((or (lex-number? lex) (lex-string? lex) (lex-char? lex)
                   (lex-symbol? lex) (lex-bool? lex))
	       (let ((lex* (read-lex port)))
                 (make-immediate lex)))
	      ((lex-punctuation=? lex #\.) (let ((lex* (read-lex port))) (make-punctuation lex)))
              ((lex-punctuation=? lex #\() (read-nested lex senv port))
              ((lex-punctuation=? lex #\[) (read-nested lex senv port))
              ((lex-punctuation=? lex #\{) (read-nested lex senv port))
	      ((lex-keyword=? lex "def") (read-define senv port))
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
    (cond ((lex-keyword=? lex "macro") 
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


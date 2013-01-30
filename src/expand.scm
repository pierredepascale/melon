;;; expand.scm -- syntax expansion for Melon

;;; PATTERN MATCHING & TEMPLATES ==================================================

;; PATTERN is define(body), name(id), immediate(literal), nested(open,close,body), 
;; syntax-call(name, body), punctuation(char)

(define (fail f p)
  (write (list 'pattern-failed f p)) (newline)
  #f)

(define (pattern-match f p env rt-env)
  (cond ((define? p)
         (if (define? f)
             (pattern-match (define-body f) (define-body p) env rt-env)
             (fail f p)))
        ((name? p)
         (if (and (name? f)
                  (string=? (lex-keyword-name (name-id f)) 
                            (lex-keyword-name (name-id p))))
             env
             (fail f p)))
        ((immediate? p)
         (if (and (immediate? f)
                  (equal? (immediate-literal f) (immediate-literal p)))
             env
             (fail f p)))
        ((nested? p)
         (if (and (nested? f)
                  (char=? (lex-punctuation-char (nested-open f)) 
                          (lex-punctuation-char (nested-open p)))
                  (char=? (lex-punctuation-char (nested-close f))
                          (lex-punctuation-char (nested-close p))))
             (pattern-match (nested-body f) (nested-body p) env rt-env)
             (fail f p)))
        ((syntax-call? p)
         (if (and (syntax-call? f)
                  (string=? (lex-keyword-name (syntax-call-name f)) 
                            (lex-keyword-name (syntax-call-name p))))
             (pattern-match (syntax-call-body f) (syntax-call-body p) env rt-env)
             (fail f p)))
        ((syntax-stmt? p)
         (if (and (syntax-stmt? f)
                  (string=? (lex-keyword-name (syntax-stmt-name f))
                            (lex-keyword-name (syntax-stmt-name p))))
             (pattern-match (syntax-stmt-body f) (syntax-stmt-body p) env rt-env)
             (fail f p)))
        ((punctuation? p)
         (if (and (punctuation? f)
                  (char=? (lex-punctuation-char (punctuation-char f))
                          (lex-punctuation-char (punctuation-char p))))
             env
             (fail f p)))
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
                   (fail f p)))
             (fail f p)))
        ((null? p) (if (null? f) env (fail f p)))
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

(bind-pattern-type! "body" (pattern-constraint 
                            (lambda (f) 
                              (body-syn f 
                                        (lambda (r fs) 
                                          (and r (null? fs)))))))
(bind-pattern-type! "expression" (pattern-constraint 
                                  (lambda (f) 
                                    (exp-syn f 
                                             (lambda (r fs) 
                                               (and r (null? fs)))))))

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

(bind-syntax-stmt! "if"
                   (lambda (f e)
;		     (display "expanding body =>") (display (syntax-stmt-body f)) 
		     (newline)
		     (list (make-syntax-stmt (syntax-stmt-name f)
					     (expand-syntax* (syntax-stmt-body f) e)))))

(bind-syntax-stmt! "fn" 
                   (lambda (f e)
                     (list (make-syntax-stmt (syntax-stmt-name f)
                                             (expand-syntax* (syntax-stmt-body f) e)))))

(bind-syntax-stmt! "syntax-case"
                   (lambda (f e)
                     (list (make-syntax-stmt (syntax-stmt-name f)
                                             (expand-syntax* (syntax-stmt-body f) e)))))
(bind-syntax-define! "macro" 
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
  (display "expanding basic define")
  (let* ((body (define-body f))
         (name (car body))
         (params (cadr body))
         (rest (cddr body)))
    (list (make-define (cons name (cons params (expand-syntax* rest senv)))))))

(define (expand-syntax-define f senv)
  (let* ((body (define-body f))
         (head (if (null? body) (error "body is empty!") (car body))))
    (if (name? head)
        (let ((name (lex-keyword-name (name-id head))))
          (if (syntax-env-define? name senv)
              (let ((expander (syntax-env-expander name senv)))
                (expander f senv))
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


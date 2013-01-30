;;; parsing.scm -- parser for the Melon language

;;; TRANSLATION ===================================================================

;; form        ::= DEF ID (ID ...) body END 
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
;;                 FN (ID , ID ...) body END
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
                                       (k (list v1) rest))))
                                (k (list v1) rest))))
                (k #f fs))))))

(define (pbetween* p sep)
  (lambda (fs k)
    (p fs (lambda (v1 rest)
            (if v1
                (sep rest (lambda (ign rest*)
                            (if ign
                                ((pbetween p sep) rest*
                                 (lambda (v2 rest*)
                                   (if v2
                                       (k (cons v1 v2) rest*)
                                       (k (list v1) rest))))
                                (k (list v1) rest))))
                (k '() fs))))))

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

(define pempty
  (lambda (fs k)
    (k '() fs)))

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

(define pident
  (lambda (fs k)
    ((ptrans* (pand pname (pkey "::") exp-syn) (lambda (name __ key) (list name key)))
     fs k)))

(define let-syn
  (lambda (fs k)
    ((ptrans* (pand (pkey "let") pname (pkey "=") exp-syn (ppunct #\;) body-syn)
              (lambda (i1 name i2 expr i3 body)
                `(let (,name ,expr) ,body)))
     fs k)))

(define letrec-syn
  (lambda (fs k)
    ((ptrans* (pand (pkey "letrec") pname (pkey "=") exp-syn (pkey "in") body-syn)
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
    ((por (ptrans* (pand syntax-case-case (ppunct #\;) syntax-case-cases)
                   (lambda (one _ two) (cons one two)))
          (ptrans syntax-case-case (lambda (v) (list v))))
     fs k)))

(define syntax-case-case
  (lambda (fs k)
    ((ptrans* (pand (pnest #\{ as-is*) (pkey "=>") exp-syn)
              (lambda (left to right)
                (cons left right)))
     fs k)))
                     
(define define-syn
  (lambda (fs k)  
    ((por (ptrans* (pdef (pand pname (pkey "=") exp-syn))
                   (lambda (name equal exp) (list 'define name exp)))
          (ptrans* (pdef (pand (pkey "macro") pname
                               (pnest #\( (pbetween pname (ppunct #\,)))
                               body-syn))
                   (lambda (stmt name args exp)
                     (list 'syntax-stmt (symbol->string name) args exp)))
          (ptrans* (pdef (pand (pkey "syntax-call") pname
                               (pnest #\( (pbetween pname (ppunct #\,)))
                               body-syn))
                   (lambda (stmt name args exp)
                     (list 'syntax-call (symbol->string name) args exp)))
	  ;;; there is no need to distinguish definers
          ;; (ptrans* (pdef (pand (pkey "syntax-definer") pname
          ;;                      (pnest #\( (pbetween pname (ppunct #\,)))
          ;;                      body-syn))
          ;;          (lambda (stmt name args exp)
          ;;            (list 'syntax-definer (symbol->string name) args exp)))
          (ptrans* (pdef (pand pname (pnest #\( (pbetween* pident (ppunct #\,))) 
                               body-syn))
                   (lambda (name args exp) (list 'def name args exp))))
     fs k)))

(define exp-syn
  (lambda (fs k)
    ((por define-syn
          if-syn
          let-syn
          letrec-syn
          syntax-case-syn
          assn-syn
;          (perror "unknown expression starting with ~a")
          ) 
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
       (ptrans (pkey "div") (lambda (_) 'div))
       (ptrans (pkey "rem") (lambda (_) 'remainder))))

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
                                 (ptrans (pnest #\( (pbetween* exp-syn (ppunct #\,)))
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
          (ptrans* (pstmt "fn" (pand (pnest #\( (pbetween* pname (ppunct #\,))) body-syn))
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

(define (test-parsing-2) 
  (let ((fs 
         '((immediate (num 1)) (name (keyword "+")) (immediate (num 2)))))
    (body-syn fs list)))


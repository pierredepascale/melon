;;; lexer.scm -- lexer for the melon system

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
	  (cond ((string=? key "::") (make-lex-keyword key))
		((string=? key ":=") (make-lex-keyword key))
		((char=? #\: (string-ref key 0))
		 (make-lex-symbol
		  (string->symbol (substring key 0 (- (string-length key) 1)))))
		(else
		 (make-lex-keyword key)))))))

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

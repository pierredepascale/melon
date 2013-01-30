;;; repl.scm -- read eval print loop

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
        (display "read:") (pp form)
        (if (not (null? form))
            (let ((expanded (expand-syntax* form *syntaxes*)))
              (display "expa:") (pp expanded)
              (let ((translated (form-syn expanded)))
                (display "exec:") (pp translated)
                (ev translated *global*)
                (newline) (display "done.") (newline)
                (loop (read-form* *syntaxes* port)))))))))

(for-each melon (cdr (command-line)))

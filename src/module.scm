;;; module.scm -- Module definition of the Melon system for Scheme48

(define-structure melon
  (export melon dylan)
  (open scheme)
  (files lexer
         sst
         expand
         parser
         interpreter
         repl))

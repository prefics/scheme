;;; compiler.scm -- The standard Scheme compiler for Schemy

(define-structure scheme-compiler
  (export compiler compile&go compile-only syntax-expand assembler)
  (open compat scheme scheme-runtime big-scheme)
  (files (scheme-compiler syntax)
	 (scheme-compiler compiler)
         (scheme-compiler gen)
         (scheme-compiler assembler)
         (scheme-compiler fasl)))
                   

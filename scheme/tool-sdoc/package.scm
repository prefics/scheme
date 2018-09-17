;;; package.scm -- package definition for sdoc tool

(define-structure tool-sdoc
  (export)
  (open scheme-runtime scheme-posix)
  (doc "Scheme source documentation tool")
  (files markup.scm
         sdoc.scm))
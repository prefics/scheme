;;; package.scm -- package definition for the schake tool

(define-structure tool-schake
  (export
   <target> make-target target?
   target-name target-doc target-dependents target-script

   target-needs-update?
   target-modification-time
   run-target-script
   update-target-directly

   targets target-names targets-clear! the-target with-target
   target-ref target-add target-delete ->target target-run target-exists?
   target-update
   (deftarget :syntax) $always $never

   <target-file> make-file-target file-target-file-name
   (deftarget-file :syntax)
   (def-c-application :syntax)

   (def-structure-target :syntax)
   include
   load-schakefile load-schakefile-from
   )
  (open scheme-runtime scheme-posix scheme-environment)
  (doc "Schake tool")
  (files schake.scm))

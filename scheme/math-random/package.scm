;;; package.scm -- package definition for math-random module

(define-structure math-random
  (export random)
  (open scheme-runtime)
  (doc "Random number generation")
  (files random.scm))

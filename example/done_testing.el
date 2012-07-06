(require 'test-more)
(test-more:import)

(test-more:done-testing
 (ok t)
 (is 1 1)
 (isnt 1 0)
 (like "123" "[0-9]"))

;;;; run tests to evaluate following S-exp
;; (eval-buffer)

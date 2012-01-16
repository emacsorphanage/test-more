(require 'test-more)
(test-more:import)

(defun add (a b)
  (if (or (null a) (null b))
      (error "argument is nil"))
  (+ a b))

(plan 1)

(test-more:subtest "test `add' function"
                   (is (add 1 2) 3 "normal case1")
                   (is (add 0 0) 0 "normal case2")
                   (test-more:is-error (add 1 nil) "invalid argument"))

(finalize)

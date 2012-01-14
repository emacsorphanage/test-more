(require 'test-more)
(test-more:import) ;; use test functions(not macros) without 'test-more:' prefix

(plan 14)

(ok (typep "This is String" 'string) "first argument is true")

(is '(a b) '(a b) "first argument equals to second argument")
(isnt (length '(a b)) 1 "first argument does not equal to second argument")

(diag "Prints a diagnostic message")

(is-type 10 'integer "type of first argument is second argument")

(like "987" "^[0-9]+$" "first argument matchs second argument which is regexp")

(test-more:skip "skips tests Like Test::More SKIP block"
                2 ;; skip 2 tests
                (ok (not-implement) "not implemented now 1")
                (is (not-implement) "not implemented now 2"))

(test-more:todo "Like Test::More TODO block"
                (test-more:ok nil "todo1")
                (test-more:is 1 2 "todo2"))

(test-more:subtest "subtest"
                   (ok 1 "subtest 1")
                   (ok nil "subtest 2"))

(test-more:is-print (princ "hello") "hello"
                    "first argument outputs string of second argument to stdout")

(test-more:is-error (error "error") "first argument throws exception")

(pass "always pass")
(fail "always fail")

(finalize)

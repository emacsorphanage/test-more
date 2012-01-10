# EL-TEST-MORE - Emacs test frameworket like Perl's Test::More

EL-TEST-MORE is inspired by Perl's Test::More and Common Lisp's CL-TEST-MORE.


## Synopsis

    (require 'test-more)
    (test-more:import)

    (plan 3)

    (ok (typep "test-more" 'string) "string type test")
    (is (+ 1 1) 2 "add test")

    (like "987" "^[0-9]+$")

    ;; Don't forget this
    (finalize)


## Functions

* ok
* is
* isnt
* diag
* is-print
* is-error
* is-type
* like
* skip
* pass
* fail

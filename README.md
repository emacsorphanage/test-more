# EL-TEST-MORE - Emacs test frameworket like Perl's Test::More

EL-TEST-MORE is inspired by Perl's Test::More and Common Lisp's CL-TEST-MORE.
EL-TEST-MORE support TAP(Test Anything Protocol), so that you can test elisp
by prove command.


## Synopsis

    (require 'test-more)
    (test-more:import)

    (plan 3)

    (ok (typep "test-more" 'string) "string type test")
    (is (+ 1 1) 2 "add test")

    (like "987" "^[0-9]+$")

    ;; Don't forget this
    (finalize)


## Test Functions

* test-more:ok($got [$desc])

Test $got is true.

* test-more:is($got $expected [$desc])

Test $got equals to $expected. default test function is `equal`.

* test-more:isnt

Test $got does *not* equals to $expected. default test function is `equal`.

* test-more:diag(diag)

Output $diag as diagnostics

* test-more:is-type($got $type [$desc])

Test type of $got is $type.

* test-more:like($got $regexp [$desc])

Test $regexp matchs $got.

* test-more:skip($why $how-many)

Skip $how-many tests.

* test-more:pass($desc)

Always pass.

* test-more:fail

Always failed.

## Test Macros

* test-more:is-print($form $expected [$desc])

Test output of $form to *STDOUT* equals to $expected

* test-more:is-error($form [$desc])

Test $form throws exception.

* test-more:todo($desc $forms)

Like Perl Test::More TODO block.
`test-more:todo` is the mark that means tests in it will be implemented.

* test-more:subtest(desc $forms)

Like Perl Test::More subtest. Group similar tests.
Tests in `test-more:subtest` is treated one test.

* test-more:done-testing

Like Perl Test::More done-testing.

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

* test-more:plan($num)

Plan to test $num tests. You should call `test-more:plan` at first.

* test-more:finalize

Finalize this test file. You should call `test-more:finalize` at last.

* test-more:ok($got [$desc])

Test $got is true.

* test-more:is($got $expected [$desc])

Test $got equals to $expected. default test function is `equal`.

* test-more:isnt

Test $got does *not* equals to $expected. default test function is `equal`.

* test-more:diag($diag)

Output $diag as diagnostics

* test-more:is-type($got $type [$desc])

Test type of $got is $type.

* test-more:like($got $regexp [$desc])

Test $regexp matchs $got.

* test-more:skip($why $how-many)

Skip $how-many tests.

* test-more:pass($desc)

Always pass.

* test-more:fail($desc)

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

* test-more:done-testing($forms)

Like Perl Test::More done-testing.


## Call Test Functions without test-more:prefix

You call `(test-more:import)` at first.


## Support TAP(Test Anything Protocol)

EL-TEST-MORE supports TAP(Test Anything Protocol).
You evaluate following elisp

    (require 'test-more)
    (test-more:import)

    (plan 4)

    (ok (typep "This is String" 'string) "first argument is true")

    (is '(a b) '(a b) "first argument equals to second argument")
    (like "987" "^[0-9]+$" "first argument matchs second argument which is regexp")

    (fail "Failing test")

    (finalize)

You get following output

    1..4
    ok 1 - first argument is true
    ok 2 - first argument equals to second argument
    ok 3 - first argument matchs second argument which is regexp
    not ok 4 - Failing test
    #  Looks like you failed 1 tests of 4 run

## Run test with *prove*

You can test elisp file with following prove command

    prove -v --exec='emacs -Q --batch -l test-more.el -l' test_files

To write `.proverc` makes you test easily.

    --exec "emacs -Q --batch -L ~/.emacs.d/auto-install -l test-more.el -l"
    --color
    --merge
    --timer
    -w

and you can use following command for testing

    prove -v test_files

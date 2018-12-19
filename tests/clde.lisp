(defpackage clde-test
  (:use :cl
        :clde
        :prove))
(in-package :clde-test)

;; NOTE: To run this test file, execute `(asdf:test-system :clde)' in your Lisp.

(plan 1)

(let ((integer-list '(1 2 3 4 5 6 7 8 9 0)))
  (subtest "PICK-RANDOM tests."
    (is-type (pick-random integer-list) 'number
	     "PICK-RANDOM returns same type as list members.")
    (ok (member (pick-random integer-list) integer-list)
	"PICK-RANDOM returns a member of the given list.")))

(finalize)

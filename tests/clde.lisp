(defpackage clde-test
  (:use :cl
        :clde
        :prove))
(in-package :clde-test)

;; NOTE: To run this test file, execute `(asdf:test-system :clde)' in your Lisp.

(plan 11)

(let ((integer-list (vector 1 2 3 4 5 6 7 8 9 0))
      (symbol-list (vector 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
  (subtest "PICK-RANDOM tests."

    (is-type (pick-random integer-list) 'number
	     "PICK-RANDOM returns same type as list members.")

    (is-type (pick-random symbol-list) 'symbol
	     "PICK-RANDOM returns same type as list members.")

    (ok (find (pick-random integer-list) integer-list)
	"PICK-RANDOM returns a member of the given list.")))

(let* ((integer-list (vector 1 2 3 4 5 6 7 8 9 0))
       (chosen (choose-n integer-list 10)))

  (subtest "CHOOSE-N tests."

    (is (length (choose-n integer-list 1)) 1
	"CHOOSE-N returns `n' items")

    (is (length (choose-n integer-list 2)) 2
	"CHOOSE-N returns `n' items")

    (is (length (choose-n integer-list (length integer-list))) (length integer-list)
	"CHOOSE-N returns `n' items")

    (ok (subsetp (coerce (choose-n integer-list 10) 'list) (coerce integer-list 'list))
	"CHOOSE-N returns only elements from the given list.")

    (ok (eql (length (remove-duplicates chosen)) (length chosen))
	"CHOOSE-N does not return the same element more than once.")))

(let* ((integer-list (vector 1 2 3 4 5 6 7 8 9 0)))

  (subtest "CHOOSE-N-NOT tests."

    (is (length (choose-n-not integer-list 1 1)) 1
	"CHOOSE-N-NOT returns `n' items")

    (is (length (choose-n-not integer-list 2 1)) 2
	"CHOOSE-N-NOT returns `n' items")

    (ok (not (find 1 (choose-n-not integer-list 9 1)))
	"CHOOSE-N-NOT does not choose `but-not'")))

(subtest "V-ADD tests."

  (ok (equalp (v-add #(1 2 3 4) #(1 2 3 4)) #(2 4 6 8))
      "#(1 2 3 4) + #(1 2 3 4).")

  (ok (equalp (v-add #(1 2 3 4) #(-1 -2 -3 -4)) #(0 0 0 0))
      "#(1 2 3 4) + #(-1 -2 -3 -4).")

  (ok (equalp (v-add #(0 0 0 0) #(0 0 0 0)) #(0 0 0 0))
      "#(0 0 0 0) + #(0 0 0 0)."))

(subtest "V-SUB tests."

  (ok (equalp (v-sub #(1 2 3 4) #(1 2 3 4)) #(0 0 0 0))
      "#(1 2 3 4) - #(1 2 3 4).")

  (ok (equalp (v-sub #(1 2 3 4) #(-1 -2 -3 -4)) #(2 4 6 8))
      "#(1 2 3 4) - #(-1 -2 -3 -4).")

  (ok (equalp (v-sub #(0 0 0 0) #(0 0 0 0)) #(0 0 0 0))
      "#(0 0 0 0) - #(0 0 0 0)."))

(subtest "V-SCALE tests."

  (ok (equalp (v-scale 0 #(1 2 3 4)) #(0 0 0 0))
      "0 * #(1 2 3 4).")

  (ok (equalp (v-scale -1 #(-1 -2 -3 -4)) #(1 2 3 4))
      "-1 * #(-1 -2 -3 -4).")

  (ok (equalp (v-scale 1 #(1 2 3 4)) #(1 2 3 4))
      "1 * #(1 2 3 4).")

  (ok (equalp (v-scale 3 #(1 2 3 4)) #(3 6 9 12))
      "3 * #(1 2 3 4)."))

(subtest "F-DIFF tests."

  (ok (equalp (f-diff 0 #(1 2 3 4) #(0 0 0 0)) #(0 0 0 0))
      "0 * (a - b).")

  (ok (equalp (f-diff 1 #(1 2 3 4) #(0 0 0 0)) #(1 2 3 4))
      "1 * (a - b).")

  (ok (equalp (f-diff 2 #(1 2 3 4) #(2 3 4 5)) #(-2 -2 -2 -2))
      "2 * (a - b)."))

(subtest "GEN-DONOR tests."

  (let ((population (make-array 3 :initial-contents '(#(1 2 3 4)
						      #(0 1 2 3)
						      #(0 1 2 3)))))
    (ok (let ((donor (gen-donor population 0)))
	  (or
	   (equalp donor #(1 2 3 4))
	   (equalp donor #(0 1 2 3))))
	"GEN-DONOR with f = 0 gets back the first chosen vector.")

    (ok (let ((donor (gen-donor population 1)))
	  (or
	   (equalp donor #(1 2 3 4))
	   (equalp donor #(-1 0 1 2))))
	"GEN-DONOR with f = 1.")

    (ok (let ((donor (gen-donor population 2)))
	  (or
	   (equalp donor #(1 2 3 4))
	   (equalp donor #(-2 -1 0 1))
	   (equalp donor #(2 3 4 5))))
	"GEN-DONOR with f = 2.")))

(subtest "CROSSOVER tests."

  (let ((target #(1 2 3 4))
	(donor #(0 0 0 0)))

    (ok (equalp (crossover target donor 1.0d0) #(0 0 0 0))
	"CROSSOVER with rate 1.0d0 takes all the donor's parameters.")))

(subtest "ROSENBROCK tests."

  (is (rosenbrock #(1.0d0 1.0d0)) 0.0d0
      "ROSENBROCK perfect solution in 2-d.")

  (is (rosenbrock #(1.0d0 1.0d0 1.0d0)) 0.0d0
      "ROSENBROCK perfect solution in 3-d.")

  (is (rosenbrock (make-array 10 :initial-element 1.0d0)) 0.0d0
      "ROSENBROCK perfect solution in 10-d."))

(subtest "SELECT tests."

  (ok (equalp (select #(0.0d0 0.0d0) #(1.0d0 1.0d0) #'rosenbrock) #(1.0d0 1.0d0))
      "SELECT for ROSENBROCK picks perfect solution."))

(finalize)

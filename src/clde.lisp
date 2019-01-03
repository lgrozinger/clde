(defpackage clde
  (:use :cl)
  (:export :de
	   :de/rand/1/bin
	   :de/rand/2/bin
	   :de/best/2/bin
	   :de/best/1/bin
	   :pick-random
	   :choose-n
	   :choose-n-not
	   :v-add
	   :v-sub
	   :v-scale
	   :f-diff
	   :gen-donor
	   :crossover
	   :select
	   :random-individual
	   :random-population
	   :rosenbrock))
(in-package :clde)

;; blah blah blah.

(defun pick-random (vector)
  (let ((rand-index (random (length vector))))
    (aref vector rand-index)))

(defun choose-n (vector n)
  (let ((chosen (make-array n))
	(remaining (make-array (length vector) :initial-contents vector)))
    (loop
       :for i :from 0 :below n
       :do
       (setf (aref chosen i) (pick-random remaining))
       (setf remaining (remove (aref chosen i) remaining :count 1 :test #'equalp)))

    chosen))

(defun choose-n-not (vector n but-not)
  (let ((allowed (remove but-not vector :count 1 :test #'equalp)))
    (choose-n allowed n)))

(defun v-add (vector-a vector-b)
  (map 'simple-vector #'+ vector-a vector-b))

(defun v-sub (vector-a vector-b)
  (map 'simple-vector #'- vector-a vector-b))

(defun v-scale (factor vector)
  (map 'simple-vector #'(lambda (x) (* factor x)) vector))

(defun f-diff (f vector-a vector-b)
  (v-scale f (v-sub vector-a vector-b)))

(defun gen-donor (eligible-population f &key (diffs 1))
  (let* ((chosen (choose-n eligible-population (+ 1 (* diffs 2))))
	 (donor (aref chosen 0)))
    (loop
       :for i :from 1 :below (length chosen) :by 2
       :do
       (setf donor (v-add donor (f-diff f (aref chosen i) (aref chosen (+ 1 i))))))

    donor))

(defun gen-donor-best (best eligible-population f &key (diffs 1))
  (let* ((chosen (choose-n eligible-population (* diffs 2)))
	 (donor (make-array (length best) :initial-contents best)))
    (loop
       :for i :from 0 :below (length chosen) :by 2
       :do
       (setf donor (v-add donor (f-diff f (aref chosen i) (aref chosen (+ 1 i))))))

    donor))


(defun crossover (target-vector donor-vector crossover-rate)
  (let* ((d (length target-vector))
	 (rand-parameter (random d))
	 (trial-vector (make-array d :initial-contents target-vector)))

    (loop
       :for j :from 0 :below d
       :do
       (if (or (<= (random 1.0d0) crossover-rate) (eql j rand-parameter))
	   (setf (aref trial-vector j) (aref donor-vector j))))

    trial-vector))

(defun rosenbrock (x)
  (let ((d (length x)))
    (loop
       :for i :below (1- d)
       :summing (+ (* 100
		      (expt (- (aref x (1+ i))
			       (expt (aref x i) 2))
			    2))
		   (expt (- 1 (aref x i)) 2)))))

(defun select (vector-a vector-b cost-function)
  (if (< (funcall cost-function vector-a)
	 (funcall cost-function vector-b))
      vector-a
      vector-b))

(defun random-individual (dimensions upper lower)
  (make-array dimensions :initial-contents (loop
					      :for i :from 0 :below dimensions
					      :collect (+ lower (random (- upper lower))))))

(defun random-population (pop-size dimensions &key (lower 0.0d0) (upper 1.0d2))
  (make-array pop-size :initial-contents (loop
					    :for i :from 0 :below pop-size
					    :collect (random-individual dimensions upper lower))))

(defun de (pop-size max-generations cost-function dimensions cr f
	   &key (strat 'rand) (diffs 1))

  (let* ((population (random-population pop-size dimensions))
	 (best (aref population 0))
	 (best-score (funcall cost-function best)))

    (loop
       :for g :from 0 :below max-generations
       :until (eql best-score 0.0d0)
       :do
       (loop
	  :for i :from 0 :below pop-size
	  :do
	  (let* ((target (aref population i))
		 (donor (if (equalp 'rand strat)
			    (gen-donor (remove target population :count 1) f :diffs diffs)
			    (gen-donor-best best (remove target population :count 1) f :diffs diffs)))
		 (trial (crossover target donor cr))
		 (selected (select target trial cost-function)))

	    (setf (aref population i) selected)
	    (let ((this-score (funcall cost-function selected)))
	      (if (< this-score best-score)
		  (progn (setf best-score this-score)
			 (setf best selected)))))))

    best))


;; functions for common schemes
(defun de/rand/1/bin (pop-size max-generations cost-function dimensions cr f)
  (de pop-size max-generations cost-function dimensions cr f :strat 'rand :diffs 1))

(defun de/rand/2/bin (pop-size max-generations cost-function dimensions cr f)
  (de pop-size max-generations cost-function dimensions cr f :strat 'rand :diffs 2))

(defun de/best/1/bin (pop-size max-generations cost-function dimensions cr f)
  (de pop-size max-generations cost-function dimensions cr f :strat 'best :diffs 1))

(defun de/best/2/bin (pop-size max-generations cost-function dimensions cr f)
  (de pop-size max-generations cost-function dimensions cr f :strat 'best :diffs 2))

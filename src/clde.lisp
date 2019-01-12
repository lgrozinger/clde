(defpackage clde
  (:use :cl :sb-thread :cl-randist)
  (:export :de
           :mtde
	   :jade-de
	   :de/rand/1/bin
	   :de/rand/2/bin
	   :de/best/2/bin
	   :de/best/1/bin
	   :rosenbrock))
(in-package :clde)

(defun lehmer-mean (list)
  (/ (reduce #'+ (map 'list #'* list list)) (reduce #'+ list)))

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

(defun gaussian-f (mean sigma)
  (let ((f (random-normal mean sigma)))
    (if (< f 0.0d0)
	(gaussian-f mean sigma)
	(if (> f 2.0d0)
	    2.0d0
	    f))))
  
(defun gaussian-cr (mean sigma)
  (let ((cr (random-normal mean sigma)))
    (if (< cr 0.0d0)
	0.0d0
	(if (> cr 1.0d0)
	    1.0d0
	    cr))))

(defun evolve-f (current-mean successful-list c)
  (if successful-list
      (let ((success-mean (lehmer-mean successful-list)))
	(+ (* (- 1 c) current-mean) (* c success-mean)))
      current-mean))
    
(defun evolve-cr (current-mean successful-list c)
  (if successful-list
      (let ((success-mean (/ (reduce #'+ successful-list) (length successful-list))))
	(+ (* (- 1 c) current-mean) (* c success-mean)))
      current-mean))

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

(defun random-population (pop-size dimensions &key (lower 0.0d0) (upper 1.0d1))
  (make-array pop-size :initial-contents (loop
					    :for i :from 0 :below pop-size
					    :collect (random-individual dimensions upper lower))))

(defun jade-de (pop-size max-generations cost-function dimensions init-cr init-f
		&key (strat 'rand) (diffs 1) (rand-lb 0.0d0) (rand-ub 1.0d1))
  (let* ((population (random-population pop-size dimensions :lower rand-lb :upper rand-ub))
	 (best (aref population 0))
	 (best-score (funcall cost-function best))
	 (mean-cr init-cr)
	 (mean-f init-f))
    (loop
      :for g :from 0 :below max-generations
      :until (eql best-score 0.0d0)
      :do
	 (if (eq 0 (mod g 1000))
	     (format t "Gen: ~a~%Best: ~a~%Score: ~a~%~%" g best best-score))
	 (let* ((successful-cr ())
		(successful-f ()))
	   (loop
	     :for i :from 0 :below pop-size
	     :do
		(let* ((target (aref population i))
		       (f (gaussian-f mean-f 1.0d-1))
		       (donor (if (equalp 'rand strat)
				  (gen-donor (remove target population :count 1) f :diffs diffs)
				  (gen-donor-best best (remove target population :count 1) f :diffs diffs)))
		       (cr (gaussian-cr mean-cr 1.0d-1))
		       (trial (crossover target donor cr))
		       (selected (select target trial cost-function))
		       (success (equal selected trial)))

		  (if success (setf successful-cr (append successful-cr (list cr))))
		  (setf (aref population i) selected)
		  (let ((this-score (funcall cost-function selected)))
		    (if (< this-score best-score)
			(progn (setf best-score this-score)
			       (setf best selected))))))
	   (setf mean-cr (evolve-cr mean-cr successful-cr 2.0d-1))
	   (setf mean-f (evolve-f mean-f successful-f 2.0d-1))))
      best))


(defun de (pop-size max-generations cost-function dimensions cr f
	   &key (strat 'rand) (diffs 1) (rand-lb 0.0d0) (rand-ub 1.0d1))

  (let* ((population (random-population pop-size dimensions :lower rand-lb :upper rand-ub))
	 (best (aref population 0))
	 (best-score (funcall cost-function best)))

    (loop
       :for g :from 0 :below max-generations
       :until (eql best-score 0.0d0)
       :do
       (if (eq 0 (mod g 1000))
	   (format t "Gen: ~a~%Best: ~a~%Score: ~a~%~%" g best best-score))
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

(defun mtde (pop-size max-generations cost-function dimensions cr f
	     &key (strat 'rand) (diffs 1) (rand-lb 0.0d0) (rand-ub 1.0d1))

  (let* ((population (random-population pop-size dimensions :lower rand-lb :upper rand-ub))
	 (best (aref population 0))
	 (best-score (funcall cost-function best)))

    (loop
       :for g :from 0 :below max-generations
       :until (eql best-score 0.0d0)
       :do
       (let ((individuals (loop
			     :for i :from 0 :below pop-size
			     :collect
			     (let ((pop population)
				   (safe-i i))
			       (make-thread (lambda ()
					      (let* ((target (aref pop safe-i))
						     (donor (if (equalp 'rand strat)
								(gen-donor (remove target pop :count 1) f :diffs diffs)
								(gen-donor-best best (remove target pop :count 1) f :diffs diffs)))
						     (trial (crossover target donor cr))
						     (selected (select target trial cost-function)))

						selected)))))))
	 (loop
	    :for i :from 0 :below (length individuals)
	    :do
	    (let ((survivor (join-thread (elt individuals i))))
	      (setf (aref population i) survivor)
	      (let ((this-score (funcall cost-function survivor)))
		(if (< this-score best-score)
		    (progn (setf best-score this-score)
			   (setf best survivor))))))))

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

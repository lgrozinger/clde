(defpackage clde
  (:use :cl :sb-thread :cl-randist)
  (:export :de
	   :jade
	   :de/rand/1/bin
	   :de/rand/2/bin
	   :de/best/2/bin
	   :de/best/1/bin
	   :rosenbrock
	   :random-population
	   :mutate-bin
	   :combine-rand
	   :combine-best))
(in-package :clde)

(defparameter *FE* 0)
(defvar *Fs* ())
(defvar *CRs* ())

(defun de (NP G D CR F objective population combination mutation
	   &key (target-cost 0.0d0) (report-every 1000))

  (let* ((P (funcall population NP D))
	 (costs (make-array (length P) :initial-contents (map 'list objective P))))

    ;; EVERY GENERATION
    (loop
       :for i :from 1 :to G
       :until (<= (reduce #'min costs) target-cost)
       :do
	 (if (eq 0 (mod i report-every)) (report i P costs))

	 ;; EVERY INDIVIDUAL
	 (loop
	    :for j :from 0 :below (length P)
	    :do
	      (let* ((target (aref P j))
		     (donor (funcall combination target P costs F))
		     (trial (funcall mutation target donor CR))
		     (score (funcall objective trial)))

		(when (< score (elt costs j))
		  (setf (aref P j) trial)
		  (setf (aref costs j) score)))))

    (report G P costs)
    (elt P (position (reduce #'min costs) costs))))

(defun jade (NP G D CR F objective population combination mutation
	   &key (target-cost 0.0d0) (report-every 1000))

  (let* ((P (funcall population NP D))
	 (costs (make-array (length P) :initial-contents (map 'list objective P))))

    ;; EVERY GENERATION
    (loop
       :for i :from 1 :to G
       :until (<= (reduce #'min costs) target-cost)
       :do
	 (if (eq 0 (mod i report-every)) (report i P costs))
	 (let ((good-CRs ())
	       (good-Fs ()))
	 ;; EVERY INDIVIDUAL
	   (loop
	      :for j :from 0 :below (length P)
	      :do
		(let* ((target (aref P j))
		       (this-CR (gaussian-cr CR 1.0d-1))
		       (this-F (gaussian-f F 1.0d-1))
		       (donor (funcall combination target P costs this-F))
		       (trial (funcall mutation target donor this-CR))
		       (score (funcall objective trial)))

		  (when (< score (elt costs j))
		    (setf (aref P j) trial)
		    (setf (aref costs j) score)
		    (setf good-CRs (cons this-CR good-CRs))
		    (setf good-Fs (cons this-F good-Fs)))))

	   (setf CR (evolve-cr CR good-CRs 1.0d-1))
	   (setf F (evolve-f F good-Fs 1.0d-1))
	   (setf *Fs* (nconc *Fs* (list F)))
	   (setf *CRs* (nconc *CRs* (list CR)))))

    (report G P costs)
    (elt P (position (reduce #'min costs) costs))))


(defun combine-rand (target P costs F &key (diffs 1))
  (declare (ignorable costs))
  (gen-donor (remove target P :count 1) F :diffs diffs))

(defun combine-best (target P costs F &key (diffs 1))
  (let ((best (elt P (position (reduce #'min costs) costs))))
    (if (equal best target)
	    (gen-donor-best best (remove best P :count 1) F :diffs diffs)
	    (gen-donor-best best (remove target (remove best P :count 1) :count 1) F :diffs diffs))))

(defun mutate-bin (target donor CR)
  (let* ((d (length target))
	 (rand-parameter (random d))
	 (trial (make-array d :initial-contents target)))

    (loop
       :for j :from 0 :below d
       :do
	 (when (or (<= (random 1.0d0) CR) (eql j rand-parameter))
	     (setf (aref trial j) (aref donor j))))

    trial))

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
      (gaussian-f current-mean 1.0d-1)))

(defun evolve-cr (current-mean successful-list c)
  (if successful-list
      (let ((success-mean (/ (reduce #'+ successful-list) (length successful-list))))
	(+ (* (- 1 c) current-mean) (* c success-mean)))
      (gaussian-cr current-mean 1.0d-1)))

(defun rosenbrock (x)
  (setf *FE* (+ *FE* 1))
  (let ((d (length x)))
    (loop
       :for i :below (1- d)
       :summing (+ (* 100
		      (expt (- (aref x (1+ i))
			       (expt (aref x i) 2))
			    2))
		   (expt (- 1 (aref x i)) 2)))))

(defun random-individual (dimensions upper lower)
  (make-array dimensions :initial-contents (loop
					      :for i :from 0 :below dimensions
					      :collect (+ lower (random (- upper lower))))))

(defun random-population (pop-size dimensions &key (lower 0.0d0) (upper 1.0d1))
  (make-array pop-size :initial-contents (loop
					    :for i :from 0 :below pop-size
					    :collect (random-individual dimensions upper lower))))

(defun report (G P costs)
  (let* ((cost (reduce #'min costs))
	 (top-P (elt P (position cost costs))))
    (format t "Gen: ~a~%Best: ~a~%Score: ~a~%~%" G cost top-P)))

;; (defun mtde (pop-size max-generations cost-function dimensions cr f
;; 	     &key (strat 'rand) (diffs 1) (rand-lb 0.0d0) (rand-ub 1.0d1))

;;   (let* ((population (random-population pop-size dimensions :lower rand-lb :upper rand-ub))
;; 	 (best (aref population 0))
;; 	 (best-score (funcall cost-function best)))

;;     (loop
;;        :for g :from 0 :below max-generations
;;        :until (eql best-score 0.0d0)
;;        :do
;;        (let ((individuals (loop
;; 			     :for i :from 0 :below pop-size
;; 			     :collect
;; 			     (let ((pop population)
;; 				   (safe-i i))
;; 			       (make-thread (lambda ()
;; 					      (let* ((target (aref pop safe-i))
;; 						     (donor (if (equalp 'rand strat)
;; 								(gen-donor (remove target pop :count 1) f :diffs diffs)
;; 								(gen-donor-best best (remove target pop :count 1) f :diffs diffs)))
;; 						     (trial (crossover target donor cr))
;; 						     (selected (select target trial cost-function)))

;; 						selected)))))))
;; 	 (loop
;; 	    :for i :from 0 :below (length individuals)
;; 	    :do
;; 	    (let ((survivor (join-thread (elt individuals i))))
;; 	      (setf (aref population i) survivor)
;; 	      (let ((this-score (funcall cost-function survivor)))
;; 		(if (< this-score best-score)
;; 		    (progn (setf best-score this-score)
;; 			   (setf best survivor))))))))

;;     best))


;; functions for common schemes
(defun de/rand/1/bin (NP G D CR F &optional (cost-function #'rosenbrock))
  (de NP G D CR F cost-function #'random-population #'combine-rand #'mutate-bin))

(defun de/rand/2/bin (NP G D CR F &optional (cost-function #'rosenbrock))
  (let ((combiner (lambda (target P costs F)
		    (combine-rand target P costs F :diffs 2))))
    (de NP G D CR F cost-function #'random-population combiner #'mutate-bin)))

(defun de/best/1/bin (NP G D CR F &optional (cost-function #'rosenbrock))
  (de NP G D CR F cost-function #'random-population #'combine-best #'mutate-bin))

(defun de/best/2/bin (NP G D CR F &optional (cost-function #'rosenbrock))
    (let ((combiner (lambda (target P costs F)
		    (combine-best target P costs F :diffs 2))))
    (de NP G D CR F cost-function #'random-population combiner #'mutate-bin)))

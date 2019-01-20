(defpackage clde
  (:use :cl :sb-thread :cl-randist)
  (:export :de
	   :mtde
	   :mtjade
	   :mtjade-refresh
	   :jade
	   :jade-refresh
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
(defparameter *C* ())

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

(defun mtde (NP G D CR F objective population combination mutation
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
       (let ((individuals (make-array NP)))
	 (loop
	    :for j :from 0 :below NP
	    :do
	    (let ((thread-P P)
		  (thread-costs costs)
		  (thread-j j))
	      (setf (aref individuals j)
		    (make-thread (lambda ()

				   (let* ((target (elt thread-P thread-j))
					  (donor (funcall combination target thread-P thread-costs F))
					  (trial (funcall mutation target donor CR))
					  (score (funcall objective trial)))

				     (if (< score (elt thread-costs thread-j))
				       (values trial score)
				       (values target (elt thread-costs thread-j)))))))))

	 (loop
	    :for j :from 0 :below NP
	    :do
	    (multiple-value-bind (survivor score) (join-thread (elt individuals j))
	      (setf (aref P j) survivor)
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
	   (setf F (evolve-f F good-Fs 1.0d-1))))

    (report G P costs)
    (values (elt P (position (reduce #'min costs) costs))
	    P)))

(defun mtjade (NP G D CR F objective population combination mutation
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
	 (let ((individuals (make-array NP))
	       (good-CRs ())
	       (good-Fs ()))
	   (loop
	      :for j :from 0 :below NP
	      :do
		(let ((thread-P P)
		      (thread-costs costs)
		      (thread-j j)
		      (this-cr (gaussian-cr CR 1.0d-1))
		      (this-f (gaussian-f F 1.0d-1)))
		  (setf (aref individuals j)
			(make-thread (lambda ()

				       (let* ((target (elt thread-P thread-j))
					      (donor (funcall combination target thread-P thread-costs this-f))
					      (trial (funcall mutation target donor this-cr))
					      (score (funcall objective trial)))

					 (if (< score (elt thread-costs thread-j))
					     (values trial score this-cr this-f)
					     (values target (elt thread-costs thread-j)))))))))

	   (loop
	      :for j :from 0 :below NP
	      :do
		(multiple-value-bind (survivor score this-cr this-f) (join-thread (elt individuals j))
		  (setf (aref P j) survivor)
		  (setf (aref costs j) score)
		  (when (and this-cr this-f)
		    (setf good-CRs (cons this-cr good-CRs))
		    (setf good-Fs (cons this-cr good-Fs)))))

	   (setf CR (evolve-cr CR good-CRs 1.0d-1))
	   (setf F (evolve-f F good-Fs 1.0d-1))))


    (report G P costs)
    (elt P (position (reduce #'min costs) costs))))


(defun jade-refresh (NP G D CR F objective population combination mutation
		     &key (target-cost 0.0d0) (report-every 1000) (stagnation-limit 1000))

  (let* ((P (funcall population NP D))
	 (costs (make-array (length P) :initial-contents (map 'list objective P)))
	 (stagnation-counter 0))

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

	   (when (not good-Fs)
	     (setf stagnation-counter (+ 1 stagnation-counter)))

	   ;; the refresh part
	   (when (or (inbred-p P) (> stagnation-counter stagnation-limit))
	     (setf NP (* NP 2))
	     (setf P (refresh P NP population costs))
	     (setf costs (make-array (length P) :initial-contents (map 'list objective P)))
	     (setf stagnation-counter 0)
	     (format t "REFRESH POPULATION... NP = ~a~%" NP))

	   (setf CR (evolve-cr CR good-CRs 1.0d-1))
	   (setf F (evolve-f F good-Fs 1.0d-1))))

    (report G P costs)
    (values (elt P (position (reduce #'min costs) costs)) P)))

(defun mtjade-refresh (NP G D CR F objective population combination mutation
		       &key (target-cost 0.0d0) (report-every 1000) (stagnation-limit 1000))

  (let* ((P (funcall population NP D))
	 (BP (elt P 0))
	 (costs (make-array (length P) :initial-contents (map 'list objective P)))
	 (BP-cost (elt costs 0))
	 (stagnation-counter 0))

    ;; EVERY GENERATION
    (loop
       :for i :from 1 :to G
       :until (<= (reduce #'min costs) target-cost)
       :do
       (if (eq 0 (mod i report-every)) (report i P costs))

       ;; EVERY INDIVIDUAL
	 (let ((individuals (make-array NP))
	       (good-CRs ())
	       (good-Fs ()))
	   (loop
	      :for j :from 0 :below NP
	      :do
		(let ((thread-P P)
		      (thread-costs costs)
		      (thread-j j)
		      (this-cr (gaussian-cr CR 1.0d-1))
		      (this-f (gaussian-f F 1.0d-1)))
		  (setf (aref individuals j)
			(make-thread (lambda ()

				       (let* ((target (elt thread-P thread-j))
					      (donor (funcall combination target thread-P thread-costs this-f))
					      (trial (funcall mutation target donor this-cr))
					      (score (funcall objective trial)))

					 (if (< score (elt thread-costs thread-j))
					     (values trial score this-cr this-f)
					     (values target (elt thread-costs thread-j)))))))))

	   (loop
	      :for j :from 0 :below NP
	      :do
		(multiple-value-bind (survivor score this-cr this-f) (join-thread (elt individuals j))
		  (setf (aref P j) survivor)
		  (setf (aref costs j) score)
		  (when (and this-cr this-f)
		    (setf good-CRs (cons this-cr good-CRs))
		    (setf good-Fs (cons this-cr good-Fs)))))

	   	   (when (not good-Fs)
		     (setf stagnation-counter (+ 1 stagnation-counter)))

		   ;; the refresh part
		   (when (or (inbred-p P) (> stagnation-counter stagnation-limit))
		     (let ((best-score (reduce #'min costs)))
		       (setf NP (* NP 2))
		       (when (< best-score BP-cost)
			 (setf BP (elt P (position best-score costs)))
			 (setf BP-cost best-score))
		       (setf P (funcall population NP D))
		       (setf costs (make-array (length P) :initial-contents (map 'list objective P)))
		       (setf stagnation-counter 0)
		       (format t "REFRESH POPULATION... NP = ~a~%" NP)))

		   (setf CR (evolve-cr CR good-CRs 1.0d-1))
		   (setf F (evolve-f F good-Fs 1.0d-1))))


    (report G P costs)
    (if (< BP-cost (reduce #'min costs))
	BP
	(elt P (position (reduce #'min costs) costs)))))


(defun inbred-p (P)
  (every (lambda (x) (equalp (elt P 0) x)) P))

(defun refresh (P NP gen-fun costs)
  (let* ((D (length (elt P 0)))
	 (best (position (reduce #'min costs) costs))
	 (survivors (list (elt P best)))
	 (refreshers (funcall gen-fun (- NP 1) D)))
    (make-array NP :initial-contents (concatenate (list 'simple-vector NP) survivors refreshers))))

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

(defun random-population (pop-size dimensions &key (lower -1.0d2) (upper 1.0d2))
  (make-array pop-size :initial-contents (loop
					    :for i :from 0 :below pop-size
					    :collect (random-individual dimensions upper lower))))

(defun report (G P costs)
  (let* ((cost (reduce #'min costs))
	 (top-P (elt P (position cost costs))))
    (setf *C* (nconc *C* (list cost)))
    (format t "Gen: ~a~%Best: ~a~%Score: ~a~%~%" G cost top-P)))

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

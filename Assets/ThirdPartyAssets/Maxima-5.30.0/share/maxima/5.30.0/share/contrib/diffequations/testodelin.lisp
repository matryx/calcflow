;; Author Barton Willis
;; University of Nebraska at Kearney
;; Copyright (C) 2004, Barton Willis

;; Brief Description: Maxima code for linear homogeneous second order
;; differential equations.

;; Maxima odelin is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; http://www.gnu.org/copyleft/gpl.html.

;; Maxima odelin has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$testodelin 1 '$version)


(defun $testodelin (fn)
  (let ((acc) (strm) (de) (sol) (t1) (gork) (i 0) (times)
	(solved-des) (unsolved-des) (total-time 0)
	(longest-time 0))

    (setq fn (string fn))
    (setq fn (make-pathname :name fn))
    ($load "odelin")

    (setq strm (open fn :direction :input))
    (while (not (eq 'eof (setq de (mread strm 'eof))))
      (push (nth 2 de) acc))
    (close strm)

    (setq acc (reverse acc))
    (dolist (de acc)
      (incf i)
      (meval '(($kill) $all)) ;; sigh....
      (format t "~%-- ~A ------------------------------------~%" i)
      (setq t1 (get-internal-run-time))      
      (displa `((mequal) de ,de))
      (setq gork `(($errcatch) (($odelin) ,de  |$y| |$x|)))  
      (setq sol (meval gork))
      (setq t1 (- (get-internal-run-time) t1))
      (push t1 times)
      (setq longest-time (max longest-time t1))
      (setq t1 (float (/ t1 internal-time-units-per-second)))
      (incf total-time t1)
      (setq t1 (mul t1 '$sec))
      (cond ((and (not (like sol '((mlist)))) (not (like ($first sol) nil)))
	     (push de solved-des)
	     (setq sol ($first sol))
	     (displa `((mequal) sol ,sol))
	     (displa `((mequal) time ,t1)))
	    (t
	     (push de unsolved-des)
	     (displa `((mequal) time ,t1))
	     (displa `((mequal) notsolved ,de)))))
    
    (setq longest-time (float (/ longest-time internal-time-units-per-second)))
    (setq total-time (mul total-time '$sec))
    (displa `((mequal) totaltime ,total-time))
    (displa `((mequal) longest_time ,longest-time))
    (displa `((mequal) number_of_solved_des ,(length solved-des)))
    (displa `((mequal) number_of_unsolved_des ,(length unsolved-des)))

    (setq times (sort times #'<))
    (push `(mlist) times)
    (displa `((mequal) times ,times))

    (dolist (de unsolved-des)
      (displa de))))
    

;; Author Barton Willis

;; University of Nebraska at Kearney
;; Copyright (C) 2004, 2005, Barton Willis
;; Brief Description: polynomial predicate function.
	   		       								 
;;  This program is free software; you can redistribute it and/or modify	 
;;  it under the terms of the GNU General Public License as published by	 
;;  the Free Software Foundation; either version 2 of the License, or		 
;;  (at your option) any later version.					 
 		       								 
;;  This program is distributed in the hope that it will be useful,		 
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of		 
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		 
;;  GNU General Public License for more details.				 
 		       								 
;;  You should have received a copy of the GNU General Public License	
;;  along with this program; if not, write to the Free Software 		 
;;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (functionp 'op-equalp)) ($load "linalg-utilities"))
  (if (not (functionp 'require-list-or-set)) ($load "nset")))

($put '$polynomialp 1 '$version)

;; Return true iff n is an integer and n >= 0.

(defun $nonnegintegerp (n)
  (and (integerp n) (>= n 0)))

(defun $polynomialp (p vars &optional (coeffp '$constantp) (exponp '$nonnegintegerp))
  (setq vars (require-list-or-set vars "$polynomialp"))
  (setq vars (mapcar '$ratdisrep vars))
  (if (every #'(lambda (s) (or ($symbolp s) ($subvarp s))) vars)
      (polynomialp ($ratdisrep p) vars coeffp exponp)
    (merror "The second argument to polynomialp must be a list of symbols")))
 
(defun polynomialp (p vars coeffp exponp)
  (or
   (mfuncall coeffp p)
   (if (member p vars :test #'equal) t nil)
   (and (op-equalp p 'mtimes 'mplus)
	(every #'(lambda (s) (polynomialp s vars coeffp exponp)) (margs p)))
   (and (op-equalp p 'mexpt) (polynomialp (car (margs p)) vars coeffp exponp)
	(mfuncall exponp (cadr (margs p))))))

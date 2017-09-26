;;;These subroutines do some low level input and utility stuff
;;;Copyright (C) 1999  Dan Stanger
;;;
;;;This library is free software; you can redistribute it and/or modify it
;;;under the terms of the GNU Library General Public License as published
;;;by the Free Software Foundation; either version 2 of the License, or (at
;;;your option) any later version.
;;;
;;;This library is distributed in the hope that it will be useful, but
;;;WITHOUT ANY WARRANTY; without even the implied warranty of
;;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;Library General Public License for more details.
;;;
;;;You should have received a copy of the GNU Library General Public
;;;License along with this library; if not, write to the Free Software
;;;Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;Dan Stanger dan.stanger@eee.org

; this constant is used to order the elements for the proper tree
; algorithm.
(defconstant *circuit-elements*
   (make-array 6 :initial-contents
     '((#\1 . $unknown) (#\V . $vsource) (#\C . $capacitor) (#\R . $resistor)
       (#\L . $inductor)
       (#\I . $isource))))

; this constant is used to order the tree elements for the state equation
(defconstant *circuit-elements-tree*
   (make-array 4 :initial-contents
     '($unknown $capacitor $resistor $vsource)))

;this constant is used to order the link elements for the state equation
(defconstant *circuit-elements-link*
   (make-array 4 :initial-contents
     '($unknown $inductor $resistor $isource)))

(defun process-type (s)
   (let* ((sn (symbol-name s))
	  (p (position (char sn 0) *circuit-elements* :key #'first )))
      (if (null p) '$unknown (cdr (aref *circuit-elements* p)))))

(DEFMTRFUN (|$getElementIndex| $ANY MDEFINE NIL NIL) 
           ($E) 
           (DECLARE (SPECIAL $E)) 
           (let ((p (position $E *circuit-elements* :key #'cdr)))
		(if (null p) (error "invalid value in getelementindex") p)))

(DEFMTRFUN (|$getTreeElementIndex| $ANY MDEFINE NIL NIL) 
           ($E) 
           (DECLARE (SPECIAL $E)) 
           (let ((p (position $E *circuit-elements-tree*)))
		(if (null p) (error "invalid value in gettreeelementindex") p)))

(DEFMTRFUN (|$getLinkElementIndex| $ANY MDEFINE NIL NIL) 
           ($E) 
           (DECLARE (SPECIAL $E)) 
           (let ((p (position $E *circuit-elements-link*)))
		(if (null p) (error "invalid value in getlinkelementindex") p)))

(defun process-line (l)
   (let* ((st (make-string-input-stream l)) (ty (read st))
          (from (read st)) (to (read st)) (ex (read-line st nil nil)))
   (list (quote (mlist))
      (intern-invert-case (concatenate 'string "$" (string ty)))
      (process-type ty)
      from
      to
      (when ex ($eval_string (intern-invert-case (concatenate 'string "&" (string ex))))))))

(DEFMTRFUN ($readfile $ANY MDEFINE NIL NIL) 
           ($FILENAME) 
           ((LAMBDA ($A) 
(with-open-file
    (l (print-invert-case (stripdollar $filename))
       :direction :input :if-does-not-exist :error)
   (do ((line	(read-line l nil nil)
		(read-line l nil nil)))
	((not line) (return nil))
	(setq $a ($cons
			(process-line line)
			$a))
   ))
                    $A)
            (LIST (QUOTE (MLIST)))))


;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(load-macsyma-macros mrgmac)

;;  qual-lisp file. Used by qual.mac.


(defun $lesses (x)
  (setq x (dinternp x))
  (do ((l (getf (symbol-plist x) 'data nil) (cdr l)) (nl))
      ((null l) (cons '(mlist) nl))
    (if (and (eq 'mgrp (caaar l)) (eq x (caddar (car l))))
	(setq nl (cons (doutern (cadaar l)) nl)))))

(defun $leqs (x)
  (setq x (dinternp x))
  (do ((l (getf (symbol-plist x) 'data nil) (cdr l)) (nl))
      ((null l) (cons '(mlist) nl))
    (if (and (eq 'mgqp (caaar l)) (eq x (caddar (car l))))
	(setq nl (cons (doutern (cadaar l)) nl)))))

(defun $greaters (x)
  (setq x (dinternp x))
  (do ((l (getf (symbol-plist x) 'data nil) (cdr l)) (nl))
      ((null l) (cons '(mlist) nl))
    (if (and (eq 'mgrp (caaar l)) (eq x (cadaar l)))
	(setq nl (cons (doutern (caddar (car l))) nl)))))

(defun $geqs (x)
  (setq x (dinternp x))
  (do ((l (getf (symbol-plist x) 'data nil) (cdr l)) (nl))
      ((null l) (cons '(mlist) nl))
    (if (and (eq 'mgqp (caaar l)) (eq x (cadaar l)))
	(setq nl (cons (doutern (caddar (car l))) nl)))))

(defun $equals (x)
  (setq x (dinternp x))
  (do ((l (getf (symbol-plist x) 'data nil) (cdr l)) (nl))
      ((null l) (cons '(mlist) nl))
    (cond ((not (eq 'meqp (caaar l))))
	  ((eq x (cadaar l)) (setq nl (cons (doutern (caddar (car l))) nl)))
	  (t (setq nl (cons (doutern (cadaar l)) nl))))))

(defun $neqs (x)
  (setq x (dinternp x))
  (do ((l (getf (symbol-plist x) 'data nil) (cdr l)) (nl))
      ((null l) (cons '(mlist) nl))
    (cond ((not (eq 'mnqp (caaar l))))
	  ((eq x (cadaar l)) (setq nl (cons (doutern (caddar (car l))) nl)))
	  (t (setq nl (cons (doutern (cadaar l)) nl))))))


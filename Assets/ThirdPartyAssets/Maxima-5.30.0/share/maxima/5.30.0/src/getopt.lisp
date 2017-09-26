;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: getopt -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.lisp
;;;; Purpose:       Command line option processing like GNU's getopt_long
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id: getopt.lisp,v 1.1 2009-07-13 15:25:24 rtoy Exp $
;;;;
;;;; *************************************************************************

;;;; This file has been modified from the original to support the
;;;; needs of maxima.  Basically, we changed getopt so that:
;;;;
;;;;  - "-ab" is recognized as two separate options: "-a" "-b"
;;;;
;;;;  - Exact matches are treated as matches, even if the match is an
;;;;    ambiguous prefix.  Hence, "--batch" will match the option
;;;;    "--batch", even though it is an ambiguous prefix for
;;;;    "--batch-lisp" and "--batch--string".  But "--bat" is still an
;;;;    error since it is ambiguous and is not an exact match for any
;;;;    option.
;;;;
;;;; To comply with the license, we include the license here:
;;;;
;;;; *************************************************************************
;;;; Copyright (C) 2003 by Kevin M. Rosenberg.
;;;; 
;;;; All rights reserved.
;;;; 
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 3. Neither the name of the author nor the names of the contributors
;;;;    may be used to endorse or promote products derived from this software
;;;;    without specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.
;;;; *************************************************************************


(in-package #-gcl #:getopt #+gcl "GETOPT")


(defun is-short-option (arg)
  (and (>= (length arg) 2)
       (char= #\- (schar arg 0))
       (char/= #\- (schar arg 1))))

(defun is-option-terminator (arg)
  (and (= 2 (length arg))
       (char= #\- (schar arg 0))
       (char= #\- (schar arg 1))))

(defun is-long-option (arg)
  (and (> (length arg) 2)
       (char= #\- (schar arg 0))
       (char= #\- (schar arg 1))
       (char/= #\- (schar arg 2))))

(defun decompose-arg (arg option-type)
  "Returns base-name,argument"
  (let ((start (ecase option-type
                 (:long 2)
                 (:short 1)))
        (name-end (position #\= arg)))

    (values (subseq arg start name-end)
            (when name-end (subseq arg (1+ name-end))))))

(defun analyze-arg (arg)
  "Analyzes an argument. Returns option-type,base-name,argument"
  (let* ((option-type (cond ((is-short-option arg) :short)
                            ((is-long-option arg) :long)
                            (t :arg))))
    (if (or (eq option-type :short) (eq option-type :long))
        (multiple-value-bind (base arg) (decompose-arg arg option-type)
          (values option-type base arg))
        (values :arg arg nil))))


(defun find-option (name options &key allow-exact-match)
  "Find an option in option list. Handles using unique abbreviations"
  (let* ((option-names (mapcar #'car options))
         (pos (match-unique-abbreviation name option-names :allow-exact-match allow-exact-match)))
    (when pos
      (nth pos options))))

(defun match-option (arg options &key allow-exact-match)
  "Matches an argument to an option. Returns option-list,option-type,base-name,argument"
  (multiple-value-bind (option-type base-name argument) (analyze-arg arg)
    (let ((match (find-option base-name options :allow-exact-match allow-exact-match)))
      (values match option-type (when match (car match)) argument))))


;;; EXPORTED functions

(defun match-unique-abbreviation (abbr strings &key (allow-exact-match nil))
  "Returns position of ABBR in STRINGS. ABBR may be a unique abbreviation.
Returns NIL if no match found."
  (let ((len (length abbr))
        (matches nil))
    (dotimes (i (length strings))
      (let* ((s (nth i strings))
             (l (length s)))
        (cond
          ((= len l)
           (when (string= abbr s)
	     (if allow-exact-match
		 (return-from match-unique-abbreviation i)
		 (push (cons s i) matches))))
          ((< len l)
           (when (string= abbr (subseq s 0 len))
             (push (cons s i) matches))))))
    (when (= 1 (length matches))
      (cdr (first matches)))))

(defun getopt (args options &key allow-exact-match)
  "Processes a list of arguments and options. Returns three values:
 - Non-option arguments
 - An alist of options consisting of the option name and the value, if any
 - A list of any option names that were not recognized

options is a list of option lists. The fields of the list are
 - NAME name of the long option
 - HAS-ARG with legal values of :NONE, :REQUIRED, :OPTIONAL
 - VAL value to return for a option with no arguments"
  (do ((pos args (cdr pos))
       (finished-options)
       (out-opts)
       (out-args)
       (errors))
      ((null pos) (values (nreverse out-args) (nreverse out-opts) errors))
    (cond
     (finished-options
      (push (car pos) out-args))
     ((is-option-terminator (car pos))
      (setq finished-options t))
     (t
      (let ((arg (car pos)))
        (multiple-value-bind (option-list option-type base-name argument)
            (match-option (car pos) options :allow-exact-match allow-exact-match)
          (cond
            ((and option-list (not (eq option-type :arg)))
             (cond
               (argument
                (case (second option-list)
                  (:none
                   (push base-name errors))
                  (t
                   (push (cons base-name argument) out-opts))))
               ((null argument)
                (if (and (eq :required (second option-list)) (null (cdr pos)))
                    (push base-name errors)
		    (case (second option-list)
		      (:none
		       (push (cons base-name (third option-list)) out-opts))
		      (:required
		       ;; Next arg is the value.  
		       (push (cons base-name (second pos)) out-opts)
		       (setf pos (cdr pos)))
		      (:optional
		       ;; Optional arg.  If the next arg is an option
		       ;; arg, we use the default value. Otherwise we
		       ;; use the next arg as the value.
		       (if (or (is-short-option (second pos))
			       (is-long-option (second pos)))
			   (push (cons base-name (third option-list)) out-opts)
			   (progn
			     (push (cons base-name (second pos)) out-opts)
			     (setf pos (cdr pos))))))))))
            (t
	     (cond ((eq :long option-type)
		    (push (nth-value 0 (decompose-arg arg option-type)) errors))
		   ((eq :short option-type)
		    (cond ((<= (length (car pos)) 2)
			   ;; Unrecognized short option (one character)
			   (push (nth-value 0 (decompose-arg arg option-type)) errors))
			  (t
			   ;; We have option that's not matched, but
			   ;; looks like a short option like "-abc".
			   ;; Expand this to '("-a" "-b" "-c") and
			   ;; effectively replace "-abc" with the
			   ;; replacement.  We setf the cdr because
			   ;; the do loop will remove "-abc" for us.
			   (setf (cdr pos)
				 (append (map 'list
					      #'(lambda (x)
						  (concatenate 'string "-" (string x)))
					      (subseq (car pos) 1))
					 (cdr pos))))))
		   (t
		    (push arg out-args)))))))))))


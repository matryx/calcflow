;; eval_string.lisp -- parse a string as an expression and evaluate it, or just parse it
;;
;; Copyright (C) 2005 Robert Dodier
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; eval_string (s)  --  parse the string s as a Maxima expression and evaluate it.
;; It may or may not have a terminator (dollar sign `$' or semicolon `;').
;; Only the first expression is parsed and evaluated, if there is more than one.
;; e.g.
;; eval_string ("foo: 42; bar: foo^2 + baz")  =>  42
;; eval_string ("(foo: 42, bar: foo^2 + baz)")  =>  baz + 1764
;; Complain if s is not a string.

(defun $eval_string (s)
  (cond
    ((stringp s)
     (meval (parse-string s)))
    (t
      (merror "eval_string: ~M is not a string." s))))

;; parse_string (s)  --  parse the string s as a Maxima expression (do not evaluate it).
;; It may or may not have a terminator (dollar sign `$' or semicolon `;').
;; Only the first expression is parsed, if there is more than one.
;; e.g.
;; parse_string ("foo: 42; bar: foo^2 + baz")  =>  foo : 42
;; parse_string ("(foo: 42, bar: foo^2 + baz)")  =>  (foo : 42, bar : foo^2 + baz)
;; Complain if s is not a string.

(defun $parse_string (s)
  (cond
    ((stringp s)
     (parse-string s))
    (t
      (merror "parse_string: ~M is not a string." s))))

;; (PARSE-STRING S)  --  parse the string as a Maxima expression.
;; Do not evaluate the parsed expression.

(defun parse-string (s)
  (declare (special *mread-prompt*))
  (with-input-from-string
    (ss (ensure-terminator s))
    (third (let ((*mread-prompt*)) (mread ss)))))

;; (ENSURE-TERMINATOR S)  -- if the string S does not contain dollar sign `$' or semicolon `;'
;; then append a dollar sign to the end of S.

(defun ensure-terminator (s)
  (cond
    ((or (search "$" s :test #'char-equal) (search ";" s :test #'char-equal))
     s)
    (t
      (concatenate 'string s "$"))))

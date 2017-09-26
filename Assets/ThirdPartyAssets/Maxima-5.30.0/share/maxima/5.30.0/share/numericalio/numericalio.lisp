;;  Copyright 2005 by Robert Dodier

;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License.

;;  This program has NO WARRANTY, not even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

;; Read functions:
;;
;;   M: read_matrix (source, sep_ch_flag)
;;   read_matrix (source, M, sep_ch_flag)
;;   read_array (source, A, sep_ch_flag)
;;   read_hashed_array (source, A, sep_ch_flag)
;;   L: read_nested_list (source, sep_ch_flag)
;;   L: read_list (source, sep_ch_flag)
;;   read_list (source, L, sep_ch_flag)
;;
;;   read_binary_matrix (source, M)
;;   read_binary_array (source, A)
;;   L: read_binary_list (source)
;;   read_binary_list (source, L)
;;
;; `source' is a file name or input stream.
;;
;; Write functions:
;;
;; `sink' is a file name or output stream.
;;
;;   write_data (X, sink, sep_ch_flag)
;;   write_binary_data (X, sink)
;;
;; Helpers:
;;
;; byte_order_flag recognized values: msb, lsb
;;
;;   assume_external_byte_order (byte_order_flag)
;;

(defun $assume_external_byte_order (x)
  (cond
    ((eq x '$lsb)
     (define-external-byte-order :lsb))
    ((eq x '$msb)
     (define-external-byte-order :msb))
    (t
      (merror "assume_external_byte_order: unrecognized byte order flag: ~a" x))))

(defun lisp-or-declared-maxima-array-p (x)
  (or (arrayp x) (mget x 'array)))

;; THESE FILE-OPENING FUNCTIONS WANT TO BE MOVED TO STRINGPROC (HOME OF OTHER SUCH FUNCTIONS) !!

(defun $openw_binary (file)
   (open
      file
      :direction :output
      :if-exists :supersede
      :element-type '(unsigned-byte 8)
      :if-does-not-exist :create))

(defun $opena_binary (file)
   (open
      file
      :direction :output
      :if-exists :append
      :element-type '(unsigned-byte 8)
      :if-does-not-exist :create))

(defun $openr_binary (file) (open file :element-type '(unsigned-byte 8)))

;; -------------------- read functions --------------------

(defun $read_matrix (stream-or-filename &rest args)
  (if ($matrixp (car args))
    (let*
      ((M (car args))
       (sep-ch-flag (cadr args))
       (nrow (length (cdr M)))
       (ncol (if (> nrow 0) (length (cdadr M)) 0))
       (L ($read_list stream-or-filename sep-ch-flag (* nrow ncol))))
      (fill-matrix-from-list L M nrow ncol))
    (let ((sep-ch-flag (car args)))
      `(($matrix) ,@(cdr ($read_nested_list stream-or-filename sep-ch-flag))))))

(defun $read_binary_matrix (stream-or-filename M)
  (if ($matrixp M)
    (let*
      ((nrow (length (cdr M)))
       (ncol (if (> nrow 0) (length (cdadr M)) 0))
       (L ($read_binary_list stream-or-filename (* nrow ncol))))
      (fill-matrix-from-list L M nrow ncol))
    (merror "read_binary_matrix: expected a matrix, found ~a instead" (type-of M))))

(defun fill-matrix-from-list (L M nrow ncol)
  (let ((k 0))
    (dotimes (i nrow)
      (let ((row (nth (1+ i) M)))
        (dotimes (j ncol)
          (setf (nth (1+ j) row) (nth (1+ k) L))
          (setq k (1+ k))))))
  M)

(defun $read_array (file-name A &optional sep-ch-flag)
  (read-array file-name A sep-ch-flag 'text))

(defun $read_binary_array (file-name A)
  (read-array file-name A nil 'binary))

(defun read-array (file-name A sep-ch-flag mode)
  (if (lisp-or-declared-maxima-array-p A)
    (let*
      ((dimensions
         (if (arrayp A)
           (array-dimensions A)
           (cdr (third (cdr (mfuncall '$arrayinfo A))))))
       (n
         (apply #'* (mapcar #'(lambda (m) (1+ m)) dimensions)))
       (L
         (if (eq mode 'text)
           ($read_list file-name sep-ch-flag n)
           ($read_binary_list file-name n))))
      ($fillarray A L)
      '$done)
    (merror "read-array: expected a declared array, found ~a instead" (type-of A))))

(defun $read_hashed_array (stream-or-filename A &optional sep-ch-flag)
  (if (streamp stream-or-filename)
    (read-hashed-array-from-stream stream-or-filename A sep-ch-flag)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file (in file-name :if-does-not-exist nil)
        (if (not (null in))
          (read-hashed-array-from-stream in A sep-ch-flag)
          (merror "read_hashed_array no such file `~a'" file-name))))))

(defun read-hashed-array-from-stream (in A sep-ch-flag)
  (let (key L (sep-ch (get-input-sep-ch sep-ch-flag in)))
    (loop
      (setq L (read-line in nil 'eof))
      (if (eq L 'eof) (return))
      (setq L (make-mlist-from-string L sep-ch))
      (cond
        ((> ($length L) 0)
         (setq key ($first L))
         (if (= ($length L) 1)
           (arrstore (list (list A 'array) key) nil)
           (arrstore (list (list A 'array) key) ($rest L)))))))
  A)

(defun $read_nested_list (stream-or-filename &optional sep-ch-flag)
  (if (streamp stream-or-filename)
    (read-nested-list-from-stream stream-or-filename sep-ch-flag)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file (in file-name :if-does-not-exist nil)
        (if (not (null in))
          (read-nested-list-from-stream in sep-ch-flag)
          (merror "read_nested_list: no such file `~a'" file-name))))))

(defun read-nested-list-from-stream (in sep-ch-flag)
  (let (A L (sep-ch (get-input-sep-ch sep-ch-flag in)))
    (loop
      (setq L (read-line in nil 'eof))
      (if (eq L 'eof)
        (return (cons '(mlist simp) (nreverse A))))
      (setq A (cons (make-mlist-from-string L sep-ch) A)))))

(defun $read_list (stream-or-filename &rest args)
  (if ($listp (car args))
    (let*
      ((L (car args))
       (sep-ch-flag (cadr args))
       (n (or (caddr args) ($length L)))
       ;; Probably we could try to avoid creating a second list
       ;; by reading directly into the first one ...
       (L2 (read-list stream-or-filename sep-ch-flag 'text n)))
      (dotimes (i (length L2))
        (setf (nth i L) (nth i L2)))
      L)
    (if (integerp (car args))
      (let ((n (car args)))
        (read-list stream-or-filename nil 'text n))
      (let ((sep-ch-flag (car args)) (n (cadr args)))
        (read-list stream-or-filename sep-ch-flag 'text n)))))

(defun read-list (stream-or-filename sep-ch-flag mode n)
  (if (streamp stream-or-filename)
    (read-list-from-stream stream-or-filename sep-ch-flag mode n)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file
        (in file-name
            :if-does-not-exist nil
            :element-type (if (eq mode 'text) 'character '(unsigned-byte 8)))
        (if (not (null in))
          (read-list-from-stream in sep-ch-flag mode n)
          (merror "read_list: no such file `~a'" file-name))))))

(defun read-list-from-stream (in sep-ch-flag mode n)
  (let (A x (sep-ch (if (eq mode 'text) (get-input-sep-ch sep-ch-flag in))))
    (loop
      (if
        (or
          (and n (eq n 0))
          (eq (setq x (if (eq mode 'text) (parse-next-element in sep-ch) (read-float-64 in)))
              'eof))
        (return (cons '(mlist simp) (nreverse A))))
      (setq A (nconc (list x) A))
      (if n (decf n)))))

(defun $read_binary_list (stream-or-filename &rest args)
  (if ($listp (car args))
    (let*
      ((L (car args))
       (n (or (cadr args) ($length L)))
       ;; Probably we could try to avoid creating a second list
       ;; by reading directly into the first one ...
       (L2 (read-list stream-or-filename nil 'binary n)))
      (dotimes (i (length L2))
        (setf (nth i L) (nth i L2)))
      L)
    (let ((n (car args)))
      (read-list stream-or-filename nil 'binary n))))

(let (pushback-sep-ch)
  (defun parse-next-element (in sep-ch)
    (let
      ((*parse-stream* in)
       (sign 1)
       (initial-pos (file-position in))
       token
       found-sep-ch)
      (loop
        (if pushback-sep-ch
          (setq token pushback-sep-ch pushback-sep-ch nil)
          (setq token (scan-one-token-g t 'eof)))
        (cond
          ((eq token 'eof)
           (if found-sep-ch
             (return nil)
             (return 'eof)))
          ((and (eq token sep-ch) (not (eq sep-ch #\space)))
           (if (or found-sep-ch (eq initial-pos 0))
             (progn
               (setq pushback-sep-ch token)
               (return nil))
             (setq found-sep-ch token)))
          ((member token '($- $+))
           (setq sign (* sign (if (eq token '$-) -1 1))))
          (t
            (return (m* sign token))))))))

(defun make-mlist-from-string (s sep-ch)
  ; scan-one-token-g isn't happy with symbol at end of string.
  (setq s (concatenate 'string s " "))

  (with-input-from-string (*parse-stream* s)
    (let ((token) (L) (LL) (sign))
      (loop
        (setq token (scan-one-token-g t 'eof))
        (cond
          ((eq token 'eof)
           (cond
             ((not (null sign))
              (format t "numericalio: trailing sign (~S) at end of line; strange, but just eat it.~%" sign)))
           (cond
             ((eq sep-ch #\space)
              (return (cons '(mlist) LL)))
             (t
               (return (cons '(mlist) (appropriate-append L LL)))))))
        (cond
          ((or (eq token '$-) (eq token '$+))
           (setq sign (cond ((eq token '$-) -1) (t 1))))
          (t
            (cond
              ((not (null sign))
               (setq token (m* sign token))
               (setq sign nil)))
            (cond
              ((eq sep-ch #\space)
               (setq LL (append LL (list token))))
              (t
                (cond
                  ((eq token sep-ch)
                   (setq L (appropriate-append L LL))
                   (setq LL nil))
                  (t
                    (setq LL (append LL (list token)))))))))))))

(defun appropriate-append (L LL)
  (cond
    ((null LL) (append L '(nil)))
    ((= (length LL) 1) (append L LL))
    (t (append L (list (append '((mlist)) LL))))))

;; ----- begin backwards compatibility stuff ... sigh -----
(defun $read_lisp_array (file-name A &optional sep-ch-flag)
  ($read_array file-name A sep-ch-flag))

(defun $read_maxima_array (file-name A &optional sep-ch-flag)
  ($read_array file-name A sep-ch-flag))
;; -----  end backwards compatibility stuff ... sigh  -----


;; -------------------- write functions -------------------

(defun open-file-appropriately (file-name mode)
  (open file-name
        :direction :output
        :element-type (if (eq mode 'text) 'character '(unsigned-byte 8))
        :if-exists (if (or (eq $file_output_append '$true) (eq $file_output_append t)) :append :supersede)
        :if-does-not-exist :create))

(defun $write_data (X stream-or-filename &optional sep-ch-flag)
  (write-data X stream-or-filename sep-ch-flag 'text))

(defun $write_binary_data (X stream-or-filename)
  (write-data X stream-or-filename nil 'binary))

(defun write-data (X stream-or-filename sep-ch-flag mode)
  (let
    ((out
       (if (streamp stream-or-filename)
         stream-or-filename
         (open-file-appropriately (require-string stream-or-filename) mode))))
    (cond
      (($matrixp X)
        (write-matrix X out sep-ch-flag mode))
      ((arrayp X)
        (write-lisp-array X out sep-ch-flag mode))
      ((mget X 'array)
        (write-maxima-array X out sep-ch-flag mode))
      ((mget X 'hashar)
        (write-hashed-array X out sep-ch-flag mode))
      (($listp X)
        (write-list X out sep-ch-flag mode))
      (t (merror "write_data: don't know what to do with a ~M" (type-of X))))
    (if (streamp stream-or-filename)
      (finish-output out)
      (close out))
    '$done))

(defun write-matrix (M out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag out)))
    (mapcar #'(lambda (x) (write-list-lowlevel (cdr x) out sep-ch mode)) (cdr M))))

(defun write-lisp-array (A out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag out)) (d (array-dimensions A)))
    (write-lisp-array-helper A d '() out sep-ch mode)))

(defun write-lisp-array-helper (A d indices out sep-ch mode)
  (cond ((equalp (length d) 1)
      (let ((L '()))
        (loop for i from 0 to (- (car d) 1) do
          (let ((x (apply 'aref (append (list A) (reverse (cons i indices))))))
            (setq L (cons x L))))
        (write-list-lowlevel (reverse L) out sep-ch mode)))
    (t
      (loop for i from 0 to (- (car d) 1) do
        (write-lisp-array-helper A (cdr d) (cons i indices) out sep-ch mode)
        (if (and (eq mode 'text) (> (length d) 2))
          (terpri out))))))

(defun write-maxima-array (A out sep-ch-flag mode)
  (write-lisp-array (symbol-array (mget A 'array)) out sep-ch-flag mode))

(defun write-hashed-array (A out sep-ch-flag mode)
  (let
    ((keys (cdddr (meval (list '($arrayinfo) A))))
     (sep-ch (get-output-sep-ch sep-ch-flag out))
     L)
    (loop
      (if (not keys) (return))
      (setq L ($arrayapply A (car keys)))
      (cond ((listp L) (pop L))
            (t (setq L (list L))))
      (write-list-lowlevel (append (cdr (pop keys)) L) out sep-ch mode))))

(defun write-list (L out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag out)))
    (write-list-lowlevel (cdr L) out sep-ch mode)))

(defun write-list-lowlevel (L out sep-ch mode)
  (setq sep-ch (cond ((symbolp sep-ch) (cadr (exploden sep-ch))) (t sep-ch)))
  (cond ((not (null L))
      (loop 
        (if (not L) (return))
        (let ((e (pop L)))
          (cond (($listp e)
              (write-list-lowlevel (cdr e) out sep-ch mode))
            (t
              (cond
                ((eq mode 'text)
                 (mgrind e out)
                 (cond
                   ((null L) (terpri out))
                   (t (write-char sep-ch out))))
                ((eq mode 'binary)
                 (if ($numberp e)
                   (write-float ($float e) out)
                   (merror "write_data: encountered non-numeric data in binary output")))
                (t
                  (merror "write_data: unrecognized mode"))))))))))

(defun get-input-sep-ch (sep-ch-flag my-stream)
  (cond
    ((eq sep-ch-flag '$tab)
     (format t "numericalio: separator flag ``tab'' not recognized for input; assume ``space'' instead.~%")
     #\space)
    (t (get-output-sep-ch sep-ch-flag my-stream))))

(defun get-output-sep-ch (sep-ch-flag my-stream)
  (cond
    ((eq sep-ch-flag '$space) #\space)
    ((eq sep-ch-flag '$tab) #\tab)
    ((or (eq sep-ch-flag '$comma) (eq sep-ch-flag '$csv)) '$\,) ; '$csv is backwards compatibility ... sigh
    ((eq sep-ch-flag '$pipe) '$\|)
    ((eq sep-ch-flag '$semicolon) '$\;)

    ((null sep-ch-flag)
      (cond
        ((ignore-errors (equal (pathname-type (truename my-stream)) "csv"))
         '$\,)
        (t #\space)))
    (t
      (format t "numericalio: separator flag ~S not recognized; assume ``space''.~%" (stripdollar sep-ch-flag))
      #\space)))

(defun require-string (s)
  (cond
    ((stringp s)
     s)
    (t
      (merror "numericalio: expected a string, instead found a ~:M" (type-of s)))))

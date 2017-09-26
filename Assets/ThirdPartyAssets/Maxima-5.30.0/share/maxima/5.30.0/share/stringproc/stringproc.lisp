;;;;
;;;;                                   ~*~  STRINGPROC  ~*~
;;;;
;;;;  Maxima string processing
;;;;
;;;;  Version       : 3.2 (april 2008)
;;;;  Copyright     : 2005-2008 Volker van Nek
;;;;  Licence       : GPL2
;;;;
;;;;  Test file     : rteststringproc.mac
;;;;  Documentation : stringproc.texi
;;;;


;;  1. I/O
;;  2. characters
;;  3. strings

(in-package :maxima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  1. I/O


(defun $openw (file)
  (if (not (stringp file))
    (merror "openw: argument must be a path string."))
  (open file
        :direction :output
        :if-exists :supersede
        :if-does-not-exist :create))


(defun $opena (file)
  (if (not (stringp file))
    (merror "opena: argument must be a path string."))
  (open file
        :direction :output
        :if-exists :append
        :if-does-not-exist :create))


(defun $openr (file) 
  (cond ((not (stringp file))
           (merror "openr: argument must be a valid path string."))
        ((not (probe-file file))
           (merror "openr: file does not exist: ~m" file)))
  (open file))


(defun $make_string_input_stream (str &optional (start 1) (end nil))  ;; 1-based indexing!
  (if (not (stringp str))
    (merror "make_string_input_stream: first argument must be a string."))
  (or (ignore-errors ;; suppresses Lisp error outputs with internal 0-based indexing 
        (make-string-input-stream str (1- start) (if end (1- end))))
      (merror "make_string_input_stream: improper start or end index.")))


(defun $make_string_output_stream ()
  (make-string-output-stream))


;; Ignore :element-type keyword. 
;; So we get the default here, namely :element-type character.
(defun $get_output_stream_string (stream)
  (if (not (streamp stream))
    (merror "get_output_stream_string: argument must be a stream."))
  (get-output-stream-string stream))


(defun $close (stream) 
  (if (not (streamp stream))
    (merror "close: argument must be a stream."))
  (close stream))


(defun $flength (stream) 
  (if (not (streamp stream))
    (merror "flength: argument must be a stream."))
  (file-length stream))


(defun $fposition (stream &optional pos)
  (if (not (streamp stream))
    (merror "fposition: first argument must be a stream."))
  (or (ignore-errors 
        (if pos
          (file-position stream (1- pos))
          (1+ (file-position stream))))
      (merror "fposition: improper position index ~m" pos)))


(defun $readline (stream)
  (if (not (streamp stream))
    (merror "readline: argument must be a stream."))
  (let ((line (read-line stream nil nil)))
    (if line line)))


(defun $freshline (&optional (stream)) 
  (and stream (not (streamp stream))
    (merror "freshline: optional argument must be a stream."))
  (fresh-line stream))


(defun $newline (&optional (stream)) 
  (and stream (not (streamp stream))
    (merror "newline: optional argument must be a stream."))
  (terpri stream))


;;  (defun $tab () $tab) 
;;    moved the end of the character section; sbcl complained


;;  $printf now in printf.lisp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  2. characters

;;  converts a Maxima string of length 1 into a Lisp character
;;
(defun $lchar (mc) ;; at Maxima level only for testing
  (l-char mc))
;;
(defun l-char (mc)
  (if (not ($charp mc))
    (merror "stringproc: ~m is no Maxima character." mc))
  (character mc))


;;  converts a Lisp character into a Maxima string of length 1
;;
(defun $cunlisp (c) ;; at Maxima level only for testing
  (if (not (characterp c))
    (merror "cunlisp: argument must be a Lisp character"))
  (m-char c))
;;
(defun m-char (c) (make-string 1 :initial-element c))


;;  tests, if object is Lisp character
(defun $lcharp (obj) (characterp obj)) ;; for testing only


;;  tests, if object is Maxima character
(defun $charp (obj)
  (and (stringp obj) (= 1 (length obj))))


;;  tests for different Maxima characters at Maxima level (Lisp level see $tokens below)
;;
(defun $constituent (mc)   (constituent (l-char mc)))
(defun $alphanumericp (mc) (alphanumericp (l-char mc)))
(defun $alphacharp (mc)    (alpha-char-p (l-char mc)))
(defun $lowercasep (mc)    (lower-case-p (l-char mc)))
(defun $uppercasep (mc)    (upper-case-p (l-char mc)))
;;
(defun $digitcharp (mc)
  (let ((nr (char-int (l-char mc))))
    (and (> nr 47) (< nr 58))))


;;  ascii-char <-> index
;;
(defun $cint (mc) 
  (if (not ($charp mc))
    (merror "cint: argument must be a Maxima character."))
  (char-code (character mc)))
;;
(defun $ascii (int) 
  (if (or (not (integerp int)) (< int 0) (> int 255))
    (merror "ascii: argument must be zero or a positve integer less than 256."))
  (m-char (code-char int)))


;;  comparison - test functions - at Maxima level
;;
(defun $cequal          (mc1 mc2)  (char=         (l-char mc1) (l-char mc2)))
(defun $cequalignore    (mc1 mc2)  (char-equal    (l-char mc1) (l-char mc2)))
(defun $clessp          (mc1 mc2)  (char<         (l-char mc1) (l-char mc2)))
(defun $clesspignore    (mc1 mc2)  (char-lessp    (l-char mc1) (l-char mc2)))
(defun $cgreaterp       (mc1 mc2)  (char>         (l-char mc1) (l-char mc2)))
(defun $cgreaterpignore (mc1 mc2)  (char-greaterp (l-char mc1) (l-char mc2)))


;;  comparison - test functions - at Lisp level
;;
(defun cequal          (c1 c2)  (char=         c1 c2))
(defun cequalignore    (c1 c2)  (char-equal    c1 c2))
(defun clessp          (c1 c2)  (char<         c1 c2))
(defun clesspignore    (c1 c2)  (char-lessp    c1 c2))
(defun cgreaterp       (c1 c2)  (char>         c1 c2))
(defun cgreaterpignore (c1 c2)  (char-greaterp c1 c2))


;; special Maxima characters
;;
(defmvar $newline
  (m-char #\newline) "Maxima newline character" string)
;;  
(defmvar $tab
  (m-char #\tab)     "Maxima tab character"     string)
;;  
(defmvar $space
  (m-char #\space)   "Maxima space character"   string)


(defun $tab () $tab) ;; returns Maxima tab character; can be autoloaded

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  3. strings


;;  tests, if object is a string
(defun $stringp (obj) (stringp obj))


;;  copy
(defun $scopy (s) 
  (if (not (stringp s))
    (merror "scopy: argument must be a string."))
  (copy-seq s))


;;  make
(defun $smake (n mc)
  (cond ((not (integerp n))
           (merror "smake: first argument must be an integer."))
        ((not ($charp mc))
           (merror "smake: second argument must be a Maxima character.")))
  (make-string n :initial-element (character mc)))


;;  returns a Maxima character = Maxima string of length 1
(defun $charat (str index) ;; 1-based indexing!
  (if (not (stringp str))
    (merror "charat: first argument must be a string."))
  (or (ignore-errors 
        (subseq str (1- index) index))
      (merror "charat: improper index ~m" index)))


(defun $charlist (s)
  (if (not (stringp s))
    (merror "charlist: argument must be a string."))
  (cons '(mlist) (mapcar #'(lambda (c) (m-char c)) (coerce s 'list))))


(putprop '$sexplode '$charlist 'alias)


;;  $tokens implements Paul Grahams function tokens in Maxima
;;
(defun $tokens (str &optional (test '$constituent))
  (cond ((not (stringp str))
           (merror "tokens: first argument must be a string."))
          ((not (member test '($constituent $alphanumericp $alphacharp $digitcharp $lowercasep $uppercasep $charp)))
           (merror "tokens: optional second argument must be one of ~%
              `constituent', `alphanumericp', `alphacharp', `digitcharp', `lowercasep', `uppercasep', `charp'.")))
  (cons '(mlist) (tokens str (stripdollar test) 0)))
;;
(defun tokens (str test start) ;; Author: Paul Graham - ANSI Common Lisp, 1996, page 67
  (let ((pos1 (position-if test str :start start)))
    (if pos1
      (let ((pos2 (position-if #'(lambda (ch) (not (funcall test ch)))
                               str 
                               :start pos1)))
        (cons (subseq str pos1 pos2)
              (if pos2
                (tokens str test pos2)
                nil)))
      nil)))

;;  test functions for $tokens on Lisp level:
(defun constituent (ch) ;; Author: Paul Graham - ANSI Common Lisp, 1996, page 67
  (and (graphic-char-p ch)
       (not (char= ch #\  ))))

(defun alphacharp    (ch)  (alpha-char-p ch))
(defun digitcharp    (ch)  (digit-char-p ch))
(defun lowercasep    (ch)  (lower-case-p ch))
(defun uppercasep    (ch)  (upper-case-p ch))
(defun charp         (ch)  (characterp   ch))
;;     characterp    (ch)
;;     alphanumericp (ch)


;;  splits string at an optional user defined delimiter character
;;  optional flag for multiple delimiter chars
(defun $split (str &optional (ds " ") (multiple? t))
  (cond ((not (stringp str))
           (merror "split: first argument must be a string."))
        ((not (stringp ds))
           (merror "split: optional second argument must be a character or string."))
        ((not (member multiple? '(t nil)))
           (merror "split: optional third argument must be `true' or `false'.")))
  (if (string= ds "")
    ($charlist str)
    (cons '(mlist) (split str ds multiple?))))
;;
(defun split (str ds &optional (multiple? t))
  (let ((pos1 (search ds str)))
     (if pos1
       (let ((ss (subseq str 0 pos1)))
         (if (and multiple? (string= ss ""))
           (split1 str ds multiple? pos1)
           (cons ss (split1 str ds multiple? pos1))))
       (list str))))
;;
(defun split1 (str ds multiple? start)
  (let ((pos1 (search ds str :start2 start)))
    (if pos1
      (let* ((pos2 (search ds str :start2 (+ pos1 (length ds))))
             (ss (subseq str (+ pos1 (length ds)) pos2)))
        (if (and multiple? (string= ss ""))
        (if pos2 (split1 str ds multiple? pos2) nil)
        (cons ss (if pos2 (split1 str ds multiple? pos2) nil))))
      nil)))


;;  $sconcat for lists, allows an optional user defined separator string
;;  returns maxima-string
(defun $simplode (li &optional (ds ""))
  (cond ((not (listp li))
           (merror "simplode: first argument must be a list."))
        ((not (stringp ds))
           (merror "simplode: optional second argument must be a string.")))
  (setq li (cdr li))
  (let ((res ""))
     (dolist (str li)
        (setq res (concatenate 'string res ($sconcat str) ds)))
     (string-right-trim ds res)))



(defun $slength (s)
  (if (not (stringp s))
    (merror "slength: argument must be a string."))
  (length s))


(defun $sposition (mc s) ;; 1-based indexing!
  (cond ((not ($charp mc))
           (merror "sposition: first argument must be a Maxima character."))
        ((not (stringp s))
           (merror "sposition: second argument must be a string.")))
  (let ((pos (position (character mc) s)))
    (if pos (1+ pos))))


(defun $sreverse (s)
  (if (not (stringp s))
    (merror "sreverse: argument must be a string."))
  (reverse s))


(defun $substring (str start &optional (end nil)) ;; 1-based indexing!
  (if (not (stringp str))
    (merror "substring: first argument must be a string."))
  (or (ignore-errors 
        (subseq str (1- start) (if end (1- end))))
      (merror "substring: improper start or end index.")))

;;  comparison - test functions - at Maxima level

(defun $sequalignore (s1 s2)
  (if (or (not (stringp s1)) (not (stringp s2)))
    (merror "sequalignore: both arguments must be strings."))
  (string-equal s1 s2))

(defun $sequal (s1 s2)
  (if (or (not (stringp s1)) (not (stringp s2)))
    (merror "sequal: both arguments must be strings."))
  (string= s1 s2))


;;  comparison - test functions - at Lisp level

;; args can also be characters
(defun sequalignore (s1 s2) (string-equal s1 s2))
(defun sequal (s1 s2)       (string= s1 s2))


(defun $smismatch (s1 s2 &optional (test '$sequal))  ;; 1-based indexing!
  (cond ((or (not (stringp s1)) (not (stringp s2)))
           (merror "smismatch: first two arguments must be strings."))
          ((not (member test '($sequal $sequalignore $cequal $cequalignore)))
           (merror "smismatch: optional second argument must be be `sequal(ignore)' or `cequal(ignore)'")))
  (let ((pos (mismatch s1 s2 :test (stripdollar test))))
     (if pos (1+ pos)) ))


;;  searching

(defun $ssearch (seq str &rest args)  ;; 1-based indexing!
  (if (not (and (stringp seq) (stringp str)))
    (merror "ssearch: first two arguments must be strings."))
  (setq args
    (stringproc-optional-args "ssearch" args))
  (or (ignore-errors 
        (let ((index (meval `(ssearch ,seq ,str ,@args))))
          (if index 
            index
            (return-from $ssearch nil))))
      (merror "ssearch: improper arguments.")))
;;
(defun ssearch (seq str &optional (test '$sequal) (start 1) (end nil))
  (let ((pos (search seq str :test (stripdollar test) :start2 (1- start) :end2 (if end (1- end)))))
    (if pos (1+ pos))))


;; helper:
;; allows flexible sequence of optional args (test, start, end)
(defun stringproc-optional-args (name args) 
  (let ((test '$sequal) 
        (start 1)  ;; 1-based indexing!
        (end nil) 
        (tests '($sequal $sequalignore $cequal $cequalignore))
        arg1 arg2 arg3)
    (if args 
      (progn
        (setq arg1 (car args))
        (cond ((and (integerp arg1) (> arg1 0))  (setq start arg1))
              ((member arg1 tests)               (setq test arg1))
              (t (merror "~m: improper argument ~m" name arg1)))
        (setq args (cdr args))))
    (if args 
      (progn
        (setq arg2 (car args))
        (cond ((and (integerp arg1) (or (integerp arg2) (null arg2)))  (setq end arg2))
              ((and (integerp arg2) (> arg2 0))                        (setq start arg2))
              ((member arg2 tests)                                     (setq test arg2))
              (t (merror "~m: improper argument ~m" name arg2)))
        (setq args (cdr args))))
    (if args 
      (progn
        (setq arg3 (car args))
        (cond ((or (integerp arg3) (null arg3))  (setq end arg3))
              ((member arg3 tests)               (setq test arg3))
              (t (merror "~m: improper argument ~m" name arg3)))))
   (list test start end)))


;;  functions for string manipulation

(defun $ssubstfirst (new old str &rest args)  ;; 1-based indexing!
  (if (not (and (stringp new) (stringp old) (stringp str)))
    (merror "ssubstfirst: first three arguments must be strings."))
  (setq args 
    (stringproc-optional-args "ssubstfirst" args))
  (or (ignore-errors 
        (meval `(ssubstfirst ,new ,old ,str ,@args)))
      (merror "ssubstfirst: improper arguments.")))
;;
(defun ssubstfirst (new old str &optional (test '$sequal) (start 1) (end nil))
  (let ((len (length old))
        (pos (search old 
                     str
                     :test (stripdollar test) ;; call to function at Lisp level (char by char)
                     :start2 (1- start)
                     :end2 (if end (1- end)))))
    (if (null pos)
      str
      (concatenate 'string (subseq str 0 pos) new (subseq str (+ pos len))))))
       

(defun $ssubst (new old str &rest args)  ;; 1-based indexing!
  (if (not (and (stringp new) (stringp old) (stringp str)))
    (merror "ssubst: first three arguments must be strings."))
  (setq args 
    (stringproc-optional-args "ssubst" args))
  (or (ignore-errors 
        (meval `(ssubst ,new ,old ,str ,@args)))
      (merror "ssubst: improper arguments.")))
;;
(defun ssubst (new old str &optional (test '$sequal) (start 1) (end nil))
  (let ((pos (search old 
                     str 
                     :test (stripdollar test)
                     :start2 (1- start)
                     :end2 (if end (1- end)))))
    (if (null pos)
       str
       (ssubst new
               old
               (ssubstfirst new old str test (+ pos 1) end)
               test
               (+ pos 1 (length new)) 
               (if end (+ end (length new) (- (length old))))) )))


(defun $sremovefirst (seq str &rest args)  ;; 1-based indexing!
  (if (not (and (stringp seq) (stringp str)))
    (merror "sremovefirst: first two arguments must be strings."))
  (setq args 
    (stringproc-optional-args "sremovefirst" args))
  (or (ignore-errors 
        (meval `(sremovefirst ,seq ,str ,@args)))
      (merror "sremovefirst: improper arguments.")))
;;
(defun sremovefirst (seq str &optional (test '$sequal) (start 1) (end nil))
  (let* ((len (length seq))
         (pos (search seq 
                      str
                      :test (stripdollar test)
                      :start2 (1- start)
                      :end2 (if end (1- end))))
         (sq1 (subseq str 0 pos))
         (sq2 (if pos (subseq str (+ pos len)) "")))
    (concatenate 'string sq1 sq2)))


(defun $sremove (seq str &rest args)  ;; 1-based indexing!
  (if (not (and (stringp seq) (stringp str)))
    (merror "sremove: first two arguments must be strings."))
  (setq args 
    (stringproc-optional-args "sremove" args))
  (or (ignore-errors 
        (meval `(sremove ,seq ,str ,@args)))
      (merror "sremove: improper arguments.")))
;;
(defun sremove (seq str &optional (test '$sequal) (start 1) (end nil))
  (if end (decf end))
  (let ((pos (search seq str :test (stripdollar test) :start2 (1- start) :end2 end)))
    (do () ((null pos) str)
      (progn
        (setq str (sremovefirst seq str test (1+ pos) (if end (1+ end))))
        (if end (setq end (- end (length seq))))
        (setq pos (search seq str :test (stripdollar test) :start2 pos :end2 end))  ))))


(defun $sinsert (seq str pos)  ;; 1-based indexing!
  (if (not (and (stringp seq) (stringp str)))
    (merror "sinsert: first two arguments must be strings."))
  (or (ignore-errors 
        (let ((sq1 (subseq str 0 (1- pos)))
              (sq2 (subseq str (1- pos))))
          (concatenate 'string sq1 seq sq2)))
      (merror "sinsert: improper position index ~m" pos)))


(defun $ssort (str &optional (test '$clessp))
  (cond ((not (stringp str))
           (merror "ssort: first argument must be a string."))
          ((not (member test '($clessp $cgreaterp $cequal $clesspignore $cgreaterpignore $cequalignore)))
           (merror "ssort: optional second argument must be `clessp', `cgreaterp', `cequal' or `clesspignore' ...")))
  (let ((copy (copy-seq str)))
     (stable-sort copy (stripdollar test))))


(defun $strim (seq str)
  (if (or (not (stringp seq)) (not (stringp str)))
    (merror "strim: both arguments must be strings."))
  (string-trim seq str))

(defun $striml (seq str)
  (if (or (not (stringp seq)) (not (stringp str)))
    (merror "striml: both arguments must be strings."))
  (string-left-trim seq str))

(defun $strimr (seq str)
  (if (or (not (stringp seq)) (not (stringp str)))
    (merror "strimr: both arguments must be strings."))
  (string-right-trim seq str))


(defun $supcase (str &optional (start 1) (end nil))  ;; 1-based indexing!
  (if (not (stringp str))
    (merror "supcase: first argument must be a string."))
  (or (ignore-errors 
        (string-upcase str :start (1- start) :end (if end (1- end))))
      (merror "supcase: improper start or end index.")))


(defun $sdowncase (str &optional (start 1) (end nil))  ;; 1-based indexing!
  (if (not (stringp str))
    (merror "sdowncase: first argument must be a string."))
  (or (ignore-errors 
        (string-downcase str :start (1- start) :end (if end (1- end))))
      (merror "sdowncase: improper start or end index.")))


(defun $sinvertcase (str &optional (start 1) (end nil))  ;; 1-based indexing!
  (if (not (stringp str))
    (merror "sinvertcase: first argument must be a string."))
  (decf start)
  (if end (decf end))
  (or (ignore-errors 
        (let ((s1 (subseq str 0 start))
              (s2 (subseq str start end))
              (s3 (if end (subseq str end) "")))
          (concatenate 'string s1 (invert-string-case s2) s3)))
      (merror "sinvertcase: improper start or end index.")))


(defun invert-char (ch) 
  (setf ch (character ch))
  (if (upper-case-p ch)
    (char-downcase ch)
     (char-upcase ch)))


(defun invert-string-case (str)
  (let* ((cl1 (explode str))
         (cl2 (cdr (butlast cl1))))
    (concatenate 'string (map 'list #'invert-char cl2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


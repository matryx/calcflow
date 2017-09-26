;; amatrix.lisp -- implement Maxima matrix via underlying Lisp array
;; copyright 2007 by Robert Dodier
;; I release this file under the terms of the GNU General Public License.

(setf (get '$amatrix '$present) t)

(declare-top (unspecial x))

(defun $amatrixmap (f &rest m)
  (let*
    ((nr ($nrows (car m)))
     (nc ($ncols (car m)))
     (m1 (mfuncall '$make_matrix nr nc))
     (a1 (get ($@-function m1 '$storage) 'storage_array)))
    (dotimes (i nr)
      (dotimes (j nc)
        (let*
          ((args
             (mapcar
               #'(lambda (m0)
                   (let
                     ((a0 (get ($@-function m0 '$storage) 'storage_array))
                      (ij (mfuncall '$compute_index0 m0 i j)))
                     (aref a0 ij)))
               m))
           (x (mapply1 f args f nil)))
          (setf (aref a1 (mfuncall '$compute_index0 m1 i j)) x))))
    m1))

(putprop '$amatrix 'amatrix-assign 'mset_extension_operator)

(defun amatrix-assign (e x)
  (let
    ((my-amatrix-name (caar e))
     (my-amatrix (symbol-value (caar e))))
    (cond
      ((= (length e) 2)
       (cond
         ((= ($nrows my-amatrix) 1)
          (amatrix-assign1 my-amatrix-name my-amatrix 1 (meval (cadr e)) x))
         ((= ($ncols my-amatrix) 1)
          (amatrix-assign1 my-amatrix-name my-amatrix (meval (cadr e)) 1 x))
         (t
           (merror "amatrix assignment: given one subscript, but expected two"))))
      ((= (length e) 3)
       (amatrix-assign1 (caar e) (symbol-value (caar e)) (meval (cadr e)) (meval (caddr e)) x))
      (t
        (merror "amatrix assignment: expected one or two subscripts")))))

(defun amatrix-assign1 (lhs aa i j x)
  (when (> (get ($@-function aa '$storage) 'refcount) 1)
    (let ((a (gensym)))
      (putprop a 1 'refcount)
      (putprop a (mfuncall '$copy_array (get ($@-function aa '$storage) 'storage_array)) 'storage_array)
      (decf (get ($@-function aa '$storage) 'refcount))
      (mrecord-assign `(($@) ,lhs $storage) a)))
  (cond
    ((and (integerp i) (integerp j))
     (amatrix-assign1-row-column aa i j x))
    ((and (eq i '$all) (integerp j))
     (amatrix-assign1-all-column aa j x))
    ((and (integerp i) (eq j '$all))
     (amatrix-assign1-row-all aa i x))
    ((and (eq i '$all) (eq j '$all))
     (amatrix-assign1-all-all aa x))
    ((and ($listp i) (integerp j))
     (amatrix-assign1-list-integer aa i j x))
    ((and (integerp i) ($listp j))
     (amatrix-assign1-integer-list aa i j x))
    ((and ($listp i) (eq j '$all))
     (amatrix-assign1-list-all aa i x))
    ((and (eq i '$all) ($listp j))
     (amatrix-assign1-all-list aa j x))
    ((and ($listp i) ($listp j))
     (amatrix-assign1-list-list aa i j x))
    (t
      `((mset) ((,aa array) ,i ,j) ,x))))

(defun amatrix-assign1-row-column (aa i j x)
  (let
    ((a (get ($@-function aa '$storage) 'storage_array))
     (ij (1- (mfuncall '$compute_index1 aa i j))))
    (setf (aref a ij) x)))

(defun amatrix-assign1-all-column (aa j x)
  (let ((m ($@-function aa '$nr)))
    (if ($amatrixp x)
      ;; MIGHT WANT TO ENSURE THAT X HAS SAME NUMBER OF ROWS AS AA AND EXACTLY ONE COLUMN 
      (dotimes (i m)
        (amatrix-assign1-row-column aa (1+ i) j (mfuncall '$get_element x (1+ i) 1)))
      (dotimes (i m)
        (amatrix-assign1-row-column aa (1+ i) j x))))
  x)

(defun amatrix-assign1-row-all (aa i x)
  (let ((n ($@-function aa '$nc)))
    (if ($amatrixp x)
      ;; MIGHT WANT TO ENSURE THAT X HAS EXACTLY ONE ROW AND SAME NUMBER OF COLUMNS AS AA
      (dotimes (j n)
        (amatrix-assign1-row-column aa i (1+ j) (mfuncall '$get_element x 1 (1+ j))))
      (dotimes (j n)
        (amatrix-assign1-row-column aa i (1+ j) x))))
  x)

(defun amatrix-assign1-all-all (aa x)
  (let
    ((m ($@-function aa '$nr))
     (n ($@-function aa '$nc)))
    (if ($amatrixp x)
      ;; MIGHT WANT TO ENSURE THAT X HAS SAME NUMBER OF ROWS AND COLUMNS AS AA
      (dotimes (i m)
        (dotimes (j n)
          (amatrix-assign1-row-column aa (1+ i) (1+ j) (mfuncall '$get_element x (1+ i) (1+ j)))))
      (dotimes (i m)
        (dotimes (j n)
          (amatrix-assign1-row-column aa (1+ i) (1+ j) x)))))
  x)

(defun amatrix-assign1-list-integer (aa l j x)
  (amatrix-assign1-list-list aa l `((mlist) ,j) x))

(defun amatrix-assign1-integer-list (aa i l x)
  (amatrix-assign1-list-list aa `((mlist) ,i) l x))

(defun amatrix-assign1-list-all (aa l x)
  (let
    ((m ($length l))
     (n ($@-function aa '$nc)))
    (if ($amatrixp x)
      ;; MIGHT WANT TO ENSURE THAT X HAS SAME NUMBER OF ROWS AND COLUMNS AS AA
      (dotimes (i m)
        (dotimes (j n)
          (amatrix-assign1-row-column aa (nth (1+ i) l) (1+ j) (mfuncall '$get_element x (1+ i) (1+ j)))))
      (dotimes (i m)
        (dotimes (j n)
          (amatrix-assign1-row-column aa (nth (1+ i) l) (1+ j) x)))))
  x)

(defun amatrix-assign1-all-list (aa l x)
  (let
    ((m ($@-function aa '$nr))
     (n ($length l)))
    (if ($amatrixp x)
      ;; MIGHT WANT TO ENSURE THAT X HAS SAME NUMBER OF ROWS AND COLUMNS AS AA
      (dotimes (i m)
        (dotimes (j n)
          (amatrix-assign1-row-column aa (1+ i) (nth (1+ j) l) (mfuncall '$get_element x (1+ i) (1+ j)))))
      (dotimes (i m)
        (dotimes (j n)
          (amatrix-assign1-row-column aa (1+ i) (nth (1+ j) l) x)))))
  x)

(defun amatrix-assign1-list-list (aa l1 l2 x)
  (let
    ((m ($length l1))
     (n ($length l2)))
    (if ($amatrixp x)
      ;; MIGHT WANT TO ENSURE THAT X HAS SAME NUMBER OF ROWS AND COLUMNS AS AA
      (dotimes (i m)
        (dotimes (j n)
          (amatrix-assign1-row-column
            aa
            (nth (1+ i) l1)
            (nth (1+ j) l2)
            (mfuncall '$get_element x (1+ i) (1+ j)))))
      (dotimes (i m)
        (dotimes (j n)
          (amatrix-assign1-row-column
            aa
            (nth (1+ i) l1)
            (nth (1+ j) l2)
            x)))))
  x)

;; UNFINISHED STUFF. SORRY, I'M WORKING ON IT.

(defun amatrix-assign1-amatrix-column (aa ai j x) (declare (ignore aa ai j x)))

(defun amatrix-assign1-row-amatrix (aa i aj x) (declare (ignore aa i aj x)))

(defun amatrix-assign1-amatrix-amatrix (aa ai aj x) (declare (ignore aa ai aj x)))

(defun amatrix-assign1-boolean-column (aa b j x) (declare (ignore aa b j x)))

(defun amatrix-assign1-row-boolean (aa i b x) (declare (ignore aa i b x)))

(defun amatrix-assign1-boolean-boolean (aa b1 b2 x) (declare (ignore aa b1 b2 x)))

(displa-def $amatrix dim-$amatrix)

; CALL SIMPLIFYA IN DIM-$NEWMATRIX BECAUSE DIM-$MATRIX NEEDS TO SEE SIMP FLAG ... SIGH 

(defun dim-$amatrix (form result)
  (let
    ((nr ($@-function form '$nr))
     (r0 ($@-function form '$r0))
     (rinc ($@-function form '$rinc))
     (nc ($@-function form '$nc))
     (c0 ($@-function form '$c0))
     (cinc ($@-function form '$cinc))
     (storage ($@-function form '$storage)))
    (if
      (and
        (integerp nr)
        (integerp r0)
        (integerp rinc)
        (integerp nc)
        (integerp c0)
        (integerp cinc)
        (symbolp storage)
        (arrayp (get storage 'storage_array)))
      (if (or (= nr 0) (= nc 0))
        (dimension-function '(($amatrix simp)) result)
        (dim-$matrix
          (simplifya
            ($genmatrix
              `((lambda) ((mlist) i j) (mfuncall '$get_element ,form i j))
              ($@-function form '$nr)
              ($@-function form '$nc))
            t)
          result))
      (dimension-function form result))))

(defun $amatrixp (a)
  (and (not ($atom a)) (eq ($op a) '$amatrix)))

(defun $nrows (a)
  (if ($amatrixp a)
    ($@-function a '$nr)
    `(($nrows) ,a)))

(defun $ncols (a)
  (if ($amatrixp a)
    ($@-function a '$nc)
    `(($ncols) ,a)))

(defun $amatrix_multiply (m1 m2)
  (let ((nc1 ($ncols m1)) (nr2 ($nrows m2)))
    (cond
      ((and (integerp nc1) (integerp nr2))
       (if (eq ($ncols m1) ($nrows m2))
         (let
           ((aa (gensym))
            (nn ($ncols m1))
            (nr ($nrows m1))
            (a1 (get ($@-function m1 '$storage) 'storage_array))
            (a1-inc ($@-function m1 '$cinc))
            (nc ($ncols m2))
            (a2 (get ($@-function m2 '$storage) 'storage_array))
            (a2-inc ($@-function m2 '$rinc)))
           (setf (get aa 'storage_array) (make-array (* nr nc) :initial-element 0))
           (dotimes (i nr)
             (dotimes (j nc)
               (let
                 ((a1-base (mfuncall '$compute_index0 m1 i 0))
                  (a2-base (mfuncall '$compute_index0 m2 0 j)))
                 (setf
                   (aref (get aa 'storage_array) (+ i (* j nr)))
                   (dot nn a1 a1-base a1-inc a2 a2-base a2-inc)))))
           `(($amatrix) ,nr 0 1 ,nc 0 ,nr ,aa 0))
         (merror "amatrix_multiply: inconformable operands")))
      (t
        `((mnctimes) ,m1 ,m2)))))

(defun dot (n a a0 ainc b b0 binc)
  (let ((ai a0) (bi b0) (sum 0))
    (dotimes (i n)
      (setq
        sum (+ sum (* (aref a ai) (aref b bi)))
        ai (+ ai ainc)
        bi (+ bi binc)))
    sum))

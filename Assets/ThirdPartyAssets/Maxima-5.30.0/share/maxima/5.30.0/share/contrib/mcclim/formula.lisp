;; A simple proof-of-concept CLIM formatter for math formulae.
;; Magic fudge factors abound.

;; FIXME: This is all a big hack.

(defparameter *text-size* 75)

(defgeneric display-formula (stream form))
(defgeneric display-function (stream name))
(defgeneric display-compound-form (form operator operands))
  

(defmethod display-formula (stream form)
  (princ form stream))

(defmethod display-formula (stream (form list))
  (display-compound-form stream (first form) (rest form)))

(defmethod display-function (stream name)
  (princ name stream))

(defmethod display-compound-form (stream operator operands)
  (display-function stream operator)
  (write-char #\( stream)
  (loop for exp in operands and idx from 0 by 1 do
        (progn
          (unless (zerop idx)
            (write-string ", " stream))
          (display-formula stream exp)))
  (write-char #\) stream))

(defun connect-addition-operands (a b stream)
  "Print connecting operator between A and B, and present B (A should already have been printed)"
  (unless (and (listp b)
               (eq (first b) '-)
               (= (length b) 2))
    (write-string "+" stream))
  (display-formula stream b))

(defun print-in-parens (stream fn)
  (write-string "(" stream)
  (funcall fn)
  (write-string ")" stream))

(defmacro with-parens ((stream) &body body)
  `(print-in-parens ,stream (lambda () ,@body)))
  

(defun connect-subtraction-operands (a b stream)
  "Print connecting operator between A and B, and present B (A should already have been printed)"
  (let ((outer (if (and (listp b)
                        (eq (first b) '-)
                        (= (length b) 2))
                   #'print-in-parens
                   #'funcall)))
    (write-string "-" stream)
    (funcall outer (lambda () (display-formula stream b)))))

(defun dopairs (fn list args)
  (when (and list (> (length list) 1))
    (apply fn (first list) (second list) args)
    (dopairs fn (rest list) args)))


   
(defmethod display-compound-form (stream (operator (eql '+)) operands)
  (when operands
    (display-formula stream (first operands))
    (dopairs #'connect-addition-operands operands (list stream))))

(defmethod display-compound-form (stream (operator (eql '-)) operands)
  (cond ((zerop (length operands))
         (error "No arguments to subtraction operator"))
        ((= 1 (length operands))
         (write-string "-" stream)
         (display-formula stream (first operands)))
        (t (display-formula stream (first operands))
           (dopairs #'connect-subtraction-operands operands (list stream)))))

(defun connect-multiplication-operands (a b stream)
  (write-string "*" stream)
  (display-formula stream b))

(defmethod display-compound-form (stream (operator (eql '*)) operands)
  (cond ((zerop (length operands))
         (error "No arguments to multiplication operator"))        
        (t (display-formula stream (first operands))           
           (dopairs #'connect-multiplication-operands operands (list stream)))))

(defun connect-division-operands (a b stream)
  (write-string "/" stream)
  (display-formula stream b))

(defmethod display-compound-form (stream (operator (eql '/)) operands)
  (cond ((zerop (length operands))
         (error "No arguments to division operator"))
        ((= 1 (length operands))
         (write-string "1/" stream)
         (display-formula stream (first operands)))
        (t (display-formula stream (first operands))           
           (dopairs #'connect-division-operands operands (list stream)))))

(defmethod display-compound-form (stream (operator (eql 'expt)) operands)
  (display-formula stream (first operands))
  (write-string "^" stream)
  (display-formula stream (second operands)))

;; Magic CLIM bits

(defmethod display-compound-form ((stream clim:extended-output-stream) (operator (eql '/)) operands)
  (cond ((zerop (length operands))
         (error "No arguments to division operator"))
        ((= 1 (length operands))  ;; FIXME
         (write-string "1/" stream)
         (display-formula stream (first operands)))
        ((= 2 (length operands)) ;; This is the pretty case which we should normalize toward.
         (multiple-value-bind (cx cy)  (clim:stream-cursor-position stream)
           (let* ((dividend-or (clim:with-output-to-output-record (stream)
                                 (display-formula stream (first operands))))
                  (divisor-or (clim:with-output-to-output-record (stream)
                                (display-formula stream (second operands))))
                  (width (* 1.15 (max (clim:bounding-rectangle-width dividend-or)
                                      (clim:bounding-rectangle-width divisor-or))))
                  (sum-height (+ (clim:bounding-rectangle-height dividend-or)
                                 (clim:bounding-rectangle-height divisor-or)))
                  (split 1/4)
                  (thickness-ratio 0.65)
                  (size (* 0.04 sum-height))
                  (thickness (* thickness-ratio size))
                  (y0 (clim:bounding-rectangle-height dividend-or))
                  (y1 (+ y0 (* split size)))
                  (y2 (+ y0 size))
                  (combined-or (clim:with-output-to-output-record (stream)
                                 (setf (clim:output-record-position dividend-or)
                                       (values (/ (- width (clim:bounding-rectangle-width dividend-or)) 2)
                                               0)
                                       (clim:output-record-position divisor-or)
                                       (values (/ (- width (clim:bounding-rectangle-width divisor-or)) 2)
                                               y2))
                                 (clim:stream-add-output-record stream dividend-or)
                                 (clim:draw-line* stream 0 y1 width y1
                                                  :line-thickness thickness
                                                  :line-cap-shape :round)
                                 (clim:stream-add-output-record stream divisor-or))))
             (setf (clim:output-record-position combined-or)
                   (values cx
                           (- cy 3 (clim:bounding-rectangle-height dividend-or)
                              (- (/ (clim:text-style-height (clim:medium-text-style stream) stream) 2))))
                   #+NIL (clim:stream-cursor-position stream) #+NIL
                   (values (+ cx (clim:bounding-rectangle-width combined-or))
                           cy))
             (clim:stream-add-output-record stream combined-or)
             (clim:stream-close-text-output-record stream)
             #+NIL (clim:replay-output-record combined-or stream))))
        (t (display-formula stream (first operands))
           (dopairs #'connect-division-operands operands (list stream)))))

(defun superscript-text-size (size)
  (assert (numberp size))
  (max 10 (round (* 0.6 size))))  ;; FIXME stream

(defmethod display-compound-form ((stream clim:extended-output-stream) (operator (eql 'expt)) operands)
  (unless (= (length operands) 2)
    (error "EXPT requires 2 operands"))
  (let ((base-or (clim:with-new-output-record (stream)                   
                   (display-formula stream (first operands)))))
    (multiple-value-bind (cx cy)  (clim:stream-cursor-position stream)
      (let* ((*text-size* (superscript-text-size (clim:text-style-size (clim:medium-text-style stream))))
             (exponent-or (clim:with-output-to-output-record (stream)
                            (display-formula stream (second operands))))
             (h0 (clim:bounding-rectangle-height exponent-or))
             (h1 (clim:bounding-rectangle-height base-or))
             (dy (max 0.0 (- h0 (* 0.4 h1)))))
        (setf (clim:output-record-position exponent-or)
              (values cx (- cy dy)))
        (clim:stream-add-output-record stream exponent-or)
        (clim:stream-close-text-output-record stream)))))


;; This :around method is where most of the CLIM magic occurs (output is captured into
;; presentations, and some formatting kludgery occurs)
(defmethod display-formula :around ((stream clim:extended-output-stream) form)
  (clim:with-text-size (stream *text-size*)
    (multiple-value-bind (cx cy)  (clim:stream-cursor-position stream)
      (let ((record (clim:with-output-to-output-record (stream) ;; FIXME why are forms seemingly not presented?
                      (clim:with-output-as-presentation (stream form (if (listp form) 'form (clim:presentation-type-of form))) ;; this is suspect..
                        (call-next-method stream form)))))
        (clim:with-bounding-rectangle* (x0 y0 x1 y1) record          
           (setf (clim:output-record-position record) (values (+ x0 1 cx) (+ y0 cy))))
        (clim:stream-add-output-record stream record)
        (clim:stream-close-text-output-record stream)
        (multiple-value-bind (nx ny)  (clim:stream-cursor-position stream)
          (setf (clim:stream-cursor-position stream)
                (values (+ cx 3 (clim:bounding-rectangle-width record)) cy)))
        #+NIL
        (clim:with-bounding-rectangle* (x0 y0 x1 y1) record
           (hef:debugf x0 y0 x1 y1)
           (clim:draw-rectangle* stream x0 y0 x1 y1 :filled nil :ink clim:+blue+))
        (when (clim:stream-drawing-p stream)
          (clim:replay-output-record record stream))))))

(defun foof ()  
  (display-formula *standard-output*
                   '(+ 12.3 (* 2 pi) (- 2) (- 4 5) (/ (* 3 a b) (* 2 x)) (log 2) (fn 2 a b) (expt 2 x))))

(defun bar ()
  (display-formula *standard-output*
                   '(/ 1 (expt 2 (+ 1 (expt 3 (expt x x)))))))

(defun baz ()
  (display-formula *standard-output*
                   '(/ (/ (expt 2 (expt 2 x)) (/ (+ 1 (expt 2 x)) x))
                       (+ (expt 2 (expt 2 x)) (/ (+ 1 (expt 2 x)) x)))))


(with-open-file (out "/home/hefner/maff.ps"
                     :direction :output
                     :if-exists :supersede)
    (CLIM:WITH-OUTPUT-TO-POSTSCRIPT-STREAM (*standard-output* out
                                            :multi-page t
                                            :scale-to-fit nil)
      (clim:with-room-for-graphics (*standard-output* :first-quadrant nil)
        (clim:with-text-family (*standard-output* :serif)
          (dotimes (i 10) (terpri))
          (baz) ))))

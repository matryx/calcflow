;; ezunits: yet another units package for Maxima
;; This program copyright 2008 by Robert Dodier.
;; I release this program under the terms of the
;; GNU General Public License.

;; Process tex(a`b): throw away the backtick, texify a,
;; and texify b with all symbols in b output as mathrm.

(defun tex-ezunits (x l r)
  (setq l (tex (second x) l '("\\;") lop (caar x)))
  (tex (mathrm-ify (third x)) l r (caar x) rop))

;; If a symbol has a texword property, preserve it.
;; Otherwise replace each symbol with a gensym which has a mathrm texword property.

(defun mathrm-ify (e)
  (let
    ((v (reverse (cdr ($listofvars e)))))
    (let
      ((L (mapcar #'(lambda (s)
                      (if (get s 'texword)
                        s
                        (let
                          ((g (gensym)))
                          (putprop
                            g
                            (concatenate
                              'string
                              "\\mathrm{"
                              (maybe-invert-string-case (symbol-name (tex-stripdollar0 s)))
                              "}")
                            'texword)
                          g)))
                  v)))
      ($substitute `((mlist) ,@(mapcar #'(lambda (a b) `((mequal) ,a ,b)) v L)) e))))

(defprop $\` tex-ezunits tex)

(defun $odds (a) (cons '(mlist) (odds (cdr a) 1)))

(defun $evens (a) (cons '(mlist) (odds (cdr a) 0)))


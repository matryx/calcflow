;; generating do loop from lisp
(setq n 10)
(gentran `(((mdo) i 1 nil nil ,n nil
                       ((mdo) j ((mplus) i 1) nil nil ,n nil
                              ((mprogn)
                               ((msetq) ((x) j i) ((x) i j))
                               ((msetq) ((y) j i) ((y) i j)))))) nil)

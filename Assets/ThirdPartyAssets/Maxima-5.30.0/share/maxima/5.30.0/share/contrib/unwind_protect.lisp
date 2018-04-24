(defmspec $unwind_protect (e)
  (setq e (margs e))
  (unwind-protect (meval (first e)) (mapcar #'meval (rest e))))
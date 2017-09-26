(in-package :cl-sloop)
(defmacro sloop (&rest body)
  (warn (intl:gettext "Using deprecated macro 'sloop'. Use 'loop' instead."))
  `(loop ,@body))


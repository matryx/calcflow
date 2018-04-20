;; -*- Lisp -*-

(in-package :maxima)

(mk:defsystem maxima-fft
  :source-pathname (maxima::maxima-load-pathname-directory)
  :binary-pathname (maxima::maxima-objdir "share" "numeric")
  :source-extension "lisp"
  :components
  ((:file "fft-core")))

(mk:oos "maxima-fft" :compile)

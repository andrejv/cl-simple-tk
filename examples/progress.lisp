(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Progress / Scale")
    
    (let ((var (float-var))
          (f (frame :parent root :padding '(10 10 10 10))))

      (pack f :expand t :fill "both")

      (pack (scale :parent f :from 0 :to 50 :step 2 :orient "vertical" :variable var)
            :padx 10 :pady 10 :side "left")
      (pack (scale :parent f :from 0 :to 50 :orient "horizontal" :variable var)
            :padx 10 :pady 10 :side "left")
      (pack (separator :parent f :orient "vertical")
            :fill "y" :side "left")

      (pack (progressbar :parent f :maximum 50 :orient "vertical" :variable var)
            :padx 10 :pady 10 :side "right")
      (pack (progressbar :parent f :maximum 50 :orient "horizontal" :variable var)
            :padx 10 :pady 10 :side "right")
      (pack (separator :parent f :orient "vertical")
            :fill "y" :side "right")

      (pack (entry :parent f :textvariable var)
            :padx 10 :pady 10 :side "top" :expand t)

      (setf (var-value var) 25))))

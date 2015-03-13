(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Checkbuttons")
    (setf (window-geometry root) "200x100+100+100")
    
    (let* ((f (frame :parent root))
           (c-var (boolean-var))
           (c (checkbutton :parent f :text "Option" :variable c-var))
           (l-var (string-var))
           (l (label :parent f :textvariable l-var)))
      
      (pack f :expand t :fill "both")
      (pack (list c l) :padx 2 :pady 2)
      
      (setf (var-value c-var) t)
      (setf (var-value l-var) "On")
      (bind-command c (lambda ()
                        (if (var-value c-var)
                            (setf (var-value l-var) "On")
                            (setf (var-value l-var) "Off")))))))

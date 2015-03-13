(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Buttons")
    (setf (window-geometry root) "200x100+100+100")
    
    (let* ((f (frame :parent root :relief "ridge"))
           (counter 0)
           (s-var (string-var))
           (b (button :parent f :textvariable s-var)))
           
      (pack f :expand t :fill "both")
      
      (pack b :padx 2 :pady 2 :expand t)
            
      (setf (var-value s-var) "Clicks: 0")
      (bind-command b (lambda ()
                        (setf (var-value s-var)
                              (format nil "Clicks: ~a" (incf counter))))))))

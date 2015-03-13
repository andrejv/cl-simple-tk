(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Example 1")
    (setf (window-geometry root) "300x100+100+200")

    (let ((f (frame :parent root)))

      (pack f :expand t :fill "both")
      
      (pack (button :parent f
                    :text "Quit"
                    :command (lambda ()
                               (window-destroy root)))
            :padx 5 :pady 5 :expand t))))

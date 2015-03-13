(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Entry")
    (setf (window-minsize root) '(300 100))

    (let* ((f (frame :parent root))
           (e-var (string-var))
           (e (entry :parent f :textvariable e-var))
           (l-var (string-var)))

      (pack f :expand t :fill "both")

      (pack e :padx 5 :pady 5)
      (pack (label :parent f
                   :textvariable l-var))

      (entry-insert e 0 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
      (entry-icursor e 10)
      (entry-insert e "ins" "1")
      (entry-selection-range e "ins" "end")

      (bind-event e "<Return>"
                  (lambda (ev)
                    (declare (ignore ev))
                    (setf (var-value l-var)
                          (var-value e-var)))))))

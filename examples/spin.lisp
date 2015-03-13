(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Spinbox")
    (setf (window-minsize root) '(300 100))

    (let* ((f (frame :parent root))
           (s-var (float-var))
           (s (spinbox :parent f
                       :from 10
                       :to 100
                       :increment 10
                       :wrap t
                       :width 4
                       :textvariable s-var))
           (l-var (string-var)))

      (pack f :expand t :fill "both")

      (pack s :expand t)
      (pack (label :parent f :textvariable l-var)
            :pady 10)

      (setf (var-value l-var) "Spinbox")
      (setf (var-value s-var) 50)

      (bind-event s "<<Increment>>"
                  (lambda (ev)
                    (declare (ignore ev))
                    (setf (var-value l-var)
                          (format nil "Increment: ~a" (var-value s-var)))))

      (bind-event s "<<Decrement>>"
                  (lambda (ev)
                    (declare (ignore ev))
                    (setf (var-value l-var)
                          (format nil "Decrement: ~a" (var-value s-var))))))))

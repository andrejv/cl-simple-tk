(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Variables")
    (setf (window-geometry root) "300x200+200+200")
    (let ((f (frame :parent root))
          (cb-var (string-var))
          (l-var (string-var)))
      (pack f :expand t :fill "both")
      (pack (checkbutton :parent f
                         :text "Checkbox"
                         :variable cb-var
                         :onvalue "enabled"
                         :offvalue "disabled"
                         :command (lambda ()
                                    (setf (var-value l-var)
                                          (format nil "Checkbox is ~a." (var-value cb-var)))))
            :padx 2 :pady 2)
      (pack (label :parent f
                   :textvariable l-var)
            :padx 2 :pady 2)
      (setf (var-value cb-var) "enabled")
      (setf (var-value l-var) "Checkbox is enabled."))))


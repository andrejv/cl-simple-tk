(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (let* ((f (frame :parent root))
           (box (listbox :parent f :selectmode "multiple"))
           (var (string-var)))

      (listbox-insert box 0 (list "One" "Two" "Three" "Four" "Five"))

      (pack f :expand t :fill "both")
      (pack box :expand t :fill "both" :padx 2 :pady 2)
      (pack (label :parent f :textvariable var) :padx 2 :pady 2 :fill "x")
      (setf (var-value var) "Selection:")

      (bind-event box "<<ListboxSelect>>"
                  (lambda (ev)
                    (declare (ignore ev))
                    (setf (var-value var)
                          (format nil "Selection: ~{~a~^,~}" (listbox-curselection box))))))))

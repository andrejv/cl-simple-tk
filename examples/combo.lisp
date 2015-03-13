(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Combobox")
    (setf (window-minsize root) '(300 200))

    (let* ((f (frame :parent root))
           (c (combobox :parent f :width 20))
           (fonts (font-names)))
      
      (pack f :expand t :fill "both")

      (pack c :expand t)
      (window-configure c :values fonts)
      (bind-event c "<<ComboboxSelected>>"
                  (lambda (ev)
                    (declare (ignore ev))
                    (message-box "A selection happened"
                                 :title "Selection"
                                 :detail (entry-get c)))))))

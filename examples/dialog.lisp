(defpackage :dialog
  (:use :cl :tk)
  (:export :main))

(in-package :dialog)

(defun get-font (root)
  (let* ((top (toplevel :parent root))
         (f (frame :parent top :padding '(10 10 10 10)))
         (list (listbox :parent f))
         (entries (font-families))
         result)

    (pack f :expand t :fill "both")
    
    (setf (window-title top) "Choose Font")
    (window-transient top :parent root)
    (grab-set top)
    
    (listbox-insert list "end" entries)
    (pack list :padx 2 :pady 2 :expand t :fill "both")

    (pack (button :text "OK" :parent f
                  :command (lambda ()
                             (let ((i (car (listbox-curselection list))))
                               (when i
                                 (setf result (nth i entries)))
                               (window-destroy top)))))

    (window-wait top)
    result))

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Dialog Example")
    (let* ((t-var (string-var))
           (f (frame :parent root :padding '(10 10 10 10)))
           (l (label :parent f :textvariable t-var)))
      (pack f :fill "both" :expand t)
      (pack l :fill "both" :expand t :padx 5 :pady 3)
      (pack (button :parent f :text "Choose Font"
                    :command (lambda ()
                               (let ((result (get-font root)))
                                 (when result
                                   (setf (var-value t-var) result)
                                   (window-configure l :font
                                                     (font-format result :size 25 :bold t))))))))))


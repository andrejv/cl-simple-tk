(defpackage :dialog
  (:use :cl :tk)
  (:export :main))

(in-package :dialog)

(defun get-font (root)
  (let* ((top (toplevel :parent root))
         (f (frame :parent top :padding '(2 2 2 2)))
         (f-scroll (frame :parent f))
         (list (listbox :parent f-scroll))
         (s (scrollbar :parent f-scroll :orient "vertical"))
         (entries (font-families))
         result)

    (setf (window-title top) "Choose Font")
    (window-transient top :parent root)
    (grab-set top)
    (destructuring-bind ((w h) x y)
        (window-geometry root)
      (declare (ignore w h))
      (setf (window-geometry top) (list () (+ x 30) (+ y 30))))

    (pack f :expand t :fill "both")

    (pack f-scroll :expand t :fill "both" :padx 2 :pady 2)    
    (grid list :sticky "news" :row 0 :column 0)
    (grid s :sticky "ns" :column 1 :row 0)
    (scrollbar-connect list s)
    (grid-rowconfigure f-scroll 0 :weight 1)
    (grid-columnconfigure f-scroll 0 :weight 1)

    (listbox-insert list "end" entries)
    
    (pack (button :text "OK" :parent f
                  :command (lambda ()
                             (let ((i (car (listbox-curselection list))))
                               (when i
                                 (setf result (nth i entries)))
                               (window-destroy top))))
          :pady 2)

    (window-wait top)
    result))

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Dialog Example")
    (let ((x (window-screenwidth root))
          (y (window-screenheight root)))
      (setf (window-geometry root) (list (list 400 200)
                                         (- (truncate x 2) 200)
                                         (- (truncate y 2) 100))))
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


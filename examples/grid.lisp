(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Grid")

    (let ((onevar (boolean-var))
          (twovar (boolean-var))
          (threevar (boolean-var))
          (f (frame :parent root :padding '(5 5 5 5))))
      
      (setf (var-value onevar) t
            (var-value twovar) nil
            (var-value threevar) t)
      
      (pack f :expand t :fill "both")
      
      (grid (frame :parent f :relief "sunken" :width 300 :height 200)
            :column 0 :row 0 :sticky "nsew"
            :columnspan 3 :rowspan 2)
      (grid (label :parent f :text "Name")
            :column 3 :row 0 :columnspan 2 :sticky "w")
      (grid (entry :parent f)
            :column 3 :row 1 :sticky "new"
            :columnspan 2)
      (grid (checkbutton :text "One" :parent f :variable onevar)
            :column 0 :row 3)
      (grid (checkbutton :text "Two" :parent f :variable twovar)
            :column 1 :row 3)
      (grid (checkbutton :text "Three" :parent f :variable threevar)
            :column 2 :row 3)
      (grid (button :text "OK" :parent f)
            :column 3 :row 3)
      (grid (button :text "Cancel" :parent f)
            :column 4 :row 3)

      (grid-rowconfigure f 1 :weight 1)
      (grid-columnconfigure f 0 :weight 3)
      (grid-columnconfigure f 1 :weight 3)
      (grid-columnconfigure f 2 :weight 3)
      (grid-columnconfigure f 3 :weight 1)
      (grid-columnconfigure f 4 :weight 1)

      (dolist (s (grid-slaves f))
        (grid-configure s :padx 2 :pady 2)))))
  

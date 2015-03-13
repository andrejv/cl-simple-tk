(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Scrollbars")
    (setf (window-minsize root) '(300 300))

    (let ((vertical (scrollbar :parent root :orient "vertical"))
          (horizontal (scrollbar :parent root :orient "horizontal"))
          (canvas (canvas :parent root :scrollregion '(-300 -300 300 300))))
      
      (grid canvas :row 0 :column 0 :sticky "nwes")
      (grid vertical :row 0 :column 1 :sticky "ns")
      (grid horizontal :row 1 :column 0 :sticky "ew")
      (grid (tk::sizegrip :parent root) :column 1 :row 1 :sticky "news")
      (grid-columnconfigure root 0 :weight 1)
      (grid-rowconfigure root 0 :weight 1)

      (scrollbar-connect canvas horizontal)
      (scrollbar-connect canvas vertical)

      (canvas-create-poylgon canvas '(-200 -200 0 200 200 -200)
                             :fill "blue"
                             :outline "red"
                             :width 10))))

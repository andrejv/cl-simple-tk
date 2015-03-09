(defpackage :converter
  (:use :cl :simple-tk)
  (:export :main))

(in-package :converter)

(defun calculate (feet meters)
  (setf (var-value meters)
        (* 0.3048 (var-value feet))))

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Feet to Meters")
    (let* ((mainframe (frame :parent root :padding '(3 3 12 12)))
           (feet (float-var))
           (meters (float-var)))

      (grid mainframe :row 0 :column 0 :sticky "nsew")
      (grid-columnconfigure root 0 :weight 1)
      (grid-rowconfigure root 0 :weight 1)

      (grid (entry :parent mainframe :width 7 :textvariable feet)
            :column 2 :row 1 :sticky "we")
      (grid (label :parent mainframe :textvariable meters)
            :column 2 :row 2 :sticky "we")
      (grid (button :parent mainframe :text "Calculate"
                    :command (lambda () (calculate feet meters)))
            :column 3 :row 3)

      (grid (label :parent mainframe :text "feet")
            :column 3 :row 1 :sticky "w")
      (grid (label :parent mainframe :text "is equivalent to")
            :column 1 :row 2 :sticky "e")
      (grid (label :parent mainframe :text "meters")
            :column 3 :row 2 :sticky "w")

      (dolist (w (window-children mainframe))
        (grid-configure w :padx 5 :pady 5))
      (grid-columnconfigure mainframe 2 :weight 1)

      (bind-event root "<Return>" (lambda (e)
                                    (declare (ignore e))
                                    (calculate feet meters))))))

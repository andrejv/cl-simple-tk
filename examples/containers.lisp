(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Containers")
    (setf (window-minsize root) '(400 400))

    (let* ((nb (notebook :parent root))
           (pw1 (panedwindow :parent nb :orient "vertical"))
           (pw2 (panedwindow :parent nb :orient "horizontal")))

      (notebook-add nb pw1 :text "Vertical")
      (notebook-add nb pw2 :text "Horizontal")

      (panedwindow-add pw1 (label :parent pw1 :text "1" :relief "sunken" :anchor "center")
                       :weight 1)
      (panedwindow-add pw1 (label :parent pw1 :text "2" :relief "sunken" :anchor "center")
                       :weight 3)

      (panedwindow-add pw2 (label :parent pw2 :text "A" :relief "sunken" :anchor "center")
                       :weight 3)
      (panedwindow-add pw2 (label :parent pw2 :text "B" :relief "sunken" :anchor "center")
                       :weight 1)

      (pack nb :expand t :fill "both"))))

(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Canvas")
    (setf (window-minsize root) '(600 600))

    (let ((canvas (canvas :parent root :scrollregion '(-300 -300 1300 1300)))
          (h-scroll (scrollbar :parent root :orient "horizontal"))
          (v-scroll (scrollbar :parent root :orient "vertical"))
          (mouse-down))

      (scrollbar-connect canvas h-scroll)
      (scrollbar-connect canvas v-scroll)
      
      (grid canvas :row 0 :column 0 :sticky "news")
      (grid v-scroll :row 0 :column 1 :sticky "ns")
      (grid h-scroll :row 1 :column 0 :sticky "ew")
      (grid (sizegrip :parent root) :row 1 :column 1 :sticky "news")

      (grid-columnconfigure root 0 :weight 1)
      (grid-rowconfigure root 0 :weight 1)

      (canvas-create-oval canvas '(0 0 50 50)
                          :fill "blue" :outline "red" :width 10 :tags "one")
      (canvas-create-rectangle canvas '(100 100 170 180)
                               :fill "blue" :outline "red" :width 10 :tags "one")

      (canvas-create-line canvas '(500 500 300 500 300 300)
                          :fill "yellow" :width 10 :tags "two")

      (canvas-bind canvas "one" "<Enter>"
                   (lambda (ev)
                     (declare (ignore ev))
                     (canvas-itemconfig canvas "one" :outline "yellow")))
      (canvas-bind canvas "one" "<Leave>"
                   (lambda (ev)
                     (declare (ignore ev))
                     (canvas-itemconfig canvas "one" :outline "red")))

      (bind-event canvas "<Button-1>"
                  (lambda (ev)
                    (destructuring-bind (x y)
                        (canvas-scrolled-coords canvas (event-mouse-position ev))
                      (setf mouse-down t)
                      (canvas-scan-mark canvas x y))))
      (bind-event canvas "<Motion>"
                  (lambda (ev)
                    (when mouse-down
                      (destructuring-bind (x y)
                          (canvas-scrolled-coords canvas (event-mouse-position ev))
                        (canvas-scan-dragto canvas x y :gain 1)))))
      (bind-event canvas"<ButtonRelease-1>"
                  (lambda (ev)
                    (declare (ignore ev))
                    (setf mouse-down nil))))))

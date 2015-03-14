(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Menus")
    (setf (window-geometry root) "400x400+300+300")

    (let* ((f (labelframe :parent root :text "Menu selection:"))
           (mbar (menu :parent root))
           (file (menu :parent mbar))
           (sub (menu :parent file))
           (var (string-var)))

      (pack f :expand t :fill "both")
      
      (menu-add-command sub "One"
                        (lambda ()
                          (setf (var-value var) "one")))
      (menu-add-command sub "Two"
                        (lambda ()
                          (setf (var-value var) "two")))
      
      (menu-add-cascade file "Submenu" sub)
      (menu-add-command file "Open"
                        (lambda ()
                          (setf (var-value var) "open"))
                        :accelerator "CTRL-O")
      (menu-add-separator file)
      (menu-add-command file "Quit"
                        (lambda ()
                          (window-destroy root)))

      (menu-add-cascade mbar "File" file)

      (pack (label :textvariable var :parent f)
            :expand t)

      (bind-event f "<Button-1>"
                  (lambda (ev)
                    (let ((m (menu :parent f)))
                      (menu-add-command m "One" (lambda ()
                                                  (setf (var-value var) "one")))
                      (menu-add-command m "Two" (lambda ()
                                                  (setf (var-value var) "two")))
                      (destructuring-bind (x y)
                          (event-mouse-position ev)
                        (menu-popup m (+ (window-rootx f) x) (+ (window-rooty f) y))))))

      (window-configure root :menu mbar))))

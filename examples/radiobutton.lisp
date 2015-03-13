(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Radiobuttons")
    (setf (window-minsize root) '(200 200))
    
    (let* ((f (frame :parent root
                     :padding '(5 5 5 5)))
           (r-var (string-var))
           (l-var (string-var))
           (l (label :parent f :textvariable l-var)))
      
      (pack f :expand t :fill "both")
      
      (dolist (rb '(("Small" 1)
                    ("Medium" 2)
                    ("Big" 3)
                    ("Bigger" 4)))
        (pack (radiobutton :parent f
                           :text (car rb)
                           :variable r-var
                           :value (cadr rb))
              :padx 2 :pady 2 :fill "x"))

      (setf (var-value r-var) 2)
      (setf (var-value l-var) "Value 2")
            
      (trace-var r-var
                 (lambda (ev)
                   (declare (ignore ev))
                   (setf (var-value l-var)
                         (format nil "Value ~d" (var-value r-var)))))

      (pack l :padx 5 :pady 5))))

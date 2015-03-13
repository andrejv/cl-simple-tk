(defpackage :tk-user
  (:use :cl :tk)
  (:export :main))

(in-package :tk-user)

(defun main ()
  (with-tk-root (root)
    (setf (window-title root) "Treeview")

    (let* ((f (frame :parent root))
           (var (string-var))
           (tw (treeview :parent f :columns (list "col1" "col2"))))

      (pack f :expand t :fill "both")

      (setf (treeview-heading-text tw "#0") "Tree")
      (setf (treeview-column-width tw "col1") 150)
      (setf (treeview-column-width tw "col2") 150)
      (setf (treeview-heading-text tw "col1") "Column A")
      (setf (treeview-heading-text tw "col2") "Column B")
      (setf (treeview-column-anchor tw "col1") "center")
      (setf (treeview-column-anchor tw "col2") "center")

      (let ((id (treeview-insert tw "" 0 :text "Line 1" :values '("A" "B"))))
        (treeview-insert tw id 0 :text "Line 2" :values '("A1" "B1"))
        (treeview-insert tw id "end" :text "Line 3" :values '("A2")))
      (let ((id (treeview-insert tw "" 1 :text "Line 4")))
        (treeview-insert tw id 0 :values '("C" "D"))
        (treeview-selection-set tw id))

      (pack tw :expand t :fill "both")

      (pack (label :parent f :textvariable var)
            :padx 5 :pady 5 :fill "x")

      (treeview-heading-command tw "col1"
                                (lambda ()
                                  (message-box "Heading clicked" :detail "col1")))
      
      (bind-event tw "<<TreeviewSelect>>"
                  (lambda (ev)
                    (declare (ignore ev))
                    (let ((id (car (treeview-selection tw))))
                      (setf (var-value var)
                            (format nil "Selection: ID: ~a, TEXT: ~a, VALUES: (~{~a~,})"
                                    id
                                    (treeview-item-text tw id)
                                    (treeview-item-values tw id)))))))))

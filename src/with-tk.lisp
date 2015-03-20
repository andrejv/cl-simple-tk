;;;
;;; Copyright (c) 2015 Andrej Vodopivec <andrej.vodopivec@gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;; THE SOFTWARE.
;;;

(in-package :simple-tk)

(defmacro with-tk ((root title) &body body)
  `(let* ((*event-table* (make-hash-table :test #'equal))
          (*window-table* (make-hash-table :test #'equal))
          (*current-interp* (init-session))
          (,root (toplevel :tk-name "." :tk-define nil)))
     (setf (gethash "." *window-table*) ,root)
     (send-command (format nil "wm title . ~s" ,title))
     ,@body
     (send-command "catch { console hide }")
     (tk-main-loop)
     (tcl-delete-interp *current-interp*)))

#+(and sbcl darwin)
(defmacro with-tk-root ((root &key (title "TK")) &body body)
  "Opens the TCL/TK session.
 
All code should be called from within WITH-TK-ROOT."
  `(sb-int:with-float-traps-masked (:underflow :overflow :inexact :invalid :divide-by-zero)
     (with-tk (,root ,title)
       ,@body)))

#+(and cmucl darwin)
(defmacro with-tk-root ((root &key (title "TK")) &body body)
  "Opens the TCL/TK session.

All code should be called from within WITH-TK-ROOT."
  `(ext:with-float-traps-masked (:underflow :overflow :inexact :invalid :divide-by-zero)
     (with-tk (,root ,title)
       ,@body)))

#+(and ccl darwin)
(defmacro with-tk-root ((root &key (title "TK")) &body body)
  "Opens the TCL/TK session.

All code should be called from within WITH-TK-ROOT."
  `(flet ((thunk ()
            (with-tk (,root ,title)
              (tk-mac-quit (lambda (&rest args)
                             (declare (ignore args))
                             (ccl:quit)))
              ,@body)))
     (let ((s (ccl:make-semaphore)))
       (ccl:process-interrupt ccl::*initial-process* (lambda () (thunk) (ccl:signal-semaphore s)))
       (ccl:wait-on-semaphore s))))

#-(and darwin (or sbcl cmucl ccl))
(defmacro with-tk-root ((root &key (title "TK")) &body body)
  "Opens the TCL/TK session.

All code should be called from within WITH-TK-ROOT."
  `(with-tk (,root ,title)
     ,@body))
